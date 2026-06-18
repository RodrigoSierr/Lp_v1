package com.typerace.actor

import com.typerace.domain.{GameConfig, GameEvent, GameLogic, GameState, GameJson}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}

import scala.concurrent.duration.*

/** Actor raíz del juego: posee el GameState, spawna sesiones y coordina ticks. */
object GameManagerActor:

  sealed trait Command

  /** El WebSocket handler pide crear una sesión; la respuesta va a `replyTo`. */
  final case class CreateSession(
      playerId: String,
      displayName: String,
      wsOut: org.apache.pekko.actor.ActorRef,
      replyTo: ActorRef[ActorRef[PlayerSessionActor.Command]]
  ) extends Command

  final case class Unregister(playerId: String) extends Command

  final case class PlayerKeyInput(playerId: String, key: String) extends Command
  final case class UsePowerUp(playerId: String) extends Command

  final case object StartGame extends Command

  private final case object Tick extends Command
  private final case object ResetLobby extends Command

  private val ResetDelay: FiniteDuration = 10.seconds

  /** Estado interno del manager (inmutable). */
  private final case class ManagerState(
      gameState: GameState,
      sessions: Map[String, ActorRef[PlayerSessionActor.Command]],
      lastTickMs: Long
  )

  private val TickInterval: FiniteDuration = 20.millis // ~50 Hz

  def apply(seed: Long): Behavior[Command] =
    Behaviors.setup { context =>
      Behaviors.withTimers { timers =>
        timers.startTimerAtFixedRate(Tick, TickInterval)
        val now = System.currentTimeMillis()
        running(
          context,
          ManagerState(
            gameState  = GameLogic.initialState(seed),
            sessions   = Map.empty,
            lastTickMs = now
          )
        )
      }
    }

  private def running(context: ActorContext[Command], state: ManagerState): Behavior[Command] =
    Behaviors
      .receive[Command] { (ctx, msg) =>
        msg match

          // ── Crear sesión (spawneamos desde dentro del actor) ──────────────
          case CreateSession(playerId, displayName, wsOut, replyTo) =>
            joinRejectionReason(state.gameState) match
              case Some(reason) =>
                val sessionRef = ctx.spawn(
                  PlayerSessionActor.rejected(reason, wsOut),
                  s"rejected-${playerId.take(8)}"
                )
                replyTo ! sessionRef
                ctx.log.warn("Rechazando {}: {}", playerId, reason)
                Behaviors.same

              case None =>
                val sessionRef = ctx.spawn(
                  PlayerSessionActor(playerId, displayName, ctx.self, wsOut),
                  s"session-${playerId.take(8)}"
                )
                ctx.watch(sessionRef)
                replyTo ! sessionRef

                val joined = GameLogic.updateState(
                  state.gameState,
                  GameEvent.PlayerJoined(playerId, displayName)
                )
                val next = state.copy(
                  gameState = joined,
                  sessions  = state.sessions + (playerId -> sessionRef)
                )
                broadcast(ctx, next)
                running(ctx, next)

          // ── Desregistrar jugador ──────────────────────────────────────────
          case Unregister(playerId) =>
            val left = GameLogic.updateState(state.gameState, GameEvent.PlayerLeft(playerId))
            val next = state.copy(
              gameState = left,
              sessions  = state.sessions - playerId
            )
            broadcast(ctx, next)
            running(ctx, next)

          // ── Input de teclado ──────────────────────────────────────────────
          case PlayerKeyInput(playerId, key) =>
            val now     = System.currentTimeMillis()
            val updated = GameLogic.updateState(
              state.gameState,
              GameEvent.PlayerInput(playerId, key, now)
            )
            if updated != state.gameState then
              val next = state.copy(gameState = updated)
              broadcast(ctx, next)
              running(ctx, next)
            else Behaviors.same

          case UsePowerUp(playerId) =>
            val now     = System.currentTimeMillis()
            val updated = GameLogic.updateState(
              state.gameState,
              GameEvent.UsePowerUp(playerId, now)
            )
            if updated != state.gameState then
              val next = state.copy(gameState = updated)
              broadcast(ctx, next)
              running(ctx, next)
            else Behaviors.same

          // ── Inicio de partida ─────────────────────────────────────────────
          case StartGame =>
            val started = GameLogic.updateState(state.gameState, GameEvent.StartGame)
            if started != state.gameState then
              val next = state.copy(gameState = started, lastTickMs = System.currentTimeMillis())
              broadcast(ctx, next)
              running(ctx, next)
            else Behaviors.same

          // ── Tick del reloj ────────────────────────────────────────────────
          case Tick =>
            val now   = System.currentTimeMillis()
            val delta = math.max(1L, now - state.lastTickMs)
            if state.gameState.isRunning then
              val ticked = GameLogic.updateState(
                state.gameState,
                GameEvent.Tick(now, delta)
              )
              // Si acaba de terminar la partida, programar reinicio en 10s
              if !state.gameState.isFinished && ticked.isFinished then
                ctx.scheduleOnce(ResetDelay, ctx.self, ResetLobby)
                ctx.log.info("Partida finalizada. Reinicio del lobby en 10 segundos.")
              val next = state.copy(gameState = ticked, lastTickMs = now)
              broadcast(ctx, next)
              running(ctx, next)
            else
              running(ctx, state.copy(lastTickMs = now))

          // ── Reinicio del lobby ────────────────────────────────────────────
          case ResetLobby =>
            ctx.log.info("Reiniciando lobby. Los jugadores conectados pueden volver a jugar.")
            val reset = GameLogic.updateState(state.gameState, GameEvent.ResetLobby)
            val next  = state.copy(gameState = reset)
            broadcast(ctx, next)
            running(ctx, next)
      }
      .receiveSignal {
        case (_, org.apache.pekko.actor.typed.Terminated(ref)) =>
          val playerId = state.sessions.collectFirst {
            case (id, session) if session == ref => id
          }
          playerId match
            case Some(id) =>
              val left = GameLogic.updateState(state.gameState, GameEvent.PlayerLeft(id))
              val next = state.copy(gameState = left, sessions = state.sessions - id)
              broadcast(context, next)
              running(context, next)
            case None =>
              Behaviors.same
      }

  private def joinRejectionReason(gameState: GameState): Option[String] =
    if gameState.isRunning then Some("Partida en curso")
    else if gameState.players.size >= GameConfig.MaxPlayers then Some("Lobby lleno")
    else None

  private def broadcast(context: ActorContext[Command], state: ManagerState): Unit =
    val now = System.currentTimeMillis()
    state.sessions.foreach { case (playerId, session) =>
      val payload = GameJson.encodeForPlayer(state.gameState, now, playerId)
      session ! PlayerSessionActor.StateUpdate(payload)
    }
