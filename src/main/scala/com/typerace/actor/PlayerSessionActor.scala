package com.typerace.actor

import com.typerace.domain.GameJson
import io.circe.parser.decode
import org.apache.pekko
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, ActorContext}
import org.apache.pekko.http.scaladsl.model.ws.{Message, TextMessage}

/** Actor de borde: traduce WebSocket ↔ mensajes del GameManager. */
object PlayerSessionActor:

  sealed trait Command

  /** Texto JSON recibido del cliente. */
  final case class FromWebSocket(text: String) extends Command

  /** Estado serializado enviado por el GameManager. */
  final case class StateUpdate(payload: String) extends Command

  /** El flujo WebSocket se cerró. */
  final case object StreamCompleted extends Command

  /** Lobby lleno u otra razón de rechazo. */
  final case object Rejected extends Command

  def apply(
      playerId: String,
      displayName: String,
      gameManager: ActorRef[GameManagerActor.Command],
      wsOut: pekko.actor.ActorRef
  ): Behavior[Command] =
    Behaviors.setup { context =>
      context.log.info("Sesión WebSocket creada: {} ({})", displayName, playerId)
      // No llamamos a gameManager aquí; el GameManager ya nos spawneó
      // y registró al jugador en el mismo mensaje CreateSession.
      active(context, playerId, gameManager, wsOut)
    }

  private def active(
      context: ActorContext[Command],
      playerId: String,
      gameManager: ActorRef[GameManagerActor.Command],
      wsOut: pekko.actor.ActorRef
  ): Behavior[Command] =
    Behaviors
      .receive[Command] { (ctx, msg) =>
        msg match
          case FromWebSocket(text) =>
            decode[GameJson.InboundMessage](text).foreach {
              case GameJson.InboundMessage("input", Some(key)) =>
                gameManager ! GameManagerActor.PlayerKeyInput(playerId, key)

              case GameJson.InboundMessage("start", _) =>
                gameManager ! GameManagerActor.StartGame

              case other =>
                ctx.log.debug("Mensaje WebSocket ignorado: {}", other)
            }
            Behaviors.same

          case StateUpdate(payload) =>
            wsOut ! TextMessage(payload)
            Behaviors.same

          case StreamCompleted =>
            ctx.log.info("WebSocket cerrado para {}", playerId)
            Behaviors.stopped

          case Rejected =>
            wsOut ! TextMessage("""{"type":"error","message":"Lobby lleno"}""")
            Behaviors.stopped
      }
      .receiveSignal {
        case (_, org.apache.pekko.actor.typed.PostStop) =>
          gameManager ! GameManagerActor.Unregister(playerId)
          Behaviors.same
      }
