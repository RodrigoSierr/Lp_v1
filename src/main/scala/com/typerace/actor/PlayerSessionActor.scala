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

  /** Conexión rechazada antes de registrar al jugador. */
  final case class Rejected(reason: String) extends Command

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

  def rejected(reason: String, wsOut: pekko.actor.ActorRef): Behavior[Command] =
    Behaviors.setup { _ =>
      wsOut ! TextMessage(s"""{"type":"error","message":"$reason"}""")
      Behaviors.receiveMessage {
        case StreamCompleted => Behaviors.stopped
        case _ => Behaviors.same
      }
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
              case GameJson.InboundMessage("input", Some(key)) if key == " " =>
                gameManager ! GameManagerActor.UsePowerUp(playerId)

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

          case Rejected(reason) =>
            wsOut ! TextMessage(s"""{"type":"error","message":"$reason"}""")
            Behaviors.stopped
      }
      .receiveSignal {
        case (_, org.apache.pekko.actor.typed.PostStop) =>
          gameManager ! GameManagerActor.Unregister(playerId)
          Behaviors.same
      }
