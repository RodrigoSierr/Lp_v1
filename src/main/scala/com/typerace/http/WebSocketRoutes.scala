package com.typerace.http

import com.typerace.actor.{GameManagerActor, PlayerSessionActor}
import org.apache.pekko.actor.typed.{ActorRef, ActorSystem}
import org.apache.pekko.actor.typed.scaladsl.AskPattern.*
import org.apache.pekko.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.{Flow, Sink, Source}
import org.apache.pekko.util.Timeout

import java.util.UUID
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object WebSocketRoutes:

  def apply(
      gameManager: ActorRef[GameManagerActor.Command]
  )(using system: ActorSystem[?]): Route =
    path("ws") {
      parameter("name".?) { nameParam =>
        val displayName = nameParam.filter(_.trim.nonEmpty).map(_.trim).getOrElse("Jugador")
        handleWebSocketMessages(playerFlow(gameManager, displayName))
      }
    }

  private def playerFlow(
      gameManager: ActorRef[GameManagerActor.Command],
      displayName: String
  )(using system: ActorSystem[?]): Flow[Message, Message, Any] =

    given ExecutionContext = system.executionContext
    given Timeout          = Timeout(5.seconds)

    val playerId = UUID.randomUUID().toString

    // ── Fuente de salida (actor clásico que el GameManager puede matar) ──
    val (wsOut: org.apache.pekko.actor.ActorRef, wsSource) =
      Source
        .actorRef[Message](
          completionMatcher = PartialFunction.empty,
          failureMatcher    = PartialFunction.empty,
          bufferSize        = 256,
          overflowStrategy  = OverflowStrategy.dropHead
        )
        .preMaterialize()

    // ── Pedir al GameManager que spawne la sesión (ask pattern) ──────────
    // La respuesta llega cuando el actor ya procesó CreateSession.
    val sessionFuture: Future[ActorRef[PlayerSessionActor.Command]] =
      gameManager.ask(replyTo =>
        GameManagerActor.CreateSession(playerId, displayName, wsOut, replyTo)
      )

    // ── Sink de entrada (mensajes del browser → actor de sesión) ─────────
    val incoming: Sink[Message, Any] = Sink.foreach[Message] {
      case TextMessage.Strict(text) =>
        sessionFuture.foreach(_ ! PlayerSessionActor.FromWebSocket(text))

      case TextMessage.Streamed(stream) =>
        stream.runFold("")(_ + _).onComplete {
          case Success(text) => sessionFuture.foreach(_ ! PlayerSessionActor.FromWebSocket(text))
          case Failure(_)    => sessionFuture.foreach(_ ! PlayerSessionActor.StreamCompleted)
        }

      case _: BinaryMessage => ()
    }

    // ── Flow WebSocket: sink de entrada + source de salida ───────────────
    Flow.fromSinkAndSource(
      incoming,
      wsSource.watchTermination() { (_, done) =>
        done.onComplete(_ => sessionFuture.foreach(_ ! PlayerSessionActor.StreamCompleted))
      }
    )
