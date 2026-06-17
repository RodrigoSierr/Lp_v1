package com.typerace.http

import com.typerace.actor.GameManagerActor
import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

import scala.concurrent.Future
import scala.util.{Failure, Success}

object HttpServer:

  def start(
      gameManager: org.apache.pekko.actor.typed.ActorRef[GameManagerActor.Command],
      host: String,
      port: Int
  )(using system: ActorSystem[?]): Future[Http.ServerBinding] =

    import system.executionContext

    val routes: Route =
      concat(
        pathEndOrSingleSlash {
          getFromResource("web/index.html")
        },
        path("index.html") {
          getFromResource("web/index.html")
        },
        WebSocketRoutes(gameManager)
      )

    val bindingFuture = Http()
      .newServerAt(host, port)
      .bind(routes)

    bindingFuture.onComplete {
      case Success(b) =>
        system.log.info(
          "✅ Servidor TypeRace en http://{}:{}/",
          b.localAddress.getHostString,
          b.localAddress.getPort
        )
      case Failure(cause) =>
        system.log.error("No se pudo iniciar el servidor HTTP: {}", cause.getMessage)
    }

    bindingFuture
