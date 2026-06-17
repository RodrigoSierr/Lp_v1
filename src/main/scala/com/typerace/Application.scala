package com.typerace

import com.typerace.actor.GameManagerActor
import com.typerace.http.HttpServer
import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.{Behaviors, ActorContext}

object Application:

  def apply(): Behavior[Nothing] =
    Behaviors.setup[Nothing] { context =>
      boot(context)
      Behaviors.ignore
    }

  private def boot(context: ActorContext[Nothing]): Unit =
    val seed        = System.currentTimeMillis()
    val gameManager = context.spawn(GameManagerActor(seed), "game-manager")

    val host = sys.env.getOrElse("HOST", "0.0.0.0")
    val port = sys.env.getOrElse("PORT", "8080").toInt

    HttpServer.start(gameManager, host, port)(using context.system)
