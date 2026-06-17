package com.typerace

import org.apache.pekko.actor.typed.ActorSystem
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main:
  def main(args: Array[String]): Unit =
    val system = ActorSystem[Nothing](Application.apply(), "typerace-system")
    // Bloqueamos el hilo principal hasta que el ActorSystem se detenga.
    // El sistema corre en threads daemon; sin este bloqueo el JVM terminaría.
    Await.result(system.whenTerminated, Duration.Inf)
