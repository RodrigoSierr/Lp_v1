package com.typerace.domain

import org.scalatest.funsuite.AnyFunSuite

class GameLogicSpec extends AnyFunSuite:

  private val seed = 42_424L

  test("StartGame asigna objetivos a todos los jugadores") {
    val joined = List(
      GameEvent.PlayerJoined("p1", "Ana"),
      GameEvent.PlayerJoined("p2", "Luis")
    ).foldLeft(GameLogic.initialState(seed))((s, e) => GameLogic.updateState(s, e))

    val running = GameLogic.updateState(joined, GameEvent.StartGame)

    assert(running.isRunning)
    assert(running.players.values.forall(_.currentTarget.nonEmpty))
    assert(running.players("p1").currentTarget == "UP" || running.players("p1").currentTarget == "DOWN" ||
      running.players("p1").currentTarget == "LEFT" || running.players("p1").currentTarget == "RIGHT")
  }

  test("Acierto incrementa racha y puntos; fallo bloquea y resetea racha") {
    val base = GameLogic.updateState(
      GameLogic.updateState(
        GameLogic.initialState(seed),
        GameEvent.PlayerJoined("p1", "Ana")
      ),
      GameEvent.StartGame
    )

    val target = base.players("p1").currentTarget
    val hit = GameLogic.updateState(base, GameEvent.PlayerInput("p1", target, 1_000L))
    assert(hit.players("p1").streak == 1)
    assert(hit.players("p1").score == GameConfig.BasePoints)

    val miss = GameLogic.updateState(hit, GameEvent.PlayerInput("p1", "WRONG", 2_000L))
    assert(miss.players("p1").streak == 0)
    assert(miss.players("p1").lockedUntil.contains(3_000L))
    assert(GameLogic.playerView(miss.players("p1"), 2_500L).isLocked)
  }

  test("Tick agota el tiempo y avanza de ronda") {
    val running = GameLogic.updateState(
      GameLogic.updateState(
        GameLogic.initialState(seed),
        GameEvent.PlayerJoined("p1", "Ana")
      ),
      GameEvent.StartGame
    )

    val afterRound = GameLogic.updateState(
      running,
      GameEvent.Tick(nowMs = 61_000L, deltaMs = GameConfig.roundDurationMs(1))
    )

    assert(afterRound.round == 2)
    assert(afterRound.timeRemainingMs == GameConfig.roundDurationMs(2))
  }

  test("Al salir el ultimo jugador de una partida finalizada se reinicia el lobby") {
    val running = GameLogic.updateState(
      GameLogic.updateState(
        GameLogic.initialState(seed),
        GameEvent.PlayerJoined("p1", "Ana")
      ),
      GameEvent.StartGame
    )
    val finished = GameLogic.updateState(
      running.copy(round = GameConfig.TotalRounds),
      GameEvent.Tick(nowMs = 61_000L, deltaMs = GameConfig.roundDurationMs(GameConfig.TotalRounds))
    )

    val reset = GameLogic.updateState(finished, GameEvent.PlayerLeft("p1"))

    assert(!reset.isFinished)
    assert(!reset.isRunning)
    assert(reset.players.isEmpty)
    assert(reset.round == 1)
  }

  test("WordGenerator es determinista para la misma semilla") {
    val w1 = WordGenerator.generateWord(seed, 0)
    val w2 = WordGenerator.generateWord(seed, 0)
    assert(w1 == w2)
    assert(w1.length >= 3)
  }

  test("Modo vidas: fallo reduce vida; llegar a 0 elimina al jugador") {
    val base = GameLogic.updateState(
      GameLogic.updateState(
        GameLogic.initialState(seed, GameMode.LivesBased),
        GameEvent.PlayerJoined("p1", "Ana")
      ),
      GameEvent.StartGame
    )

    assert(base.players("p1").lives.contains(GameConfig.InitialLives))

    // Fallar una vez → pierde 1 vida
    val miss1 = GameLogic.updateState(base, GameEvent.PlayerInput("p1", "WRONG", 1_000L))
    assert(miss1.players("p1").lives.contains(GameConfig.InitialLives - 1))

    // Fallar hasta quedar sin vidas → eliminado
    val allMisses = (2 to GameConfig.InitialLives).foldLeft(miss1) { (s, i) =>
      // Esperar a que pase el lock antes del siguiente fallo
      val unlocked = s.copy(players = s.players.updated("p1",
        s.players("p1").copy(lockedUntil = Some(0L))))
      GameLogic.updateState(unlocked, GameEvent.PlayerInput("p1", "WRONG", i * 2_000L))
    }
    assert(allMisses.players("p1").lives.contains(0))
    // Long.MaxValue - 1 es menor que el centinela de eliminación Long.MaxValue
    assert(GameLogic.playerView(allMisses.players("p1"), Long.MaxValue - 1).isLocked)
    assert(GameLogic.playerView(allMisses.players("p1"), Long.MaxValue - 1).isEliminated)
  }

  test("Cleanse elimina el Freeze propio") {
    val base = GameLogic.updateState(
      GameLogic.updateState(
        GameLogic.initialState(seed),
        GameEvent.PlayerJoined("p1", "Ana")
      ),
      GameEvent.StartGame
    )

    // Inyectamos Cleanse y Freeze manualmente
    val frozen = base.copy(players = base.players.updated("p1",
      base.players("p1").copy(
        powerUps   = Set("Cleanse"),
        lockedUntil = Some(99_999_999L)
      )
    ))

    val cleansed = GameLogic.updateState(frozen, GameEvent.UsePowerUp("p1", "Cleanse", 1_000L))
    assert(cleansed.players("p1").lockedUntil.isEmpty)
    assert(!cleansed.players("p1").powerUps.contains("Cleanse"))
  }

  test("Hack solo funciona en ronda 3 y reinicia racha y progreso del rival") {
    val base2Players = List(
      GameEvent.PlayerJoined("p1", "Ana"),
      GameEvent.PlayerJoined("p2", "Luis")
    ).foldLeft(GameLogic.initialState(seed))((s, e) => GameLogic.updateState(s, e))
    val running = GameLogic.updateState(base2Players, GameEvent.StartGame)

    // Ronda 2 → Hack no debe funcionar
    val inR2 = running.copy(round = 2, players = running.players.updated("p1",
      running.players("p1").copy(powerUps = Set("Hack"))))
    val afterHackR2 = GameLogic.updateState(inR2, GameEvent.UsePowerUp("p1", "Hack", 1_000L))
    assert(afterHackR2.players("p1").powerUps.contains("Hack")) // No se consumió

    // Ronda 3 → Hack sí debe funcionar
    val inR3 = running.copy(round = 3, players = running.players
      .updated("p1", running.players("p1").copy(powerUps = Set("Hack")))
      .updated("p2", running.players("p2").copy(streak = 5, targetProgress = 2))
    )
    val afterHackR3 = GameLogic.updateState(inR3, GameEvent.UsePowerUp("p1", "Hack", 2_000L))
    assert(!afterHackR3.players("p1").powerUps.contains("Hack")) // Se consumió
    assert(afterHackR3.players("p2").streak == 0)
    assert(afterHackR3.players("p2").targetProgress == 0)
  }
