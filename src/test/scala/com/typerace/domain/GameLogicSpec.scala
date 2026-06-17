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
      GameEvent.Tick(nowMs = 61_000L, deltaMs = GameConfig.RoundDurationMs)
    )

    assert(afterRound.round == 2)
    assert(afterRound.timeRemainingMs == GameConfig.RoundDurationMs)
  }

  test("WordGenerator es determinista para la misma semilla") {
    val w1 = WordGenerator.generateWord(seed, 0)
    val w2 = WordGenerator.generateWord(seed, 0)
    assert(w1 == w2)
    assert(w1.length >= 3)
  }
