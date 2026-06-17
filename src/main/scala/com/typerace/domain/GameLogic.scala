package com.typerace.domain

/** Funciones puras de transición de estado del juego. */
object GameLogic:

  def initialState(seed: Long): GameState =
    GameState(
      round = 1,
      timeRemainingMs = GameConfig.RoundDurationMs,
      players = Map.empty,
      isRunning = false,
      isFinished = false,
      seed = seed,
      winnerId = None
    )

  def updateState(state: GameState, event: GameEvent): GameState =
    event match
      case GameEvent.PlayerJoined(id, name) =>
        if state.isRunning || state.isFinished || state.players.size >= GameConfig.MaxPlayers then state
        else state.copy(players = state.players + (id -> freshPlayer(id, name, state)))

      case GameEvent.PlayerLeft(id) =>
        state.copy(players = state.players - id)

      case GameEvent.StartGame =>
        if state.players.isEmpty || state.isRunning || state.isFinished then state
        else
          state.copy(
            isRunning = true,
            timeRemainingMs = GameConfig.RoundDurationMs,
            players = state.players.map { case (id, p) => id -> assignNewTarget(p, state.round, state.seed) }
          )

      case GameEvent.PlayerInput(playerId, key, atMs) =>
        if !state.isRunning || state.isFinished then state
        else
          state.players.get(playerId) match
            case None          => state
            case Some(player)  => applyInput(state, player, normalizeKey(key), atMs)

      case GameEvent.Tick(nowMs, deltaMs) =>
        if !state.isRunning || state.isFinished then state
        else tickRunning(state, nowMs, deltaMs)

  // ---------------------------------------------------------------------------
  // Jugadores
  // ---------------------------------------------------------------------------

  private def freshPlayer(id: String, name: String, state: GameState): PlayerState =
    val player = PlayerState(
      id = id,
      displayName = name,
      score = 0,
      streak = 0,
      currentTarget = "",
      targetProgress = 0,
      lockedUntil = None,
      targetSequence = 0
    )
    assignNewTarget(player, state.round, state.seed)

  private def assignNewTarget(player: PlayerState, round: Int, seed: Long): PlayerState =
    val target = TargetGenerator.generate(round, seed, player.targetSequence)
    player.copy(
      currentTarget = target,
      targetProgress = 0,
      targetSequence = player.targetSequence + 1
    )

  private def isLocked(player: PlayerState, atMs: Long): Boolean =
    player.lockedUntil.exists(_ > atMs)

  // ---------------------------------------------------------------------------
  // Entrada de teclado
  // ---------------------------------------------------------------------------

  private def applyInput(
      state: GameState,
      player: PlayerState,
      key: String,
      atMs: Long
  ): GameState =
    if isLocked(player, atMs) then state
    else
      val roundKind = GameConfig.roundKind(state.round)
      val updatedPlayer =
        if inputMatches(player, key, roundKind) then onCorrectHit(player, state)
        else onWrongHit(player, atMs)

      state.copy(players = state.players.updated(player.id, updatedPlayer))

  private def inputMatches(player: PlayerState, key: String, roundKind: RoundKind): Boolean =
    roundKind match
      case RoundKind.Arrows | RoundKind.Alphanumeric =>
        player.currentTarget.equalsIgnoreCase(key)

      case RoundKind.Words =>
        val expected = player.currentTarget.lift(player.targetProgress).map(_.toLower).getOrElse("")
        key.length == 1 && key.toLowerCase == expected

  private def onCorrectHit(player: PlayerState, state: GameState): PlayerState =
    val newStreak = player.streak + 1
    val points = GameConfig.BasePoints * newStreak
    val roundKind = GameConfig.roundKind(state.round)

    roundKind match
      case RoundKind.Arrows | RoundKind.Alphanumeric =>
        assignNewTarget(
          player.copy(score = player.score + points, streak = newStreak),
          state.round,
          state.seed
        )

      case RoundKind.Words =>
        val nextProgress = player.targetProgress + 1
        val wordComplete = nextProgress >= player.currentTarget.length
        val scored = player.copy(score = player.score + points, streak = newStreak)

        if wordComplete then assignNewTarget(scored, state.round, state.seed)
        else scored.copy(targetProgress = nextProgress)

  private def onWrongHit(player: PlayerState, atMs: Long): PlayerState =
    player.copy(
      streak = 0,
      targetProgress = 0,
      lockedUntil = Some(atMs + GameConfig.LockDurationMs)
    )

  /** Normaliza teclas del navegador a tokens del dominio. */
  def normalizeKey(raw: String): String =
    raw.trim match
      case "ArrowUp"    => "UP"
      case "ArrowDown"  => "DOWN"
      case "ArrowLeft"  => "LEFT"
      case "ArrowRight" => "RIGHT"
      case other if other.length == 1 => other.toLowerCase
      case other        => other.toUpperCase

  // ---------------------------------------------------------------------------
  // Reloj y rondas
  // ---------------------------------------------------------------------------

  private def tickRunning(state: GameState, nowMs: Long, deltaMs: Long): GameState =
    val remaining = math.max(0L, state.timeRemainingMs - deltaMs)
    if remaining > 0L then state.copy(timeRemainingMs = remaining)
    else advanceRoundOrFinish(state)

  private def advanceRoundOrFinish(state: GameState): GameState =
    if state.round >= GameConfig.TotalRounds then finishGame(state)
    else startNextRound(state)

  private def startNextRound(state: GameState): GameState =
    val nextRound = state.round + 1
    state.copy(
      round = nextRound,
      timeRemainingMs = GameConfig.RoundDurationMs,
      players = state.players.map { case (id, p) =>
        id -> assignNewTarget(p.copy(streak = 0, lockedUntil = None), nextRound, state.seed)
      }
    )

  private def finishGame(state: GameState): GameState =
    val winner = state.players.values.toList.sortBy(-_.score).headOption.map(_.id)
    state.copy(
      isRunning = false,
      isFinished = true,
      timeRemainingMs = 0L,
      winnerId = winner
    )

  /** Vista serializable para clientes (incluye flag de bloqueo derivado). */
  def playerView(player: PlayerState, nowMs: Long): PlayerView =
    PlayerView(
      id = player.id,
      displayName = player.displayName,
      score = player.score,
      streak = player.streak,
      currentTarget = player.currentTarget,
      targetProgress = player.targetProgress,
      isLocked = isLocked(player, nowMs),
      lockedUntil = player.lockedUntil
    )

  def toClientState(state: GameState, nowMs: Long): ClientGameState =
    ClientGameState(
      round = state.round,
      roundKind = GameConfig.roundKind(state.round).toString,
      timeRemainingMs = state.timeRemainingMs,
      isRunning = state.isRunning,
      isFinished = state.isFinished,
      winnerId = state.winnerId,
      players = state.players.values.map(p => playerView(p, nowMs)).toList.sortBy(_.id)
    )

/** DTO inmutable orientado al cliente WebSocket. */
final case class PlayerView(
    id: String,
    displayName: String,
    score: Int,
    streak: Int,
    currentTarget: String,
    targetProgress: Int,
    isLocked: Boolean,
    lockedUntil: Option[Long]
)

final case class ClientGameState(
    round: Int,
    roundKind: String,
    timeRemainingMs: Long,
    isRunning: Boolean,
    isFinished: Boolean,
    winnerId: Option[String],
    players: List[PlayerView]
)
