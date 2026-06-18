package com.typerace.domain

/** Funciones puras de transición de estado del juego. */
object GameLogic:

  def initialState(seed: Long): GameState =
    GameState(
      round = 1,
      timeRemainingMs = GameConfig.roundDurationMs(1),
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
        val players = state.players - id
        // Si todos se van y está finalizado, resetear el state
        if players.isEmpty && state.isFinished then initialState(state.seed)
        else state.copy(players = players)

      case GameEvent.ResetLobby =>
        // Reinicia la partida conservando a los jugadores conectados (reseteando sus stats)
        val freshPlayers = state.players.map { case (id, p) =>
          id -> PlayerState(
            id = p.id,
            displayName = p.displayName,
            score = 0,
            streak = 0,
            currentTarget = "",
            targetProgress = 0,
            lockedUntil = None,
            targetSequence = 0,
            powerUp = None,
            activeDebuffs = List.empty,
            typingHistory = List.empty
          )
        }
        GameState(
          round = 1,
          timeRemainingMs = GameConfig.roundDurationMs(1),
          players = freshPlayers,
          isRunning = false,
          isFinished = false,
          seed = state.seed + 1, // nueva seed para nueva partida
          winnerId = None
        )

      case GameEvent.StartGame =>
        if state.players.isEmpty || state.isRunning || state.isFinished then state
        else
          state.copy(
            isRunning = true,
            timeRemainingMs = GameConfig.roundDurationMs(state.round),
            players = state.players.map { case (id, p) => id -> assignNewTarget(p, state.round, state.seed, 0L) }
          )

      case GameEvent.UsePowerUp(playerId, atMs) =>
        if !state.isRunning || state.isFinished then state
        else
          state.players.get(playerId) match
            case Some(p) if p.powerUp.isDefined =>
              val power = p.powerUp.get
              val updatedSelf = p.copy(powerUp = None)
              val updatedOthers = state.players.collect {
                case (id, other) if id != playerId =>
                  id -> applyDebuff(other, power, atMs, state)
              }
              state.copy(players = state.players ++ updatedOthers + (playerId -> updatedSelf))
            case _ => state

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
      targetSequence = 0,
      powerUp = None,
      activeDebuffs = List.empty,
      typingHistory = List.empty
    )
    assignNewTarget(player, state.round, state.seed, 0L)

  private def adaptiveLevel(history: List[Long], atMs: Long): Int =
    if history.size < 5 || atMs == 0L then 1
    else
      val recent = history.filter(_ > atMs - 10000L) // strokes in last 10 seconds
      if recent.size > 40 then 3 // Experto (> 4 pulsaciones por seg)
      else if recent.size > 20 then 2 // Intermedio (> 2 pulsaciones por seg)
      else 1

  private def assignNewTarget(player: PlayerState, round: Int, seed: Long, atMs: Long): PlayerState =
    val level = adaptiveLevel(player.typingHistory, atMs)
    val target = TargetGenerator.generate(round, seed, player.targetSequence, level)
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
        if inputMatches(player, key, roundKind) then onCorrectHit(player, state, atMs)
        else onWrongHit(player, atMs)

      state.copy(players = state.players.updated(player.id, updatedPlayer))

  private def inputMatches(player: PlayerState, key: String, roundKind: RoundKind): Boolean =
    roundKind match
      case RoundKind.Arrows | RoundKind.Alphanumeric =>
        player.currentTarget.equalsIgnoreCase(key)

      case RoundKind.Words =>
        val expected = player.currentTarget.lift(player.targetProgress).map(_.toString.toLowerCase).getOrElse("")
        key.length == 1 && key.toLowerCase == expected

  private def onCorrectHit(player: PlayerState, state: GameState, atMs: Long): PlayerState =
    val newStreak = player.streak + 1
    val points = GameConfig.BasePoints * newStreak
    val roundKind = GameConfig.roundKind(state.round)
    
    val newHistory = (atMs :: player.typingHistory).take(50) // Guardamos hasta 50
    
    // Dar poder aleatorio cada 5 aciertos de racha (si no tiene uno)
    val newPowerUp = if newStreak > 0 && newStreak % 5 == 0 && player.powerUp.isEmpty then
      val s = state.seed + player.score + atMs
      if s % 2 == 0 then Some("Freeze") else Some("Scramble")
    else player.powerUp

    val p = player.copy(score = player.score + points, streak = newStreak, typingHistory = newHistory, powerUp = newPowerUp)

    roundKind match
      case RoundKind.Arrows | RoundKind.Alphanumeric =>
        assignNewTarget(p, state.round, state.seed, atMs)

      case RoundKind.Words =>
        val nextProgress = player.targetProgress + 1
        val wordComplete = nextProgress >= player.currentTarget.length

        if wordComplete then assignNewTarget(p, state.round, state.seed, atMs)
        else p.copy(targetProgress = nextProgress)

  private def onWrongHit(player: PlayerState, atMs: Long): PlayerState =
    val newHistory = (atMs :: player.typingHistory).take(50)
    player.copy(
      streak = 0,
      targetProgress = 0,
      lockedUntil = Some(atMs + GameConfig.LockDurationMs),
      typingHistory = newHistory
    )

  private def applyDebuff(player: PlayerState, debuff: String, atMs: Long, state: GameState): PlayerState =
    debuff match
      case "Freeze" => player.copy(lockedUntil = Some(atMs + 2000L)) // 2s Freeze
      case "Scramble" => assignNewTarget(player.copy(targetProgress = 0, streak = 0), state.round, state.seed, atMs)
      case _ => player

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
      timeRemainingMs = GameConfig.roundDurationMs(nextRound),
      players = state.players.map { case (id, p) =>
        id -> assignNewTarget(p.copy(streak = 0, lockedUntil = None), nextRound, state.seed, 0L)
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
      lockedUntil = player.lockedUntil,
      powerUp = player.powerUp,
      activeDebuffs = player.activeDebuffs
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
    lockedUntil: Option[Long],
    powerUp: Option[String],
    activeDebuffs: List[String]
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
