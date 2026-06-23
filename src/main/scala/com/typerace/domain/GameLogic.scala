package com.typerace.domain

/** Funciones puras de transición de estado del juego. */
object GameLogic:

  def initialState(seed: Long, gameMode: GameMode = GameMode.TimeBased): GameState =
    GameState(
      round = 1,
      timeRemainingMs = GameConfig.roundDurationMs(1),
      players = Map.empty,
      isRunning = false,
      isFinished = false,
      seed = seed,
      winnerId = None,
      gameMode = gameMode
    )

  def updateState(state: GameState, event: GameEvent): GameState =
    event match
      case GameEvent.PlayerJoined(id, name) =>
        if state.isRunning || state.isFinished || state.players.size >= GameConfig.MaxPlayers then state
        else state.copy(players = state.players + (id -> freshPlayer(id, name, state)))

      case GameEvent.PlayerLeft(id) =>
        val players = state.players - id
        // Si todos se van y está finalizado, resetear el state
        if players.isEmpty && state.isFinished then initialState(state.seed, state.gameMode)
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
            powerUps = Set.empty,
            activeDebuffs = List.empty,
            typingHistory = List.empty,
            lives = livesForMode(state.gameMode)
          )
        }
        GameState(
          round = 1,
          timeRemainingMs = GameConfig.roundDurationMs(1),
          players = freshPlayers,
          isRunning = false,
          isFinished = false,
          seed = state.seed + 1, // nueva seed para nueva partida
          winnerId = None,
          gameMode = state.gameMode
        )

      case GameEvent.StartGame =>
        if state.players.isEmpty || state.isRunning || state.isFinished then state
        else
          state.copy(
            isRunning = true,
            timeRemainingMs = GameConfig.roundDurationMs(state.round),
            players = state.players.map { case (id, p) => id -> assignNewTarget(p, state.round, state.seed, 0L) }
          )

      case GameEvent.UsePowerUp(playerId, power, atMs) =>
        if !state.isRunning || state.isFinished then state
        else
          state.players.get(playerId) match
            case Some(p) if p.powerUps.contains(power) =>
              if isEliminated(p) then state  // Jugadores eliminados no pueden usar poderes
              else power match
                case "Cleanse" =>
                  // Auto-aplicado: elimina el propio Freeze sin afectar las vidas
                  val updatedSelf = p.copy(
                    powerUps = p.powerUps - "Cleanse",
                    lockedUntil = None
                  )
                  state.copy(players = state.players + (playerId -> updatedSelf))

                case "Hack" if state.round < 3 =>
                  // Hack solo funciona en ronda 3
                  state

                case offensivePower =>
                  val updatedSelf = p.copy(powerUps = p.powerUps - offensivePower)
                  val updatedOthers = state.players.collect {
                    case (id, other) if id != playerId =>
                      id -> applyDebuff(other, offensivePower, atMs, state)
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

  private def livesForMode(gameMode: GameMode): Option[Int] =
    gameMode match
      case GameMode.LivesBased => Some(GameConfig.InitialLives)
      case GameMode.TimeBased  => None

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
      powerUps = Set.empty,
      activeDebuffs = List.empty,
      typingHistory = List.empty,
      lives = livesForMode(state.gameMode)
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

  private def isEliminated(player: PlayerState): Boolean =
    player.lives.exists(_ <= 0)

  // ---------------------------------------------------------------------------
  // Entrada de teclado
  // ---------------------------------------------------------------------------

  private def applyInput(
      state: GameState,
      player: PlayerState,
      key: String,
      atMs: Long
  ): GameState =
    if isLocked(player, atMs) || isEliminated(player) then state
    else
      val roundKind = GameConfig.roundKind(state.round)
      val updatedPlayer =
        if inputMatches(player, key, roundKind) then onCorrectHit(player, state, atMs)
        else onWrongHit(player, atMs, state.gameMode)

      val newPlayers = state.players.updated(player.id, updatedPlayer)
      val newState   = state.copy(players = newPlayers)

      // En modo vidas, comprobar si el fallo dejó a alguien sin vidas
      if state.gameMode == GameMode.LivesBased then checkLivesVictory(newState)
      else newState

  private def inputMatches(player: PlayerState, key: String, roundKind: RoundKind): Boolean =
    roundKind match
      case RoundKind.Arrows | RoundKind.Alphanumeric =>
        player.currentTarget.equalsIgnoreCase(key)

      case RoundKind.Words =>
        val expected = player.currentTarget.lift(player.targetProgress).map(_.toString.toLowerCase).getOrElse("")
        key.length == 1 && key.toLowerCase == expected

  private def onCorrectHit(player: PlayerState, state: GameState, atMs: Long): PlayerState =
    val newStreak = player.streak + 1
    val points    = GameConfig.BasePoints * newStreak
    val roundKind = GameConfig.roundKind(state.round)

    val newHistory = (atMs :: player.typingHistory).take(50) // Guardamos hasta 50

    // Asignar poderes basado en la racha
    val newPowerUps = assignPowerUps(player.powerUps, newStreak, state, atMs)

    val p = player.copy(
      score        = player.score + points,
      streak       = newStreak,
      typingHistory = newHistory,
      powerUps     = newPowerUps
    )

    roundKind match
      case RoundKind.Arrows | RoundKind.Alphanumeric =>
        assignNewTarget(p, state.round, state.seed, atMs)

      case RoundKind.Words =>
        val nextProgress = player.targetProgress + 1
        val wordComplete = nextProgress >= player.currentTarget.length

        if wordComplete then assignNewTarget(p, state.round, state.seed, atMs)
        else p.copy(targetProgress = nextProgress)

  /** Asigna poderes según la racha actual.
   *
   *  - Cada 3 aciertos: Cleanse (auto-defensa contra Freeze).
   *  - Cada 5 aciertos: poder ofensivo (Freeze, Scramble, o Hack solo en ronda 3).
   *    Si el jugador ya tiene el poder, se intenta el siguiente disponible.
   */
  private def assignPowerUps(current: Set[String], streak: Int, state: GameState, atMs: Long): Set[String] =
    var powers = current

    // Cleanse cada 3 aciertos seguidos
    if streak % 3 == 0 && !powers.contains("Cleanse") then
      powers = powers + "Cleanse"

    // Poder ofensivo cada 5 aciertos seguidos
    if streak % 5 == 0 then
      val rng = state.seed + atMs + streak
      // En ronda 3 se puede ganar Hack
      if state.round == 3 && !powers.contains("Hack") then
        powers = powers + "Hack"
      // Luego Freeze o Scramble dependiendo de la pseudo-aleatoriedad
      else if rng % 2 == 0 && !powers.contains("Freeze") then
        powers = powers + "Freeze"
      else if !powers.contains("Scramble") then
        powers = powers + "Scramble"
      else if !powers.contains("Freeze") then
        powers = powers + "Freeze"

    powers

  private def onWrongHit(player: PlayerState, atMs: Long, gameMode: GameMode): PlayerState =
    val newHistory = (atMs :: player.typingHistory).take(50)

    val newLives = gameMode match
      case GameMode.LivesBased => player.lives.map(l => math.max(0, l - 1))
      case GameMode.TimeBased  => player.lives

    // Si se quedó sin vidas → bloqueo permanente (eliminado)
    val lockUntil = newLives match
      case Some(0) => Some(Long.MaxValue)   // Centinela de eliminación permanente
      case _       => Some(atMs + GameConfig.LockDurationMs)

    player.copy(
      streak        = 0,
      targetProgress = 0,
      lockedUntil   = lockUntil,
      typingHistory  = newHistory,
      lives         = newLives
    )

  private def applyDebuff(player: PlayerState, debuff: String, atMs: Long, state: GameState): PlayerState =
    debuff match
      case "Freeze"   => player.copy(lockedUntil = Some(atMs + 2000L)) // 2s Freeze
      case "Scramble" => assignNewTarget(player.copy(targetProgress = 0, streak = 0), state.round, state.seed, atMs)
      case "Hack"     => player.copy(targetProgress = 0, streak = 0)   // Reinicia palabra y racha (no los puntos)
      case _          => player

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
  // Reloj, vidas y rondas
  // ---------------------------------------------------------------------------

  private def tickRunning(state: GameState, nowMs: Long, deltaMs: Long): GameState =
    val remaining = math.max(0L, state.timeRemainingMs - deltaMs)
    if remaining > 0L then state.copy(timeRemainingMs = remaining)
    else advanceRoundOrFinish(state)

  /** En modo vidas: detecta si queda 0 o 1 jugador activo y finaliza la partida. */
  private def checkLivesVictory(state: GameState): GameState =
    val activePlayers = state.players.values.filter(p => !isEliminated(p)).toList
    activePlayers.size match
      case 0                            => finishGame(state)   // Todos eliminados
      case 1 if state.players.size > 1  => finishGame(state)   // Solo queda uno
      case _                            => state

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
      id            = player.id,
      displayName   = player.displayName,
      score         = player.score,
      streak        = player.streak,
      currentTarget = player.currentTarget,
      targetProgress = player.targetProgress,
      isLocked      = isLocked(player, nowMs),
      lockedUntil   = player.lockedUntil,
      powerUps      = player.powerUps.toList.sorted,
      activeDebuffs = player.activeDebuffs,
      lives         = player.lives,
      isEliminated  = isEliminated(player)
    )

  def toClientState(state: GameState, nowMs: Long): ClientGameState =
    ClientGameState(
      round           = state.round,
      roundKind       = GameConfig.roundKind(state.round).toString,
      timeRemainingMs = state.timeRemainingMs,
      isRunning       = state.isRunning,
      isFinished      = state.isFinished,
      winnerId        = state.winnerId,
      players         = state.players.values.map(p => playerView(p, nowMs)).toList.sortBy(_.id),
      gameMode        = state.gameMode.toString
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
    powerUps: List[String],
    activeDebuffs: List[String],
    lives: Option[Int],
    isEliminated: Boolean
)

final case class ClientGameState(
    round: Int,
    roundKind: String,
    timeRemainingMs: Long,
    isRunning: Boolean,
    isFinished: Boolean,
    winnerId: Option[String],
    players: List[PlayerView],
    gameMode: String
)
