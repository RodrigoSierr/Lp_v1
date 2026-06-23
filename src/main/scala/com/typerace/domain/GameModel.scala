package com.typerace.domain

/** Modo de juego seleccionable desde el lobby. */
enum GameMode:
  case TimeBased
  case LivesBased

/** Tipos de ronda y reglas de objetivo. */
enum RoundKind:
  case Arrows
  case Alphanumeric
  case Words

object RoundKind:
  def fromRound(round: Int): RoundKind = round match
    case 1 => RoundKind.Arrows
    case 2 => RoundKind.Alphanumeric
    case 3 => RoundKind.Words
    case _ => RoundKind.Words

/** Eventos inmutables que alimentan la máquina de estado pura. */
sealed trait GameEvent

object GameEvent:
  final case class PlayerJoined(playerId: String, displayName: String) extends GameEvent
  final case class PlayerLeft(playerId: String)                        extends GameEvent
  final case class PlayerInput(playerId: String, key: String, atMs: Long) extends GameEvent
  final case class Tick(nowMs: Long, deltaMs: Long)                    extends GameEvent
  final case class UsePowerUp(playerId: String, power: String, atMs: Long) extends GameEvent
  final case object StartGame                                          extends GameEvent
  final case object ResetLobby                                         extends GameEvent

/** Estado de un jugador en la carrera. */
final case class PlayerState(
    id: String,
    displayName: String,
    score: Int,
    streak: Int,
    currentTarget: String,
    targetProgress: Int,
    lockedUntil: Option[Long],
    targetSequence: Int,
    powerUps: Set[String],       // Inventario de poderes (puede tener varios a la vez)
    activeDebuffs: List[String],
    typingHistory: List[Long],
    lives: Option[Int]           // None en modo tiempo; Some(n) en modo vidas
)

/** Estado global del juego. */
final case class GameState(
    round: Int,
    timeRemainingMs: Long,
    players: Map[String, PlayerState],
    isRunning: Boolean,
    isFinished: Boolean,
    seed: Long,
    winnerId: Option[String],
    gameMode: GameMode
)

object GameConfig:
  val TotalRounds: Int           = 3
  val BasePoints: Int            = 10
  val LockDurationMs: Long       = 1_000L
  val MaxPlayers: Int            = 8
  val InitialLives: Int          = 5

  def roundDurationMs(round: Int): Long = round match
    case 1 => 15_000L
    case 2 => 20_000L
    case 3 => 30_000L
    case _ => 30_000L

  def roundKind(round: Int): RoundKind = RoundKind.fromRound(round)
