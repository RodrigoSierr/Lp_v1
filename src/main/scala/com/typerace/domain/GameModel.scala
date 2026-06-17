package com.typerace.domain

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
  final case object StartGame                                          extends GameEvent

/** Estado de un jugador en la carrera. */
final case class PlayerState(
    id: String,
    displayName: String,
    score: Int,
    streak: Int,
    currentTarget: String,
    targetProgress: Int,
    lockedUntil: Option[Long],
    targetSequence: Int
)

/** Estado global del juego. */
final case class GameState(
    round: Int,
    timeRemainingMs: Long,
    players: Map[String, PlayerState],
    isRunning: Boolean,
    isFinished: Boolean,
    seed: Long,
    winnerId: Option[String]
)

object GameConfig:
  val TotalRounds: Int           = 3
  val RoundDurationMs: Long      = 60_000L
  val BasePoints: Int            = 10
  val LockDurationMs: Long       = 1_000L
  val MaxPlayers: Int            = 8

  def roundKind(round: Int): RoundKind = RoundKind.fromRound(round)
