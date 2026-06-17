package com.typerace.domain

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*

/** Codecs Circe para mensajes WebSocket. */
object GameJson:

  given Encoder[PlayerView]      = deriveEncoder
  given Decoder[PlayerView]      = deriveDecoder
  given Encoder[ClientGameState] = deriveEncoder
  given Decoder[ClientGameState] = deriveDecoder

  given Encoder[GameEvent.PlayerInput] = deriveEncoder
  given Decoder[GameEvent.PlayerInput] = deriveDecoder

  // ── Mensajes de entrada desde el browser ──────────────────────────────────
  final case class InboundMessage(tpe: String, key: Option[String] = None)

  object InboundMessage:
    given Decoder[InboundMessage] = Decoder.instance { c =>
      for
        t <- c.downField("type").as[String]
        k <- c.downField("key").as[Option[String]]
      yield InboundMessage(t, k)
    }

  // ── Mensaje de salida hacia el browser ────────────────────────────────────
  final case class OutboundState(selfId: String, state: ClientGameState)

  object OutboundState:
    given Encoder[OutboundState] = Encoder.instance { o =>
      Json.obj(
        "type"   -> Json.fromString("state"),
        "selfId" -> Json.fromString(o.selfId),
        "state"  -> o.state.asJson
      )
    }

  def encodeState(state: ClientGameState): String =
    state.asJson.noSpaces

  def encodeForPlayer(gameState: GameState, nowMs: Long, selfId: String): String =
    OutboundState(
      selfId = selfId,
      state  = GameLogic.toClientState(gameState, nowMs)
    ).asJson.noSpaces
