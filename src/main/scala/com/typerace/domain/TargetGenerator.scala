package com.typerace.domain

/** Generación pura de objetivos según la ronda activa. */
object TargetGenerator:

  private val Arrows: Vector[String] = Vector("UP", "DOWN", "LEFT", "RIGHT")

  private val Alphanumeric: Vector[String] =
    ('a' to 'z').map(_.toString).toVector ++ ('0' to '9').map(_.toString).toVector

  def generate(round: Int, seed: Long, sequence: Int): String =
    RoundKind.fromRound(round) match
      case RoundKind.Arrows       => arrowTarget(seed, sequence)
      case RoundKind.Alphanumeric => alphanumericTarget(seed, sequence)
      case RoundKind.Words        => WordGenerator.generateWord(seed, sequence)

  private def arrowTarget(seed: Long, sequence: Int): String =
    val idx = WordGenerator.pickIndex(seed, sequence * 17 + 1, Arrows.size)
    Arrows(idx)

  private def alphanumericTarget(seed: Long, sequence: Int): String =
    val idx = WordGenerator.pickIndex(seed, sequence * 31 + 2, Alphanumeric.size)
    Alphanumeric(idx)
