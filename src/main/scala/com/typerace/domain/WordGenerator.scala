package com.typerace.domain

/** Generador puro de palabras pseudo-aleatorias a partir de una semilla. */
object WordGenerator:

  private val Onsets: Vector[String] = Vector(
    "b", "c", "d", "f", "g", "h", "j", "k", "l", "m",
    "n", "p", "r", "s", "t", "v", "z", "br", "cr", "dr",
    "fr", "gr", "pl", "pr", "tr", "cl", "fl", "gl"
  )

  private val Nuclei: Vector[String] = Vector(
    "a", "e", "i", "o", "u", "ai", "ea", "io", "ou", "ia", "ei", "au"
  )

  private val Codas: Vector[String] = Vector(
    "", "", "n", "r", "s", "l", "m", "t", "x", "d", "z", "nt", "st", "nd"
  )

  /** PRNG determinista lineal congruencial (sin efectos secundarios). */
  def nextSeed(seed: Long, salt: Int): Long =
    ((seed * 1_103_515_245L + salt.toLong * 12_345L + 12_345L) & 0x7fffffffL)

  def pickIndex(seed: Long, salt: Int, size: Int): Int =
    if size <= 0 then 0
    else
      val raw = nextSeed(seed, salt) % size
      // & 0x7fffffffL garantiza positivo, pero por seguridad usamos math.abs
      math.abs(raw.toInt) % size

  /** Construye una sílaba combinando onset + núcleo + coda. */
  def syllable(seed: Long, index: Int): String =
    val s0     = nextSeed(seed, index * 3 + 7)
    val onset  = Onsets(pickIndex(s0, 0, Onsets.size))
    val nucleus = Nuclei(pickIndex(s0, 1, Nuclei.size))
    val coda   = Codas(pickIndex(s0, 2, Codas.size))
    s"$onset$nucleus$coda"

  /** Genera una palabra legible uniendo entre 2 y 4 sílabas (más si el nivel es mayor). */
  def generateWord(seed: Long, wordIndex: Int, level: Int = 1): String =
    val sizeSeed      = nextSeed(seed, wordIndex * 97 + 11)
    val baseSyllables = if level >= 3 then 4 else if level == 2 then 3 else 2
    val syllableCount = baseSyllables + pickIndex(sizeSeed, 0, 2)
    val raw = (0 until syllableCount)
      .map(i => syllable(seed, wordIndex * 100 + i))
      .mkString

    normalizeWord(raw)

  private def normalizeWord(raw: String): String =
    val letters = raw.filter(_.isLetter).toLowerCase
    val padded  =
      if letters.length >= 3 then letters
      else (letters + "ora").take(3)
    capitalizeWord(padded)

  private def capitalizeWord(word: String): String =
    if word.isEmpty then word else word.head.toUpper + word.tail
