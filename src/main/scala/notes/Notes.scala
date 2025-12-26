package org.aranadedoros.chordal
package notes

import chords.*
import intervals.*

/* core */
object Pitch:
  val chromatic =
    Vector("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  def isValid(pitch: String): Boolean = chromatic.contains(pitch.toUpperCase)

object Diatonic:
  val letters = Vector("C", "D", "E", "F", "G", "A", "B")

  def shift(letter: String, steps: Int): String =
    val idx = letters.indexOf(letter)
    letters((idx + steps % 7 + 7) % 7)

def naturalPitch(letter: String): String =
  if !Diatonic.letters.contains(letter) then
    throw new IllegalArgumentException(s"Invalid natural pitch: $letter")
  letter

def transposePitch(base: String, semitones: Int): String =
  val idx = Pitch.chromatic.indexOf(base)
  if idx < 0 then
    throw new IllegalArgumentException(s"Invalid pitch: $base")

  Pitch.chromatic(
    (idx + semitones + 12) % 12
  )

def accidentalBetween(natural: String, target: String): Accidental =
  val nIdx = Pitch.chromatic.indexOf(natural)
  val tIdx = Pitch.chromatic.indexOf(target)

  val diff = (tIdx - nIdx + 12) % 12

  diff match
    case 0  => Natural
    case 1  => Sharp
    case 11 => Flat
    case _ =>
      throw new IllegalStateException(
        s"Cannot spell $target from natural $natural"
      )

def pitchFrom(letter: String, accidental: Accidental): String =
  val natural = naturalPitch(letter)
  accidental match
    case Natural     => natural
    case Sharp       => transposePitch(natural, 1)
    case Flat        => transposePitch(natural, -1)
    case DoubleSharp => transposePitch(natural, 2)
    case DoubleFlat  => transposePitch(natural, -2)

sealed trait Accidental
case object Natural     extends Accidental
case object Sharp       extends Accidental
case object Flat        extends Accidental
case object DoubleSharp extends Accidental
case object DoubleFlat  extends Accidental

case class Spelling(letter: String, accidental: Accidental):
  override def toString: String =
    accidental match
      case Natural     => letter
      case Sharp       => s"$letter#"
      case Flat        => s"${letter}b"
      case DoubleSharp => s"$letter##"
      case DoubleFlat  => s"${letter}bb"

object Spelling:
  def from(base: Note, interval: Interval): Spelling =
    val newLetter =
      Diatonic.shift(base.spelling.letter, interval.diatonicSteps)

    val targetPitch =
      transposePitch(base.pitch, interval.semitones)

    val natural =
      naturalPitch(newLetter)

    val accidental =
      accidentalBetween(natural, targetPitch)

    Spelling(newLetter, accidental)

  def from(base: Spelling, interval: Interval): Spelling =
    val newLetter =
      Diatonic.shift(base.letter, interval.diatonicSteps)

    val natural =
      naturalPitch(newLetter)

    val targetPitch =
      transposePitch(
        base.letter + base.accidental.toString,
        interval.semitones
      )

    val accidental =
      accidentalBetween(natural, targetPitch)

    Spelling(newLetter, accidental)

  def resolve(letter: String, pitch: String): Spelling =
    val natural =
      naturalPitch(letter)

    val accidental =
      accidentalBetween(natural, pitch)

    Spelling(letter, accidental)

case class Note(pitch: String, spelling: Spelling):
  require(Pitch.isValid(pitch), "Use a valid pitch")

  def toPitch: String           = spelling.letter
  override def toString: String = spelling.toString

  def transposeChromatically(interval: Interval, backwards: Boolean = false): Note =
    val semitoneToMove =
      if backwards then -interval.semitones else interval.semitones
    val newPitch =
      transposePitch(pitch, semitoneToMove)
    Note(newPitch, spelling)

  def transposeDiatonically(interval: Interval, backwards: Boolean = false): Note =
    val diatonicSteps =
      if backwards then -interval.diatonicSteps else interval.diatonicSteps

    val semitones =
      if backwards then -interval.semitones else interval.semitones

    val newLetter =
      Diatonic.shift(spelling.letter, diatonicSteps)

    val expectedPitch =
      transposePitch(pitch, semitones)

    val newSpelling =
      Spelling.resolve(newLetter, expectedPitch)

    Note(expectedPitch, newSpelling)

object Note:

  def apply(name: String): Note =
    val (letter, accidental) = parse(name)
    val pitch                = pitchFrom(letter, accidental)
    Note(pitch, Spelling(letter, accidental))

  private def parse(s: String): (String, Accidental) =
    s.toList match
      case letter :: '#' :: Nil => (letter.toString, Sharp)
      case letter :: 'b' :: Nil => (letter.toString, Flat)
      case letter :: Nil        => (letter.toString, Natural)
      case _ =>
        throw new IllegalArgumentException(s"Invalid note: $s")

extension (s: String)
  def note: Note = Note(s)
  def major: Chord =
    val root = Note(s)
    Triad(root, MajorChord)
  def minor: Chord =
    val root = Note(s)
    Triad(root, MinorChord)
  def power: PowerChord = PowerChord(Note(s))
  def sharpen: Note =
    s.note.transposeChromatically(ChromaticUp)
  def flatten: Note =
    s.note.transposeChromatically(ChromaticDown)

extension (n: Note)
  def sharpenChromatic: Note =
    n.transposeChromatically(ChromaticUp)
  def flattenChromatic: Note =
    n.transposeChromatically(ChromaticDown)
  def sharpenDiatonic: Note =
    n
      .transposeDiatonically(SecondMinorInterval)
  def major: Triad =
    Triad(n, MajorChord)
  def minor: Triad =
    Triad(n, MinorChord)
