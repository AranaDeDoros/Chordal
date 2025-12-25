package org.aranadedoros.chordal
package notes

import chords.*

/* core */

object Pitch:
  val chromatic =
    Vector("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
  def isValid(pitch:String): Boolean = chromatic.contains(pitch.toUpperCase)

/* intervals */
sealed trait Interval:
  def semitones: Int

  def diatonicSteps: Int

  def readAs: String

  def quality: Quality

case object UnisonInterval extends Interval, Quality:
  override def semitones: Int = 0

  override def diatonicSteps: Int = 0
  override def readAs: String = "1"

  override def quality: Quality = Unison

case object SecondMinor extends Interval, Quality:
  override def semitones: Int = 1
  override def diatonicSteps: Int = 1
  override def readAs: String = "b2"

  override def quality: Quality = Minor

case object SecondMajor extends Interval, Quality:
  override def semitones: Int = 2
  override def diatonicSteps: Int = 1
  override def readAs: String = "2"

  override def quality: Quality = Major

case object ThirdMinor extends Interval, Quality:
  override def semitones: Int = 3
  override def diatonicSteps: Int = 2
  override def readAs: String = "b3"

  override def quality: Quality = Minor

case object ThirdMajor extends Interval, Quality:
  override def semitones: Int = 4
  override def diatonicSteps: Int = 2
  override def readAs: String = "3"

  override def quality: Quality = Major

case object FourthPerfect extends Interval, Quality:
  override def semitones: Int = 5
  override def diatonicSteps: Int = 3
  override def readAs: String = "4"

  override def quality: Quality = Perfect

case object FourthAugmented extends Interval, Quality:
  override def semitones: Int = 6
  override def diatonicSteps: Int = 3
  override def readAs: String = "#4"

  override def quality: Quality = Augmented

case object FifthDiminished extends Interval, Quality:
  override def semitones: Int = 6
  override def diatonicSteps: Int = 4
  override def readAs: String = "b5"

  override def quality: Quality = Diminished

case object FifthPerfect extends Interval, Quality:
  override def semitones: Int = 7
  override def diatonicSteps: Int = 4
  override def readAs: String = "5"

  override def quality: Quality = Perfect

case object SixthMinor extends Interval, Quality:
  override def semitones: Int = 8
  override def diatonicSteps: Int = 5
  override def readAs: String = "b6"

  override def quality: Quality = Minor

case object SixthMajor extends Interval, Quality:
  override def semitones: Int = 9
  override def diatonicSteps: Int = 5
  override def readAs: String = "6"

  override def quality: Quality = Major

case object SeventhMinor extends Interval, Quality:
  override def semitones: Int = 10
  override def diatonicSteps: Int = 6
  override def readAs: String = "b7"

  override def quality: Quality = Minor

case object SeventhMajor extends Interval, Quality:
  override def semitones: Int = 11
  override def diatonicSteps: Int = 6
  override def readAs: String = "7"

  override def quality: Quality = Major

case object Octave extends Interval, Quality:
  override def semitones: Int = 12

  override def diatonicSteps: Int = 7
  override def readAs: String = "8"

  override def quality: Quality = Perfect

sealed trait Quality

case object Unison extends Quality

case object Major extends Quality

case object Minor extends Quality

case object Diminished extends Quality

case object HalfDiminished extends Quality

case object Augmented extends Quality

case object Perfect extends Quality

case object Tone:
  val movement: Int = 2

case object Semitone:
  val movement: Int = 1

sealed trait Alteration

case object Sharp extends Alteration {
  private val suffix: String = "#"

  override def toString: String = suffix
}

case object Flat extends Alteration {
  private val suffix: String = "b"

  override def toString: String = suffix
}

class Note(private val pitch: String):
  require(Pitch.isValid(pitch), "Use a valid pitch")

  override def toString: String = pitch
  def toPitch: String = pitch

  def transposeBy(interval: Interval, backwards: Boolean = false): Note =
    if (interval.isInstanceOf[Unison.type]) return Note(pitch)
    val semitoneToMove: Int = if (backwards) -interval.semitones else interval.semitones
    val idx = Pitch.chromatic.indexOf(pitch.toUpperCase())
    import scala.math.floorMod
    val movement = floorMod(idx + semitoneToMove, Pitch.chromatic.length)
    Note(Pitch.chromatic(movement))

  def sharpen: Accidental = Note.sharpen(this)

  def flatten: Accidental =  Note.flatten(this)



object Note:
  private def sharpen(note: Note): Accidental = Accidental(note.pitch, Sharp)
  private def flatten(note: Note): Accidental = Accidental(note.pitch, Flat)

extension (s: String)
    def note: Note =
      s match
        case str if str.endsWith("#") =>
          Accidental(str.dropRight(1), Sharp)
        case str if str.endsWith("b") =>
          Accidental(str.dropRight(1), Flat)
        case _ =>
          new Note(s)
    //will add interval movement later
    def chord: Chord = RegularChord(Note(s), Note(s), Note(s))
    def power: PowerChord = PowerChord(Note(s), Note(s))

case class Accidental(pitch: String, alt: Alteration)
  extends Note(pitch), Alteration:
  override def toString = s"${pitch}${alt}"

