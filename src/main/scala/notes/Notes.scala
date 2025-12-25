package org.aranadedoros.chordal
package notes

import chords.*
import intervals.*

/* core */

object Pitch:
  val chromatic =
    Vector("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
  def isValid(pitch:String): Boolean = chromatic.contains(pitch.toUpperCase)

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
    def chord: Chord =
      //major on default
      val third = Note(s).transposeBy(ThirdMajorInterval)
      val fifth = Note(s).transposeBy(FifthPerfectInterval)
      RegularChord(Note(s), Note(s), Note(s))
    def power: PowerChord = PowerChord(Note(s),
      Note(s).transposeBy(FifthPerfectInterval))

case class Accidental(pitch: String, alt: Alteration)
  extends Note(pitch), Alteration:
  override def toString = s"${pitch}${alt}"

