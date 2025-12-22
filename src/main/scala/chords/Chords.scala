package org.aranadedoros.chordal
package chords

import notes.{Interval, Note, Pitch, Unison}

import scala.math.floorMod

/* chords */
sealed trait Chord:
  def transposeBy(interval: Interval, backwards: Boolean = false): Chord
  protected def transposeNotes(notes: List[Note], interval: Interval, backwards: Boolean): List[Note] =
    if interval.isInstanceOf[Unison.type] then notes
    else
      val semitoneToMove = if backwards then -interval.semitones else interval.semitones
      notes.map { n =>
        val idx = Pitch.chromatic.indexOf(n.toPitch)
        val movement = floorMod(idx + semitoneToMove, Pitch.chromatic.length)
        Note(Pitch.chromatic(movement))
      }

case class RegularChord(root: Note, third: Note, fifth: Note,
                        extensions: List[Note] = Nil) extends Chord:
  override def toString: String =
  s"($root, $third, $fifth${if (extensions.nonEmpty) s", ${extensions.mkString(", ")}" else ""})"

  override def transposeBy(interval: Interval, backwards: Boolean): Chord =
    if interval.isInstanceOf[Unison.type] then this
    else
      val transposed = transposeNotes(root :: third :: fifth :: extensions, interval, backwards)
      RegularChord(transposed(0), transposed(1), transposed(2), transposed.drop(3))

  def toPowerChord = PowerChord(root, fifth)

  def addExtensions(extensions: List[Note]) : Chord = this.copy(extensions=extensions)

  def addExtension(extension: Note) : Chord = this.copy(extensions=extensions:+extension)

case class PowerChord(root: Note, fifth: Note) extends Chord:
  override def toString: String = s"($root, $fifth)"

  override def transposeBy(interval: Interval, backwards: Boolean): Chord =
    if interval.isInstanceOf[Unison.type] then this
    else
      val transposed = transposeNotes(List(root, fifth), interval, backwards)
      PowerChord(transposed(0), transposed(1))


