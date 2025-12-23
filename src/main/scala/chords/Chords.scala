package org.aranadedoros.chordal
package chords

import notes.{FourthPerfect, Interval, Note, Pitch,
  SecondMajor, SecondMinor, SeventhMajor, SeventhMinor, SixthMajor,
  SixthMinor, ThirdMajor, ThirdMinor, Unison}

import scala.math.floorMod

/* chords */

sealed trait Quality

case object Major extends Quality

case object Minor extends Quality

//case object Diminished extends Quality
//
//case object Augmented extends Quality

sealed trait Chord:
  def transposeBy(interval: Interval, backwards: Boolean = false): Chord
  def render : String

  protected def transposeNotes(notes: List[Note], interval: Interval, backwards: Boolean): List[Note] =
    if interval.isInstanceOf[Unison.type] then notes
    else
      val semitoneToMove = if backwards then -interval.semitones else interval.semitones
      notes.map { n =>
        val idx = Pitch.chromatic.indexOf(n.toPitch)
        val movement = floorMod(idx + semitoneToMove, Pitch.chromatic.length)
        Note(Pitch.chromatic(movement))
      }

sealed trait NoThird

sealed trait Suspension:
  def interval: Interval

case object Sus2 extends Suspension:
  val interval: Interval = SecondMajor

  override def toString: String = "sus2"

case object Sus4 extends Suspension:
  val interval: Interval = FourthPerfect

  override def toString: String = "sus4"

sealed trait Add:
  def interval: Interval
  override def toString: String

case object Add2 extends Add:
  val interval: Interval = SecondMajor
  override def toString = "add2"

case object Add4 extends Add:
  val interval: Interval = FourthPerfect
  override def toString = "add4"

case object Add6 extends Add:
  val interval: Interval = SixthMajor
  override def toString = "add6"

case object Add9 extends Add:
  val interval: Interval = SecondMajor
  override def toString = "add9"

case class AddChord(root: Note, add: Add, fifth: Note, extensions: List[Note] = Nil) extends Chord:
  def addedNote: Note = root.transposeBy(add.interval)

  override def toString : String  =
    val ext = if extensions.isEmpty then ""
    else extensions.map(_.toString).mkString(",",",",",")
      s"($root,$fifth,$addedNote$ext)"

  override def render: String =
    val ext =
      if extensions.isEmpty then ""
      else extensions.map(_.toString).mkString("(",",",")")

    s"$root$add$ext"

  override def transposeBy(interval: Interval, backwards: Boolean = false): Chord =
    if interval.isInstanceOf[Unison.type] then this
    else
      val transposed =
        transposeNotes(
          root :: fifth :: extensions,
          interval,
          backwards
        )

      AddChord(
        root = transposed(0),
        add = add,
        fifth = transposed(1),
        extensions = transposed.drop(2)
      )

object Add:

  def apply(chord: RegularChord, add: Add): AddChord  =

    require(
      chord.extensions.forall { _ =>
        val seventhMajor = chord.root.transposeBy(SeventhMajor)
        val seventhMinor = chord.root.transposeBy(SeventhMinor)
        chord.third.toPitch != seventhMajor.toPitch &&
          chord.third.toPitch != seventhMinor.toPitch
      },
      "Add chords cannot contain sevenths"
    )

    AddChord(
      root = chord.root,
      add = add,
      fifth = chord.fifth,
      extensions = chord.extensions
    )




case class SuspendedChord(root: Note, suspension: Suspension, fifth: Note,
                          extensions: List[Note] = Nil) extends Chord, NoThird:
  def suspendedNote: Note = root.transposeBy(suspension.interval)

  override def toString: String =
    val ext = if extensions.isEmpty then ""
    else extensions.map(_.toString).mkString("",",",",")
    s"($root,$suspendedNote,$fifth$ext)"

  override def render: String =
    val ext =
      if extensions.isEmpty then ""
      else extensions.map(_.toString).mkString("(",",",")")
    s"$root$suspension$ext"

  override def transposeBy(interval: Interval, backwards: Boolean = false): Chord =
    if interval.isInstanceOf[Unison.type] then this
    else
      val transposed =
        transposeNotes(
          root :: fifth :: extensions,
          interval,
          backwards
        )

      SuspendedChord(
        root = transposed(0),
        suspension = suspension, // interval identity is preserved
        fifth = transposed(1),
        extensions = transposed.drop(2)
      )


object SuspendedChord:

  def sus2(chord: RegularChord): SuspendedChord =
    SuspendedChord(
      root = chord.root,
      suspension = Sus2,
      fifth = chord.fifth,
      extensions = chord.extensions
    )

  def sus4(chord: RegularChord): SuspendedChord =
    SuspendedChord(
      root = chord.root,
      suspension = Sus4,
      fifth = chord.fifth,
      extensions = chord.extensions
    )


case class RegularChord(root: Note, third: Note, fifth: Note,
                        extensions: List[Note] = Nil) extends Chord:
  require(
    third.toPitch == root.transposeBy(ThirdMajor).toPitch ||
      third.toPitch == root.transposeBy(ThirdMinor).toPitch,
    s"RegularChord must contain a major or minor third"
  )

  def quality: Quality =
    val majorThird = root.transposeBy(ThirdMajor).toPitch
    val minorThird = root.transposeBy(ThirdMinor).toPitch

    if third.toPitch == majorThird then Major
    else if third.toPitch == minorThird then Minor
    else
      throw new IllegalStateException(
        s"Invalid RegularChord: third $third is neither major nor minor above root $root"
      )

  override def toString : String = s"($root,$third,$fifth${if (extensions.nonEmpty) s",${extensions.mkString(",")}" else ""})"
  override def render: String =
    val exts =
      if extensions.isEmpty then ""
      else extensions.map(_.toString).mkString(",")

    s"$root${
      quality match
        case Major => ""
        case Minor => "m"
    }$exts"

  override def transposeBy(interval: Interval, backwards: Boolean): Chord =
    if interval.isInstanceOf[Unison.type] then this
    else
      val transposed = transposeNotes(root :: third :: fifth :: extensions, interval, backwards)
      RegularChord(transposed(0), transposed(1), transposed(2), transposed.drop(3))

  def toPowerChord = PowerChord(root, fifth)

  def addExtensions(extensions: List[Note]): RegularChord =
    this.copy(extensions = extensions)

  def addExtension(extension: Note): RegularChord =
    this.copy(extensions = extensions :+ extension)

extension (chord: RegularChord)
  def add2: AddChord = Add(chord, Add2)
  def add4: AddChord = Add(chord, Add4)
  def add6: AddChord = Add(chord, Add6)
  def add9: AddChord = Add(chord, Add9)
  def sus2: SuspendedChord = SuspendedChord.sus2(chord)
  def sus4: SuspendedChord = SuspendedChord.sus4(chord)

case class PowerChord(root: Note, fifth: Note) extends Chord:
  override def render: String = s"(${root}5)"
  override def toString: String = s"($root,$fifth)"
  override def transposeBy(interval: Interval, backwards: Boolean): Chord =
    if interval.isInstanceOf[Unison.type] then this
    else
      val transposed = transposeNotes(List(root, fifth), interval, backwards)
      PowerChord(transposed(0), transposed(1))
