package org.aranadedoros.chordal
package chords

import notes.*
import intervals.*
import progressions.RomanDegree
import scala.math.floorMod
import extensions.*

/* chords */
private def isValidExtension(ext: Extension): Boolean =
  ext match
    case Ninth | FlatNinth | Eleventh |
        SharpEleventh | Thirteenth => true
    case _ => false

sealed trait Chord:
  def transposeBy(interval: Interval, backwards: Boolean = false): Chord

  /** method to print a core triad quality chord
    * @return
    *   A chord representation with its quality. e.g. Cm, C
    */
  def name: String
  protected def transposeNotes(
    notes: List[Note],
    interval: Interval,
    backwards: Boolean
  ): List[Note] =
    if interval.isInstanceOf[Unison.type] then notes
    else
      val semitoneToMove: Int = if backwards then -interval.semitones else interval.semitones
      notes.map {
        n =>
          val idx      = Pitch.chromatic.indexOf(n)
          val movement = floorMod(idx + semitoneToMove, Pitch.chromatic.length)
          Note(Pitch.chromatic(movement))
      }
  def notes: String = toString

sealed trait NoThird
sealed trait Add:
  def interval: Interval
  override def toString: String

case object Add2 extends Add:
  val interval: Interval = SecondMajorInterval
  override def toString  = "add2"

case object Add4 extends Add:
  val interval: Interval = FourthPerfectInterval
  override def toString  = "add4"

case object Add6 extends Add:
  val interval: Interval = SixthMajorInterval
  override def toString  = "add6"

case object Add9 extends Add:
  val interval: Interval = SecondMajorInterval
  override def toString  = "add9"

case class AddChord(root: Note, extensions: List[Note] = Nil, add: Add)
    extends Chord:

  def third: Note     = root.transposeDiatonically(ThirdMajorInterval)
  def fifth: Note     = root.transposeDiatonically(FifthPerfectInterval)
  def addedNote: Note = root.transposeDiatonically(add.interval)

  override def toString: String =
    val ext: String = if extensions.isEmpty then ""
    else extensions.map(_.toString).mkString(",", ",", ",")
    s"($root,$addedNote$ext)"

  override def name: String =
    val ext =
      if extensions.isEmpty then ""
      else extensions.map(_.toString).mkString("(", ",", ")")

    s"$root$add$ext"

  override def transposeBy(interval: Interval, backwards: Boolean = false): AddChord =
    AddChord(
      root = root.transposeDiatonically(interval, backwards),
      add = add,
      extensions = extensions.map(_.transposeDiatonically(interval, backwards))
    )

object Add:
  def apply(chord: Triad, add: Add): AddChord =
    require(
      chord.extensions.isEmpty, // anything extra beside the triad is an extension and thus implies a seventh
      "Add chords cannot contain sevenths"
    )
    AddChord(
      root = chord.root,
      add = add
    )

sealed trait Suspension:
  def interval: Interval
  override def toString: String

case object Sus2 extends Suspension:
  val interval: Interval        = SecondMajorInterval
  override def toString: String = "sus2"

case object Sus4 extends Suspension:
  val interval: Interval = FourthPerfectInterval

  override def toString: String = "sus4"

case class SuspendedChord(
  root: Note,
  suspension: Suspension,
  extensions: List[Extension] = Nil
) extends Chord, NoThird:

  def suspendedNote: Note = root.transposeDiatonically(suspension.interval)
  def fifth: Note =
    root.transposeDiatonically(FifthPerfectInterval)

  def extensionNotes: List[Note] =
    extensions.map(
      e => root.transposeDiatonically(e.interval)
    )

  override def toString: String =
    val ext = if extensions.isEmpty then ""
    else extensions.map(_.toString).mkString("", ",", ",")
    s"($root,$suspendedNote,$fifth$ext)"

  override def name: String =
    val ext =
      if extensions.isEmpty then ""
      else extensions.map(_.toString).mkString("(", ",", ")")
    s"$root$suspendedNote$ext"

  override def transposeBy(interval: Interval, backwards: Boolean = false): SuspendedChord =
    this.copy(root = root.transposeDiatonically(interval, backwards))

  def addExtensions(exts: List[Extension]): SuspendedChord =
    require(exts.forall(isValidExtension), s"Invalid extensions: $exts")
    copy(extensions = exts)

  def addExtension(ext: Extension): SuspendedChord =
    require(isValidExtension(ext), s"Invalid extension: $ext")
    copy(extensions = extensions :+ ext)

object SuspendedChord:
  def sus2(chord: Triad): SuspendedChord =
    SuspendedChord(
      root = chord.root,
      suspension = Sus2,
      extensions = chord.extensions
    )

  def sus4(chord: Triad): SuspendedChord =
    SuspendedChord(
      root = chord.root,
      suspension = Sus4,
      extensions = chord.extensions
    )

case class PowerChord(root: Note) extends Chord:
  override def name: String     = s"(${root}5)"
  def fifth: Note               = root.transposeDiatonically(FifthPerfectInterval)
  override def toString: String = s"($root,$fifth)"
  override def transposeBy(interval: Interval, backwards: Boolean): PowerChord =
    if interval.isInstanceOf[Unison.type] then this
    else
      val transposed = transposeNotes(List(root, fifth), interval, backwards)
      PowerChord(transposed(0))

sealed trait ChordQuality:
  def third: Interval
  def fifth: Interval
  def seventh: Option[Interval]
  def symbol: String

case object MajorChord extends ChordQuality:
  def third: Interval          = ThirdMajorInterval
  def fifth: Interval          = FifthPerfectInterval
  def seventh: Option[Nothing] = None
  def symbol                   = ""

case object MinorChord extends ChordQuality:
  def third: Interval          = ThirdMinorInterval
  def fifth: Interval          = FifthPerfectInterval
  def seventh: Option[Nothing] = None
  def symbol                   = "m"

case object DiminishedChord extends ChordQuality:
  def third: Interval          = ThirdMinorInterval
  def fifth: Interval          = FifthDiminishedInterval
  def seventh: Option[Nothing] = None
  def symbol                   = "°"

case object AugmentedChord extends ChordQuality:
  def third: Interval          = ThirdMajorInterval
  def fifth: Interval          = FifthAugmentedInterval
  def seventh: Option[Nothing] = None
  def symbol                   = "aug"

case object SeventhMajorChord extends ChordQuality:
  def third: Interval = ThirdMajorInterval
  def fifth: Interval = FifthPerfectInterval
  def seventh         = Some(SeventhMajorInterval)
  def symbol          = "maj7"

case object SeventhMinorChord extends ChordQuality:
  def third: Interval = ThirdMinorInterval
  def fifth: Interval = FifthPerfectInterval
  def seventh         = Some(SeventhMinorInterval)
  def symbol          = "7"

case object HalfDiminishedChord extends ChordQuality:
  def third: Interval = ThirdMinorInterval
  def fifth: Interval = FifthDiminishedInterval
  def seventh         = Some(SeventhMinorInterval)
  def symbol          = "ø"

case object FullyDiminishedChord extends ChordQuality:
  def third: Interval = ThirdMinorInterval
  def fifth: Interval = FifthDiminishedInterval
  def seventh         = Some(SeventhMinorInterval)
  def symbol          = "°7"

//todo extract triad and tetra
case class Triad(root: Note, quality: ChordQuality, extensions: List[Extension] = Nil)
    extends Chord:
  require(
    third == root.transposeDiatonically(ThirdMajorInterval) ||
      third == root.transposeDiatonically(ThirdMinorInterval),
    s"Triad must contain a major or minor third"
  )
  def third: Note =
    root.transposeDiatonically(quality.third)

  def fifth: Note =
    root.transposeDiatonically(quality.fifth)

  def seventh: Option[Note] =
    quality.seventh.map(
      interval => root.transposeDiatonically(interval)
    )

  def extensionNotes: List[Note] =
    extensions.map(
      e => root.transposeDiatonically(e.interval)
    )

  override def toString: String =
    val notes =
      root :: third :: fifth :: seventh.toList ::: extensionNotes
    notes.mkString("(", ",", ")")

  override def name: String =
    val exts = extensions.map(_.symbol).mkString
    s"$root${quality.symbol}$exts"

  override def transposeBy(interval: Interval, backwards: Boolean): Triad =
    if interval.isInstanceOf[Unison.type] then this
    else
      this.copy(root = root.transposeDiatonically(interval, backwards))

  def toPowerChord = PowerChord(root)

  def addExtensions(exts: List[Extension]): Triad =
    require(exts.forall(isValidExtension), s"Invalid extensions: $exts")
    copy(extensions = exts)

  def addExtension(ext: Extension): Triad =
    require(isValidExtension(ext), s"Invalid extension: $ext")
    copy(extensions = extensions :+ ext)

object Triad:
  def major(root: Note): Triad =
    Triad(root, MajorChord)

  def minor(root: Note): Triad =
    Triad(root, MinorChord)

  def diminished(root: Note): Triad =
    Triad(root, DiminishedChord)

  def augmented(root: Note): Triad =
    Triad(root, AugmentedChord)

  def fromDegree(root: Note, degree: RomanDegree): Triad =
    val degreeRoot =
      root.transposeDiatonically(
        Interval.fromDiatonicSteps(degree.diatonicSteps)
      )
    Triad(
      root = degreeRoot,
      quality = degree.quality
    )

extension (chord: Triad)
  def add2: AddChord       = Add(chord, Add2)
  def add4: AddChord       = Add(chord, Add4)
  def add6: AddChord       = Add(chord, Add6)
  def add9: AddChord       = Add(chord, Add9)
  def sus2: SuspendedChord = SuspendedChord.sus2(chord)
  def sus4: SuspendedChord = SuspendedChord.sus4(chord)
