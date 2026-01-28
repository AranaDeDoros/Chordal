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

trait IntervallicChord:
  def root: Note
  def intervals: List[Interval]

  final def notes: List[Note] =
    intervals.map(
      interval => root.transposeDiatonically(interval)
    )
  final def asNotes: String =
    notes.mkString("(", ",", ")")

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
  // def notes: String = toString

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

case class AddChord(triad: Triad, add: Add)
    extends Chord, IntervallicChord:

  def root: Note      = triad.root
  def addedNote: Note = triad.root.transposeDiatonically(add.interval)

  def intervals: List[Interval] =
    List(
      UnisonInterval,
      triad.quality.third,
      triad.quality.fifth,
      add.interval
    )

  override def toString: String =
    s"(${triad.root},${triad.third},${triad.fifth},$addedNote)"

  override def name: String =
    s"${triad.root}$add"

  override def transposeBy(interval: Interval, backwards: Boolean = false): AddChord =
    AddChord(
      triad = triad,
      add = add
    )

object Add:
  def apply(chord: Triad, add: Add): AddChord =
    AddChord(
      triad = chord,
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
  suspension: Suspension
) extends Chord, NoThird, IntervallicChord:

  def suspendedNote: Note = root.transposeDiatonically(suspension.interval)
  def fifth: Note =
    root.transposeDiatonically(FifthPerfectInterval)

  override def toString: String =
    s"($root,$suspendedNote,$fifth)"

  override def name: String =
    s"$root$suspension"

  def intervals: List[Interval] =
    List(UnisonInterval, suspension.interval, FifthPerfectInterval)

  override def transposeBy(interval: Interval, backwards: Boolean = false): SuspendedChord =
    this.copy(root = root.transposeDiatonically(interval, backwards))

object SuspendedChord:
  def sus2(chord: Triad): SuspendedChord =
    SuspendedChord(
      root = chord.root,
      suspension = Sus2
    )

  def sus4(chord: Triad): SuspendedChord =
    SuspendedChord(
      root = chord.root,
      suspension = Sus4
    )

case class PowerChord(root: Note) extends Chord, IntervallicChord:

  override def name: String = s"${root}5"

  def intervals: List[Interval] =
    List(UnisonInterval, FifthPerfectInterval)

  def fifth: Note = root.transposeDiatonically(FifthPerfectInterval)

  override def toString: String = s"($root,$fifth)"

  override def transposeBy(interval: Interval, backwards: Boolean): PowerChord =
    if interval.isInstanceOf[Unison.type] then this
    else
      val transposed =
        transposeNotes(notes, interval, backwards)
      PowerChord(transposed.head)

case class Triad(root: Note, quality: TriadQuality)
    extends Chord, IntervallicChord:

  def third: Note =
    root.transposeDiatonically(quality.third)

  def fifth: Note =
    root.transposeDiatonically(quality.fifth)

  def intervals: List[Interval] =
    List(UnisonInterval, quality.third, quality.fifth)

  override def toString: String =
    List(root, third, fifth).mkString("(", ",", ")")

  override def name: String =
    s"$root${quality.symbol}"

  override def transposeBy(interval: Interval, backwards: Boolean): Triad =
    if interval.isInstanceOf[Unison.type] then this
    else
      this.copy(root = root.transposeDiatonically(interval, backwards))

  def toPowerChord = PowerChord(root)

object Triad:
  def major(root: Note): Triad =
    Triad(root, MajorTriad)

  def minor(root: Note): Triad =
    Triad(root, MinorTriad)

  def diminished(root: Note): Triad =
    Triad(root, DiminishedTriad)

  def augmented(root: Note): Triad =
    Triad(root, AugmentedTriad)

extension (chord: Triad)
  def add2: AddChord       = Add(chord, Add2)
  def add4: AddChord       = Add(chord, Add4)
  def add6: AddChord       = Add(chord, Add6)
  def add9: AddChord       = Add(chord, Add9)
  def sus2: SuspendedChord = SuspendedChord.sus2(chord)
  def sus4: SuspendedChord = SuspendedChord.sus4(chord)

sealed trait TriadQuality:
  def third: Interval
  def fifth: Interval
  def symbol: String

case object MajorTriad extends TriadQuality:
  val third: Interval = ThirdMajorInterval
  val fifth: Interval = FifthPerfectInterval
  val symbol          = ""

case object MinorTriad extends TriadQuality:
  val third: Interval = ThirdMinorInterval
  val fifth: Interval = FifthPerfectInterval
  val symbol          = "m"

case object DiminishedTriad extends TriadQuality:
  val third: Interval = ThirdMinorInterval
  val fifth: Interval = FifthDiminishedInterval
  val symbol          = "°"

case object AugmentedTriad extends TriadQuality:
  val third: Interval = ThirdMajorInterval
  val fifth: Interval = FifthAugmentedInterval
  val symbol          = "aug"

sealed trait SeventhQuality extends TriadQuality {
  def seventh: Interval
}

case object MajorSeventh extends SeventhQuality:
  val third: Interval   = ThirdMajorInterval
  val fifth: Interval   = FifthPerfectInterval
  val seventh: Interval = SeventhMajorInterval
  val symbol            = "maj7"

case object MinorSeventh extends SeventhQuality:
  val third: Interval   = ThirdMinorInterval
  val fifth: Interval   = FifthPerfectInterval
  val seventh: Interval = SeventhMinorInterval
  val symbol            = "7"

case object HalfDiminished:
  val triad: TriadQuality     = DiminishedTriad
  val seventh: SeventhQuality = MinorSeventh

case object FullyDiminished:
  val triad: TriadQuality = DiminishedTriad
  val seventh: Interval   = SeventhMinorInterval

case class SeventhChord(
  triad: Triad,
  seventh: SeventhQuality
) extends Chord, IntervallicChord:

  def root: Note = triad.root

  def intervals: List[Interval] =
    triad.intervals :+ seventh.seventh

  override def name: String =
    s"${triad.name}${seventh.symbol}"

  override def toString: String = asNotes

  def addExtensions(exts: List[Extension]): ExtendedChord =
    require(exts.forall(isValidExtension), s"Invalid extensions: $exts")
    ExtendedChord(this, exts)

  def addExtension(ext: Extension): ExtendedChord =
    require(isValidExtension(ext), s"Invalid extension: $ext")
    ExtendedChord(this, List(ext))

  override def transposeBy(interval: Interval, backwards: Boolean): SeventhChord =
    if interval.isInstanceOf[Unison.type] then this
    else
      copy(
        triad = triad.transposeBy(interval, backwards)
      )

case class ExtendedChord(
  base: SeventhChord,
  extensions: List[Extension]
) extends Chord, IntervallicChord:

  def root: Note = base.root
  def extensionNotes: List[Note] =
    extensions.map(
      e =>
        base.triad.root.transposeDiatonically(e.interval)
    )

  def intervals: List[Interval] =
    base.intervals ++ extensions.map(_.interval)

  override def name: String =
    s"${base.name}${extensions.map(_.symbol).mkString}"

  def addExtensions(exts: List[Extension]): ExtendedChord =
    require(exts.forall(isValidExtension), s"Invalid extensions: $exts")
    copy(extensions = exts)

  def addExtension(ext: Extension): ExtendedChord =
    require(isValidExtension(ext), s"Invalid extension: $ext")
    copy(extensions = extensions :+ ext)

  override def transposeBy(interval: Interval, backwards: Boolean): ExtendedChord =
    if interval.isInstanceOf[Unison.type] then this
    else
      copy(
        base = base.transposeBy(interval, backwards)
      )

extension (t: Triad)
  def withSeventh(q: SeventhQuality): SeventhChord =
    SeventhChord(t, q)

case class SlashChord(
  chord: Chord,
  bass: Note
) extends Chord {

  override def transposeBy(interval: Interval, backwards: Boolean = false): SlashChord =
    SlashChord(
      chord.transposeBy(interval),
      bass.transposeDiatonically(interval)
    )

  override def name: String =
    s"${chord.name}/$bass"

  override def toString: String =
    s"${chord.toString}/$bass"
}

extension (c: Chord)
  infix def /(bass: Note): SlashChord =
    SlashChord(c, bass)
  infix def withBass(bass: Note): SlashChord =
    /(bass)

object Voicing:
  def invert(chord: IntervallicChord, n: Int): List[Note] =
    chord.notes.drop(n) ++ chord.notes.take(n)

  def drop2(chord: IntervallicChord): List[Note] =
    chord.notes match
      case root :: third :: fifth :: seventh :: rest =>
        List(root, fifth, third, seventh) ++ rest
      case _ => chord.notes
