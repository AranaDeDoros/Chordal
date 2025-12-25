package org.aranadedoros.chordal
package chords

import notes.*
import intervals.*
import scala.math.floorMod

/* chords */
sealed trait Chord:
  def transposeBy(interval: Interval, backwards: Boolean = false): Chord
  /**
   * method to print a core triad quality chord
    * @return A chord representation with its quality. e.g. Cm, C
   */
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

sealed trait Add:
  def interval: Interval
  override def toString: String

case object Add2 extends Add:
  val interval: Interval = SecondMajorInterval
  override def toString = "add2"

case object Add4 extends Add:
  val interval: Interval = FourthPerfectInterval
  override def toString = "add4"

case object Add6 extends Add:
  val interval: Interval = SixthMajorInterval
  override def toString = "add6"

case object Add9 extends Add:
  val interval: Interval = SecondMajorInterval
  override def toString = "add9"

case class AddChord(root: Note, third: Note, fifth: Note,
                    extensions: List[Note] = Nil, add: Add) extends Chord:
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
          root :: third :: fifth :: extensions,
          interval,
          backwards
        )
      AddChord(
        root = transposed(0),
        third = transposed(1),
        fifth = transposed(2),
        extensions = transposed.drop(3),
        add = add
      )

object Add:
  def apply(chord: Triad, add: Add): AddChord  =
    require(
      chord.extensions.forall { _ =>
        val seventhMajor = chord.root.transposeBy(SeventhMajorInterval)
        val seventhMinor = chord.root.transposeBy(SeventhMinorInterval)
        chord.third.toPitch != seventhMajor.toPitch &&
          chord.third.toPitch != seventhMinor.toPitch
      },
      "Add chords cannot contain sevenths"
    )
    AddChord(
      root = chord.root,
      third = chord.third,
      fifth = chord.fifth,
      extensions = chord.extensions,
      add = add
    )

sealed trait Suspension:
  def interval: Interval

case object Sus2 extends Suspension:
  val interval: Interval = SecondMajorInterval

  override def toString: String = "sus2"

case object Sus4 extends Suspension:
  val interval: Interval = FourthPerfectInterval

  override def toString: String = "sus4"

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

private object SuspendedChord:

  def sus2(chord: Triad): SuspendedChord =
    SuspendedChord(
      root = chord.root,
      suspension = Sus2,
      fifth = chord.fifth,
      extensions = chord.extensions
    )

  def sus4(chord: Triad): SuspendedChord =
    SuspendedChord(
      root = chord.root,
      suspension = Sus4,
      fifth = chord.fifth,
      extensions = chord.extensions
    )

case class PowerChord(root: Note, fifth: Note) extends Chord:
  override def render: String = s"(${root}5)"
  override def toString: String = s"($root,$fifth)"
  override def transposeBy(interval: Interval, backwards: Boolean): Chord =
    if interval.isInstanceOf[Unison.type] then this
    else
      val transposed = transposeNotes(List(root, fifth), interval, backwards)
      PowerChord(transposed(0), transposed(1))

sealed trait ChordQuality
case object MajorChord extends ChordQuality
case object MinorChord extends ChordQuality
case object DiminishedChord extends ChordQuality
case object HalfDiminishedChord extends ChordQuality
case object AugmentedChord extends ChordQuality
case object PerfectChord extends ChordQuality
case object FullyDiminishedChord extends ChordQuality
case object SeventhMajorChord extends ChordQuality
case object SeventhMinorChord extends ChordQuality

sealed  trait QualitySymbol:
  val symbol : String
case object NoSymbol extends QualitySymbol:
  val symbol: String = ""
case object MajorSymbol extends QualitySymbol:
  val symbol : String = NoSymbol.symbol
case object MinorSymbol extends QualitySymbol:
  val symbol : String = "m"
case object DiminishedSymbol extends QualitySymbol:
  val symbol : String = "°"
case object AugmentedSymbol extends QualitySymbol:
  val symbol : String = "aug"
case object SeventhMajorSymbol extends QualitySymbol:
  val symbol = "maj7"
case object SeventhMinorSymbol extends QualitySymbol:
  val symbol = "7"
case object HalfDiminishedSymbol extends QualitySymbol:
  val symbol = "ø"
case object FullyDiminishedSymbol extends QualitySymbol:
  val symbol = "°7"

//todo extract triad and tetra
case class Triad(root: Note, third: Note, fifth: Note,
                        extensions: List[Note] = Nil) extends Chord:
  require(
    third.toPitch == root.transposeBy(ThirdMajorInterval).toPitch ||
      third.toPitch == root.transposeBy(ThirdMinorInterval).toPitch,
    s"Triad must contain a major or minor third"
  )

  private def seventh: Option[Note] =
    extensions.find { n =>
      n.toPitch == root.transposeBy(SeventhMajorInterval).toPitch ||
        n.toPitch == root.transposeBy(SeventhMinorInterval).toPitch
    }

  private def triadQuality: ChordQuality =
    val isMajorThird = third.toPitch == root.transposeBy(ThirdMajorInterval).toPitch
    val isMinorThird = third.toPitch == root.transposeBy(ThirdMinorInterval).toPitch

    val isPerfectFifth = fifth.toPitch == root.transposeBy(FifthPerfectInterval).toPitch
    val isDiminishedFifth = fifth.toPitch == root.transposeBy(FifthDiminishedInterval).toPitch
    val isAugmentedFifth = fifth.toPitch == root.transposeBy(FourthAugmentedInterval).toPitch

    (isMajorThird, isMinorThird, isPerfectFifth, isDiminishedFifth, isAugmentedFifth) match
      case (true, false, true, false, false) => MajorChord
      case (false, true, true, false, false) => MinorChord
      case (false, true, false, true, false) => DiminishedChord
      case (true, false, false, false, true) => AugmentedChord
      case _ =>
        throw new IllegalStateException(
          s"Invalid triad: root=$root third=$third fifth=$fifth"
        )

  private def extendedQuality: ChordQuality =
    val triad = triadQuality
    val sev = seventh.get

    val isMajorSeventh = sev.toPitch == root.transposeBy(SeventhMajorInterval).toPitch
    val isMinorSeventh = sev.toPitch == root.transposeBy(SeventhMinorInterval).toPitch

    (triad, isMajorSeventh, isMinorSeventh) match
      case (MajorChord, true, false) => SeventhMajorChord // maj7
      case (MajorChord, false, true) => SeventhMinorChord // 7
      case (MinorChord, false, true) => SeventhMinorChord // m7
      case (DiminishedChord, false, true) => HalfDiminishedChord // ø7
      case (DiminishedChord, true, false) => FullyDiminishedChord // °7
      case _ =>
        throw new IllegalStateException(
          s"Invalid seventh chord: triad=$triad seventh=$sev"
        )

  def quality: ChordQuality =
      if seventh.isDefined then extendedQuality
      else triadQuality

  override def toString : String = s"($root,$third,$fifth${if (extensions.nonEmpty)
    s",${extensions.mkString(",")}" else ""})"

  override def render: String =
    val exts =
      if extensions.isEmpty then ""
      else extensions.map(_.toString).mkString(",")

    s"$root${
      quality match
        case MajorChord => MajorSymbol.symbol
        case MinorChord => MinorSymbol.symbol
        case DiminishedChord => DiminishedSymbol.symbol
        case AugmentedChord => AugmentedSymbol.symbol
        case SeventhMajorChord      => SeventhMajorSymbol.symbol
        case SeventhMinorChord      => SeventhMinorSymbol.symbol
        case HalfDiminishedChord    => HalfDiminishedSymbol.symbol
        case FullyDiminishedChord   => FullyDiminishedSymbol.symbol
        case PerfectChord => NoSymbol.symbol
    }$exts"

  override def transposeBy(interval: Interval, backwards: Boolean): Chord =
    if interval.isInstanceOf[Unison.type] then this
    else
      val transposed = transposeNotes(root :: third :: fifth :: extensions, interval, backwards)
      Triad(transposed(0), transposed(1), transposed(2), transposed.drop(3))

  def toPowerChord = PowerChord(root, fifth)

  def addExtensions(extensions: List[Note]): Triad =
    this.copy(extensions = extensions)

  def addExtension(extension: Note): Triad =
    this.copy(extensions = extensions :+ extension)

extension (chord: Triad)
  def add2: AddChord = Add(chord, Add2)
  def add4: AddChord = Add(chord, Add4)
  def add6: AddChord = Add(chord, Add6)
  def add9: AddChord = Add(chord, Add9)
  def sus2: SuspendedChord = SuspendedChord.sus2(chord)
  def sus4: SuspendedChord = SuspendedChord.sus4(chord)


