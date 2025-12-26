package org.aranadedoros.chordal
package progressions

import chords.*
import intervals.*

enum RomanDegree(
  val diatonicSteps: Int,
  val quality: ChordQuality
):
  case I      extends RomanDegree(0, MajorChord)
  case ii     extends RomanDegree(1, MinorChord)
  case iii    extends RomanDegree(2, MinorChord)
  case IV     extends RomanDegree(3, MajorChord)
  case V      extends RomanDegree(4, MajorChord)
  case vi     extends RomanDegree(5, MinorChord)
  case viidim extends RomanDegree(6, DiminishedChord)
/* progression */
object Progression:
  /** ii–V–I (Jazz) */
  def jazz(chord: Triad): Progression =
    val ii = Triad.minor(chord.root.transposeDiatonically(SecondMajorInterval))
    val V  = Triad.major(chord.root.transposeDiatonically(FourthPerfectInterval))
    val I  = chord
    Progression(List(ii, V, I))

  /** I–V–vi–IV (Pop / Rock) */
  def pop(chord: Triad): Progression =
    val I  = chord
    val V  = Triad.major(chord.root.transposeDiatonically(FifthPerfectInterval))
    val vi = Triad.minor(chord.root.transposeDiatonically(SixthMajorInterval))
    val IV = Triad.major(chord.root.transposeDiatonically(FourthPerfectInterval))
    Progression(List(I, V, vi, IV))

  /** I–IV–V (Blues) */
  def blues(chord: Triad): Progression =
    val I  = chord
    val IV = Triad.major(chord.root.transposeDiatonically(FourthPerfectInterval))
    val V  = Triad.major(chord.root.transposeDiatonically(FifthPerfectInterval))
    Progression(List(I, IV, V))

  /** vi–IV–I–V (Ballad / Pop alt) */
  def ballad(chord: Triad): Progression =
    val vi = Triad.minor(chord.root.transposeDiatonically(SixthMajorInterval))
    val IV = Triad.major(chord.root.transposeDiatonically(FourthPerfectInterval))
    val I  = chord
    val V  = Triad.major(chord.root.transposeDiatonically(FifthPerfectInterval))
    Progression(List(vi, IV, I, V))

  def fromDegrees(
    tonic: Triad,
    degrees: RomanDegree*
  ): Progression =
    val chords =
      degrees.toList.map {
        degree =>
          Triad.fromDegree(tonic.root, degree)
      }
    Progression(chords)

case class Progression(chords: List[Chord]):

  def has(chord: Chord): Option[Chord] =
    chords.find(
      c => c == chord
    )

  def ++(progression: Progression): List[Chord] =
    chords ++ progression.chords

  def +(chord: Chord): List[Chord] =
    chords :+ chord

  def -(chord: Chord, dropAll: Boolean = false): List[Chord] =
    if (dropAll) {
      chords.filterNot(_ == chord)
    } else {
      val (left, right) = chords.span(_ != chord)
      val newList       = left ::: right.drop(1)
      newList
    }

  override def toString: String =
    chords.mkString("(", ",", ")")

extension (c: Triad)
  def jazz: Progression   = Progression.jazz(c)
  def pop: Progression    = Progression.pop(c)
  def blues: Progression  = Progression.blues(c)
  def ballad: Progression = Progression.ballad(c)
