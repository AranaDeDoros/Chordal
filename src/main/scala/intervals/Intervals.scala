package org.aranadedoros.chordal
package intervals

/* intervals */
sealed trait Quality

case object Unison         extends Quality
case object Major          extends Quality
case object Minor          extends Quality
case object Diminished     extends Quality
case object HalfDiminished extends Quality
case object Augmented      extends Quality

case object Perfect extends Quality
sealed trait Interval:
  def semitones: Int
  def diatonicSteps: Int
  def readAs: String
  def quality: Quality

case object UnisonInterval extends Interval:
  override def semitones: Int     = 0
  override def diatonicSteps: Int = 0
  override def readAs: String     = "1"
  override def quality: Quality   = Unison

case object SecondMinorInterval extends Interval:
  override def semitones: Int     = 1
  override def diatonicSteps: Int = 1
  override def readAs: String     = "b2"
  override def quality: Quality   = Minor

case object SecondMajorInterval extends Interval:
  override def semitones: Int     = 2
  override def diatonicSteps: Int = 1
  override def readAs: String     = "2"
  override def quality: Quality   = Major

case object ThirdMinorInterval extends Interval:
  override def semitones: Int     = 3
  override def diatonicSteps: Int = 2
  override def readAs: String     = "b3"
  override def quality: Quality   = Minor

case object ThirdMajorInterval extends Interval:
  override def semitones: Int     = 4
  override def diatonicSteps: Int = 2
  override def readAs: String     = "3"
  override def quality: Quality   = Major

case object FourthPerfectInterval extends Interval:
  override def semitones: Int     = 5
  override def diatonicSteps: Int = 3
  override def readAs: String     = "4"
  override def quality: Quality   = Perfect

case object FourthAugmentedInterval extends Interval:
  override def semitones: Int     = 6
  override def diatonicSteps: Int = 3
  override def readAs: String     = "#4"
  override def quality: Quality   = Augmented

case object FifthDiminishedInterval extends Interval:
  override def semitones: Int     = 6
  override def diatonicSteps: Int = 4
  override def readAs: String     = "b5"
  override def quality: Quality   = Diminished

case object FifthPerfectInterval extends Interval:
  override def semitones: Int     = 7
  override def diatonicSteps: Int = 4
  override def readAs: String     = "5"
  override def quality: Quality   = Perfect

case object SixthMinorInterval extends Interval:
  override def semitones: Int     = 8
  override def diatonicSteps: Int = 5
  override def readAs: String     = "b6"
  override def quality: Quality   = Minor

case object SixthMajorInterval extends Interval:
  override def semitones: Int     = 9
  override def diatonicSteps: Int = 5
  override def readAs: String     = "6"
  override def quality: Quality   = Major

case object SeventhMinorInterval extends Interval:
  override def semitones: Int     = 10
  override def diatonicSteps: Int = 6
  override def readAs: String     = "b7"
  override def quality: Quality   = Minor

case object SeventhMajorInterval extends Interval:
  override def semitones: Int     = 11
  override def diatonicSteps: Int = 6
  override def readAs: String     = "7"
  override def quality: Quality   = Major

case object OctaveInterval extends Interval:
  override def semitones: Int     = 12
  override def diatonicSteps: Int = 7
  override def readAs: String     = "8"
  override def quality: Quality   = Perfect

case object Tone:
  val steps: Int = 2

case object Semitone:
  val steps: Int = 1
