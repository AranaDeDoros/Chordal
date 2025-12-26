package org.aranadedoros.chordal
package extensions
import intervals.*

sealed trait Extension:
  def interval: Interval
  def symbol: String

case object Ninth extends Extension:
  override def interval: Interval = SecondMajorInterval
  override def symbol             = "9"

case object FlatNinth extends Extension:
  override def interval: Interval = SecondMinorInterval
  override def symbol             = "b9"

case object Eleventh extends Extension:
  override def interval: Interval = FourthPerfectInterval
  override def symbol             = "11"

case object SharpEleventh extends Extension:
  override def interval: Interval = FourthAugmentedInterval
  override def symbol             = "#11"

case object Thirteenth extends Extension:
  override def interval: Interval = SixthMajorInterval
  override def symbol             = "13"
