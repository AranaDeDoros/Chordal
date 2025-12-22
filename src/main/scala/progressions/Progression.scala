package org.aranadedoros.chordal
package progressions

import chords.Chord


/* progression */
  case class Progression(chords: List[Chord]):
    def +(chord: Chord): List[Chord] =
      chords :+ chord

    def -(chord: Chord, dropAll: Boolean = false): List[Chord] =
      if (dropAll) {
        chords.filterNot(_ == chord)
      } else {
        val (left, right) = chords.span(_ != chord)
        val newList = left ::: right.drop(1)
        newList
      }

    override def toString: String =
      chords.mkString("(", ",", ")")
