package org.aranadedoros.chordal

import munit.FunSuite
import notes.note
import chords.*
import progressions.*

class RomanDegreeSuite extends FunSuite:

  test("I–V–vi–IV in C major") {
    val tonic = Triad.major("C".note)

    val progression =
      Progression.fromDegrees(
        tonic,
        RomanDegree.I,
        RomanDegree.V,
        RomanDegree.vi,
        RomanDegree.IV
      )

    val rendered = progression.chords.map(_.name)

    assertEquals(
      rendered,
      List("C", "G", "Am", "F")
    )
  }

  test("ii–V–I in C major") {
    val tonic = Triad.major("C".note)

    val progression =
      Progression.fromDegrees(
        tonic,
        RomanDegree.ii,
        RomanDegree.V,
        RomanDegree.I
      )

    val rendered = progression.chords.map(_.name)

    assertEquals(
      rendered,
      List("Dm", "G", "C")
    )
  }
