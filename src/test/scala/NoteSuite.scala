package org.aranadedoros.chordal

import munit.FunSuite
import notes.*
import intervals.*

class NoteSuite extends FunSuite:

  test("create a note from string") {
    val c = "C".note
    assertEquals(c.toString, "C")
  }

  test("sharpen a note diatonically") {
    val c      = "C".note
    val cSharp = c.sharpenDiatonic
    assertEquals(cSharp.toString, "Db")
  }

  test("transpose note by major third") {
    val c = "C".note
    val e = c.transposeDiatonically(ThirdMajorInterval)
    assertEquals(e.toString, "E")
  }

  test("transpose note by perfect fifth") {
    val c = "C".note
    val g = c.transposeDiatonically(FifthPerfectInterval)
    assertEquals(g.toString, "G")
  }

  test("transpose wraps around octave") {
    val b = "B".note
    val c = b.transposeDiatonically(SecondMinorInterval)
    assertEquals(c.toString, "C")
  }

  test("transpose backwards") {
    val c = "C".note
    val b = c.transposeDiatonically(SecondMinorInterval, backwards = true)
    assertEquals(b.toString, "B")
  }

  test("same pitch class after octave transposition") {
    val c       = "C".note
    val cOctave = c.transposeDiatonically(OctaveInterval)
    assertEquals(c.toPitch, cOctave.toPitch)
  }
