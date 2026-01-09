package org.aranadedoros.chordal

import chords.*
import extensions.FlatNinth
import intervals.*
import notes.*

import munit.FunSuite

class ChordSuite extends FunSuite:

  test("create a C major triad") {
    val root  = "C".note
    val third = root.transposeDiatonically(ThirdMajorInterval)
    val fifth = root.transposeDiatonically(FifthPerfectInterval)

    val chord = Triad(root, MajorChord)

    assertEquals(chord.root.toString, "C")
    assertEquals(chord.third.toString, "E")
    assertEquals(chord.fifth.toString, "G")
  }

  test("power chord drops the third") {
    val root = "C".note
    val chord = Triad(
      root,
      MajorTriad
    )

    val power = chord.toPowerChord

    assertEquals(power.root.toString, "C")
    assertEquals(power.fifth.toString, "G")
  }

  test("sus2 replaces the third with a second") {
    val root = "C".note
    val chord = Triad(
      root,
      MajorTriad
    )

    val sus = chord.sus2

    assertEquals(sus.suspendedNote.toString, "D")
    assertEquals(sus.fifth.toString, "G")
  }

  test("add9 keeps triad and adds ninth") {
    val root = "C".note
    val chord = Triad(
      root,
      MajorTriad
    )

    val add = chord.add9

    assertEquals(add.addedNote.toString, "D")
    assertEquals(add.name, "Cadd9")
  }

  test("transpose chord by whole step") {
    val root = "C".note
    val chord = Triad(
      root,
      MajorTriad
    )

    val dChord = chord.transposeBy(SecondMajorInterval)

    assertEquals(dChord.root.toString, "D")
    assertEquals(dChord.third.toString, "F#")
    assertEquals(dChord.fifth.toString, "A")
  }
