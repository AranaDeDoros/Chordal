package org.aranadedoros.chordal

import munit.FunSuite
import notes.*
import intervals.*
import chords.*

class ChordSuite extends FunSuite:

  test("create a C major triad") {
    val root  = "C".note
    val third = root.transposeBy(ThirdMajorInterval)
    val fifth = root.transposeBy(FifthPerfectInterval)

    val chord = Triad(root, third, fifth)

    assertEquals(chord.root.toString, "C")
    assertEquals(chord.third.toString, "E")
    assertEquals(chord.fifth.toString, "G")
  }

  test("power chord drops the third") {
    val root = "C".note
    val chord = Triad(
      root,
      root.transposeBy(ThirdMajorInterval),
      root.transposeBy(FifthPerfectInterval)
    )

    val power = chord.toPowerChord

    assertEquals(power.root.toString, "C")
    assertEquals(power.fifth.toString, "G")
  }

  test("add minor seventh extension") {
    val root = "C".note
    val chord = Triad(
      root,
      root.transposeBy(ThirdMajorInterval),
      root.transposeBy(FifthPerfectInterval)
    )

    val seventh  = root.transposeBy(SeventhMinorInterval)
    val extended = chord.addExtension(seventh)

    assertEquals(extended.extensions.size, 1)
    assertEquals(extended.extensions.head.toString, "A#") // will change once enharmonics are added
  }

  test("sus2 replaces the third with a second") {
    val root = "C".note
    val chord = Triad(
      root,
      root.transposeBy(ThirdMajorInterval),
      root.transposeBy(FifthPerfectInterval)
    )

    val sus = chord.sus2

    assertEquals(sus.suspendedNote.toString, "D")
    assertEquals(sus.fifth.toString, "G")
  }

  test("add9 keeps triad and adds ninth") {
    val root = "C".note
    val chord = Triad(
      root,
      root.transposeBy(ThirdMajorInterval),
      root.transposeBy(FifthPerfectInterval)
    )

    val add = chord.add9

    assertEquals(add.addedNote.toString, "D")
    assertEquals(add.render, "Cadd9")
  }

  test("transpose chord by whole step") {
    val root = "C".note
    val chord = Triad(
      root,
      root.transposeBy(ThirdMajorInterval),
      root.transposeBy(FifthPerfectInterval)
    )

    val dChord = chord.transposeBy(SecondMajorInterval)

    assertEquals(dChord.root.toString, "D")
    assertEquals(dChord.third.toString, "F#")
    assertEquals(dChord.fifth.toString, "A")
  }
