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
      MajorChord
    )

    val power = chord.toPowerChord

    assertEquals(power.root.toString, "C")
    assertEquals(power.fifth.toString, "G")
  }

  test("add minor seventh extension") {
    val root = "C".note
    val chord = Triad(
      root,
      SeventhMinorChord
    )

    val seventh = root.transposeDiatonically(SeventhMinorInterval)
    println(chord)
    val extended = chord.addExtension(FlatNinth)
    println(extended.extensionNotes)
    assertEquals(extended.extensions.size, 1)
    val extensionFound = extended.extensionNotes.find(
      p => p.toString == "Db"
    )
    val ninthFound = extensionFound match
      case Some(n) => true
      case None    => false
    assert(ninthFound, "ninth was found")

  }

  test("sus2 replaces the third with a second") {
    val root = "C".note
    val chord = Triad(
      root,
      MajorChord
    )

    val sus = chord.sus2

    assertEquals(sus.suspendedNote.toString, "D")
    assertEquals(sus.fifth.toString, "G")
  }

  test("add9 keeps triad and adds ninth") {
    val root = "C".note
    val chord = Triad(
      root,
      MajorChord
    )

    val add = chord.add9

    assertEquals(add.addedNote.toString, "D")
    assertEquals(add.render, "Cadd9")
  }

  test("transpose chord by whole step") {
    val root = "C".note
    val chord = Triad(
      root,
      MajorChord
    )

    val dChord = chord.transposeBy(SecondMajorInterval)

    assertEquals(dChord.root.toString, "D")
    assertEquals(dChord.third.toString, "F#")
    assertEquals(dChord.fifth.toString, "A")
  }
