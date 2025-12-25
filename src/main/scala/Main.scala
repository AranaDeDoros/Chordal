package org.aranadedoros.chordal

import notes.note
import intervals.*
import chords.{RegularChord, add9, sus2}

object Main:
  def main(args: Array[String]): Unit =
    // let's create a C chord from scratch
    val root = "C".note
    val third = root.transposeBy(ThirdMajorInterval)
    val fifth = root.transposeBy(FifthPerfectInterval)
    // let's create the chord
    val chord = RegularChord(root, third, fifth)
    println(chord)
    // but we're playing rock, let's drop that third and gets us a
    val powerChord = chord.toPowerChord
    println(powerChord)
    // let's add a seventh
    val seventhMinor = root.transposeBy(SeventhMinorInterval)
    val seventhDom = chord.addExtension(seventhMinor)
    println(s"$seventhDom") //will find out how to work with enharmonics later
    val susChord = chord.sus2
    println(susChord)
    val addChord = chord.add9
    println(addChord)
    println(addChord.addedNote)
    println(addChord.render)

