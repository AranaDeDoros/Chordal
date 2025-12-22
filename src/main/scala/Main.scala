package org.aranadedoros.chordal

import notes.{FifthPerfect, SeventhMinor, ThirdMajor, note}
import chords.RegularChord

object Main:
  def main(args: Array[String]): Unit =
    // let's create a C chord from scratch
    val root = "C".note
    val third = root.transposeBy(ThirdMajor)
    val fifth = root.transposeBy(FifthPerfect)
    // let's create the chord
    val chord = RegularChord(root, third, fifth)
    println(chord)
    // but we're playing rock, let's drop that third and gets us a
    val powerChord = chord.toPowerChord
    println(powerChord)
    // let's add a seventh
    val seventhMinor = root.transposeBy(SeventhMinor)
    val seventhDom = chord.addExtension(seventhMinor :: Nil)
    println(s"$seventhDom") //will find out how to work with enharmonics later

