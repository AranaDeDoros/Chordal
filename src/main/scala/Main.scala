package org.aranadedoros.chordal

import notes.{note, sharpen}
import intervals.*
import chords.*
import progressions.*
import extensions.{FlatNinth, SharpEleventh}

object Main:
  def main(args: Array[String]): Unit =
    // let's create a C chord from scratch
    val root  = "C".note
    val third = root.transposeBy(ThirdMajorInterval)
    val fifth = root.transposeBy(FifthPerfectInterval)
    // let's create the chord
    val chord = Triad(root, MajorChord)
    println(chord)
    // but we're playing rock, let's drop that third and gets us a
    val powerChord = chord.toPowerChord
    println(powerChord)
    // let's add an extension
    val flatNinth = chord.addExtension(FlatNinth)
    println(s"$flatNinth") // will find out how to work with enharmonics later
    val susChord = chord.sus2
    println(susChord)
    val addChord = chord.add9
    println(addChord)
    println(addChord.addedNote)
    println(addChord.render)
    val cRoot      = "C".note
    val thirdM     = root.transposeBy(ThirdMinorInterval)
    val fifthP     = root.transposeBy(FifthPerfectInterval)
    val minorChord = Triad(cRoot, MinorChord)
    val add2       = minorChord.add2
    println(add2)
    val c = Triad.major("C".note)
    val pop =
      Progression.fromDegrees(c, RomanDegree.I, RomanDegree.V, RomanDegree.vi, RomanDegree.IV)
    println(pop)
    val sharpEleventh = c.addExtension(SharpEleventh)
    println(sharpEleventh)
    val seventhMajor = Triad("C".note, SeventhMajorChord)
    println(seventhMajor.render)
    println("C#".note.toPitch)
    println("C".note.sharpen)
