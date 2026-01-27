package org.aranadedoros.chordal

import chords.*
import dsl.*
import dsl.Notes.{D, E}
import extensions.Ninth
import notes.{major, note, Note}

import scala.language.postfixOps

object Main:
  def main(args: Array[String]): Unit =

    val C  = Note("C")
    val D  = Note("D")
    val Eb = Note("Eb")

    val cMajor = Triad.major(C)
    val dMinor = Triad.minor(D)
    val ebDim  = Triad.diminished(Eb)

    println(cMajor.name) // "C"
    println(dMinor.name) // "Dm"
    println(ebDim.name)  // "E°"

    println(cMajor.asNotes) // List(C, E, G)
    println(dMinor.asNotes) // List(D, F, A)

    val csus2 = cMajor.sus2
    val csus4 = cMajor.sus4

    println(csus2.name)    // "C D"
    println(csus2.asNotes) // List(C, D, G)

    println(csus4.asNotes) // List(C, F, G)

    val cAdd9 = cMajor.add9
    val cAdd6 = cMajor.add6

    println(cAdd9.name)    // "Cadd9"
    println(cAdd9.asNotes) // List(C, E, G, D)

    println(cAdd6.asNotes) // List(C, E, G, A)

    val c5 = cMajor.toPowerChord

    println(c5.name)    // "(C5)"
    println(c5.asNotes) // List(C, G)

    val cMaj7 = cMajor.withSeventh(MajorSeventh)
    val d7    = dMinor.withSeventh(MinorSeventh)

    println(cMaj7.name)    // "Cmaj7"
    println(cMaj7.asNotes) // List(C, E, G, B)

    println(d7.name)    // "Dm7"
    println(d7.asNotes) // List(D, F, A, C)

    val cMaj9 =
      cMajor
        .withSeventh(MajorSeventh)
        .addExtension(Ninth)

    println(cMaj9.name)    // "Cmaj79"
    println(cMaj9.asNotes) // List(C, E, G, B, D)

    val key = Triad.major("C".note)

    val ii = Triad.minor("D".note)
    val V  = Triad.major("G".note)
    val I  = Triad.major("C".note)

    val ii7   = ii.withSeventh(MinorSeventh)
    val V7    = V.withSeventh(MinorSeventh)
    val Imaj7 = I.withSeventh(MajorSeventh)

    println(List(ii7, V7, Imaj7).map(_.name))

    val chord1 =
      chord {
        root(C)
        quality(MajorTriad)
        withExtensions(Ninth)
      }

    println(chord1)

    val chord2 =
      chord {
        root("D".note)
        sus(Sus4)
      }
    println(chord2)

    val chord3 =
      chord {
        root(E)
      }
    println(chord3)

    val dMajor: Triad =
      D major
    println(dMajor)
