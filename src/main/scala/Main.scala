package org.aranadedoros.chordal

import chords.*
import dsl.*
import extensions.Ninth
import interpreter.RegularChord.chord
import interpreter.Add.add
import notes.note
import interpreter.{AddedChordInterpreter, ChordInterpreter}

import scala.language.postfixOps

object Main:
  def main(args: Array[String]): Unit =
    val C = "C".note
    val CMajor = chord {
      root(C)
      quality(MajorChord)
      withExtensions(Ninth)
    } realize

    println(CMajor.name)
    println(CMajor.notes)

    val add2Desc = add {
      root(C)
      addedNote(Add2)
    }
    val add2Obj = AddedChordInterpreter.interpret(add2Desc)
    println(add2Obj.name)
    println(add2Obj.notes)
