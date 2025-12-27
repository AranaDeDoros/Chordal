package org.aranadedoros.chordal

import chords.*
import dsl.*
import extensions.Ninth
import interpreter.RegularChord.chord
import interpreter.{Add, AddedChordInterpreter, ChordInterpreter}
import notes.note

object Main:
  def main(args: Array[String]): Unit =
    val C = "C".note
    val chordDesc = chord {
      root(C)
      quality(MajorChord)
      withExtensions(Ninth)
    }
    val chordObj = ChordInterpreter.interpret(chordDesc)
    println(chordObj.render)
    println(chordObj.toString)

    val add2Desc = Add.chord {
      root(C)
      addedNote(Add2)
    }
    val add2Obj = AddedChordInterpreter.interpret(add2Desc)
    println(add2Obj.render)
    println(add2Obj.toString)
