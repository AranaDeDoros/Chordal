package org.aranadedoros.chordal
package interpreter

import chords.*
import dsl.*
import extensions.Extension
import notes.Note

sealed trait ChordDesc

object ChordDesc:

  case class TriadDesc(
    root: Note,
    quality: TriadQuality,
    extensions: List[Extension]
  ) extends ChordDesc

  case class SuspendedDesc(
    root: Note,
    suspension: Suspension,
    extensions: List[Extension]
  ) extends ChordDesc

  case class AddedDesc(
    base: TriadDesc,
    add: Add,
    extensions: List[Extension]
  ) extends ChordDesc

  case class PowerDesc(
    root: Note
  ) extends ChordDesc

object ChordInterpreter:

  private def triad(desc: ChordDesc.TriadDesc): Triad =
    Triad(desc.root, desc.quality)

  def interpret(desc: ChordDesc): Chord =
    desc match

      case t: ChordDesc.TriadDesc =>
        triad(t)

      case ChordDesc.SuspendedDesc(root, suspension, _) =>
        SuspendedChord(root, suspension)

      case ChordDesc.AddedDesc(base, add, _) =>
        AddChord(triad(base), add)

      case ChordDesc.PowerDesc(root) =>
        PowerChord(root)
