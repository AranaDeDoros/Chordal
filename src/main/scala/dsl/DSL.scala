package org.aranadedoros.chordal
package dsl

import chords.*
import extensions.*
import notes.*

sealed trait ChordDesc:
  def root: Note
  def extensions: List[Extension]

object ChordDesc:

  case class TriadDesc(
    root: Note,
    quality: TriadQuality,
    extensions: List[Extension] = Nil
  ) extends ChordDesc

  case class SuspendedDesc(
    root: Note,
    suspension: Suspension,
    extensions: List[Extension] = Nil
  ) extends ChordDesc

  case class PowerDesc(
    root: Note
  ) extends ChordDesc:
    val extensions: List[Extension] = Nil

  case class AddedDesc(
    base: TriadDesc,
    add: Add,
    extensions: List[Extension] = Nil
  ) extends ChordDesc:
    def root: Note = base.root

def triad(root: Note, quality: TriadQuality) =
  ChordDesc.TriadDesc(root, quality)

def sus(root: Note, s: Suspension) =
  ChordDesc.SuspendedDesc(root, s)

def power(root: Note) =
  ChordDesc.PowerDesc(root)

def add(base: ChordDesc.TriadDesc, add: Add): ChordDesc.AddedDesc =
  ChordDesc.AddedDesc(base, add)

////////////////////////////////////////
// Notes Values
////////////////////////////////////////
object Notes:
  val C = Note("C")
  val D = Note("D")
  val E = Note("E")
  val F = Note("F")
  val G = Note("G")
  val A = Note("A")
  val B = Note("B")

  val Cs = Note("C#")
  val Db = Note("Db")
  val Ds = Note("D#")
  val Eb = Note("Eb")
  val Fs = Note("F#")
  val Gb = Note("Gb")
  val Gs = Note("Gs")
  val Ab = Note("Ab")
  val As = Note("As")
  val Bb = Note("Bb")

extension (c: ChordDesc)
  def withExtensions(exts: Extension*): ChordDesc =
    c match
      case t: ChordDesc.TriadDesc     => t.copy(extensions = exts.toList)
      case s: ChordDesc.SuspendedDesc => s.copy(extensions = exts.toList)
      case a: ChordDesc.AddedDesc     => a.copy(extensions = exts.toList)
      case p: ChordDesc.PowerDesc     => p

final class ChordBuilder:

  private var root: Option[Note]             = None
  private var quality: Option[TriadQuality]  = None
  private var suspension: Option[Suspension] = None
  private var add: Option[Add]               = None
  private var extensions: List[Extension]    = Nil

  // setters (used by DSL syntax)
  def setRoot(n: Note): Unit                   = root = Some(n)
  def setQuality(q: TriadQuality): Unit        = quality = Some(q)
  def setSuspension(s: Suspension): Unit       = suspension = Some(s)
  def setAdd(a: Add): Unit                     = add = Some(a)
  def setExtensions(es: List[Extension]): Unit = extensions = es

  def build(): ChordDesc =
    (root, quality, suspension, add) match

      case (Some(r), Some(q), None, None) =>
        ChordDesc.TriadDesc(r, q, extensions)

      case (Some(r), None, Some(s), None) =>
        ChordDesc.SuspendedDesc(r, s, extensions)

      case (Some(r), Some(q), None, Some(a)) =>
        ChordDesc.AddedDesc(
          ChordDesc.TriadDesc(r, q),
          a,
          extensions
        )

      case (Some(r), None, None, None) =>
        ChordDesc.PowerDesc(r)

      case _ =>
        throw new IllegalStateException("Invalid chord specification")

////////////////////////////////////////
// Entry point
////////////////////////////////////////

def chord(body: ChordBuilder ?=> Unit): ChordDesc =
  given builder: ChordBuilder = new ChordBuilder
  body
  builder.build()

////////////////////////////////////////
// DSL operations
////////////////////////////////////////

def root(n: Note)(using b: ChordBuilder): Unit =
  b.setRoot(n)

def quality(q: TriadQuality)(using b: ChordBuilder): Unit =
  b.setQuality(q)

def sus(s: Suspension)(using b: ChordBuilder): Unit =
  b.setSuspension(s)

def add(a: Add)(using b: ChordBuilder): Unit =
  b.setAdd(a)

def withExtensions(es: Extension*)(using b: ChordBuilder): Unit =
  b.setExtensions(es.toList)
