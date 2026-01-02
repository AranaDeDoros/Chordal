package org.aranadedoros.chordal
package dsl

import notes.*
import extensions.*
import chords.*

import org.aranadedoros.chordal.interpreter.ChordInterpreter
////////////////////////////////////////
// Base
////////////////////////////////////////

trait BasicSpec

////////////////////////////////////////
// Capabilities
////////////////////////////////////////

trait HasRoot:
  var root: Option[Note]

trait HasQuality:
  var quality: Option[ChordQuality]

trait HasExtensions:
  var extensions: List[Extension]

trait HasSuspension:
  var suspension: Option[Suspension]

trait HasAddedNote:
  var addedNote: Option[Add]

////////////////////////////////////////
// DSL OPERATIONS
////////////////////////////////////////

def root(note: Note)(using spec: HasRoot): Unit =
  spec.root = Some(note)

def quality(q: ChordQuality)(using spec: HasQuality): Unit =
  spec.quality = Some(q)

def withExtensions(exts: Extension*)(using spec: HasExtensions): Unit =
  spec.extensions = exts.toList

def suspension(s: Suspension)(using spec: HasSuspension): Unit =
  spec.suspension = Some(s)

def addedNote(a: Add)(using spec: HasAddedNote): Unit =
  spec.addedNote = Some(a)

////////////////////////////////////////
// TRIAD / REGULAR CHORD
////////////////////////////////////////

final case class ChordDescription(
  root: Note,
  quality: ChordQuality,
  extensions: List[Extension]
)
extension (c: ChordDescription)
  def realize: Triad = ChordInterpreter.interpret(c)

final class ChordSpec
    extends BasicSpec
    with HasRoot
    with HasQuality
    with HasExtensions:

  var root: Option[Note]            = None
  var quality: Option[ChordQuality] = None
  var extensions: List[Extension]   = Nil

////////////////////////////////////////
// SUSPENDED
////////////////////////////////////////

final case class SuspendedChordDescription(
  root: Note,
  suspension: Suspension,
  extensions: List[Extension]
)

final class SuspendedChordSpec
    extends BasicSpec
    with HasRoot
    with HasSuspension
    with HasExtensions:

  var root: Option[Note]             = None
  var suspension: Option[Suspension] = None
  var extensions: List[Extension]    = Nil

////////////////////////////////////////
// ADDED
////////////////////////////////////////

final case class AddedChordDescription(
  root: Note,
  addedNote: Add,
  extensions: List[Extension]
)

final class AddedChordSpec
    extends BasicSpec
    with HasRoot
    with HasAddedNote
    with HasExtensions:

  var root: Option[Note]          = None
  var addedNote: Option[Add]      = None
  var extensions: List[Extension] = Nil

////////////////////////////////////////
// POWER
////////////////////////////////////////

final case class PowerChordDescription(
  root: Note
)

final class PowerChordSpec
    extends BasicSpec
    with HasRoot:

  var root: Option[Note] = None
