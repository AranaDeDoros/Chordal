package org.aranadedoros.chordal
package interpreter

import dsl.*
import chords.*

object RegularChord:
  def chord(body: ChordSpec ?=> Unit): ChordDescription =
    given spec: ChordSpec = new ChordSpec
    body
    ChordDescription(
      root = spec.root.getOrElse(throw new IllegalStateException("root missing")),
      quality = spec.quality.getOrElse(throw new IllegalStateException("quality missing")),
      extensions = spec.extensions
    )

object ChordInterpreter:
  def interpret(desc: ChordDescription): Triad =
    Triad(desc.root, desc.quality).addExtensions(desc.extensions)

object Suspended:
  def chord(body: SuspendedChordSpec ?=> Unit): SuspendedChordDescription =
    given spec: SuspendedChordSpec = new SuspendedChordSpec
    body
    SuspendedChordDescription(
      root = spec.root.getOrElse(
        throw new IllegalStateException("Chord root not specified")
      ),
      suspension = spec.suspension.getOrElse(
        throw new IllegalStateException("Chord suspension not specified")
      ),
      extensions = spec.extensions
    )

object SuspendedChordInterpreter:
  def interpret(desc: SuspendedChordDescription): SuspendedChord =
    SuspendedChord(root = desc.root, suspension = desc.suspension, extensions = Nil)

object Add:
  def chord(body: AddedChordSpec ?=> Unit): AddedChordDescription =
    given spec: AddedChordSpec = new AddedChordSpec
    body
    AddedChordDescription(
      root = spec.root.getOrElse(
        throw new IllegalStateException("Chord root not specified")
      ),
      addedNote = spec.addedNote.getOrElse(
        throw new IllegalStateException("Chord add not specified")
      ),
      extensions = spec.extensions
    )

object AddedChordInterpreter:
  def interpret(desc: AddedChordDescription): AddChord =
    AddChord(root = desc.root, add = desc.addedNote)

object Power:
  def chord(body: PowerChordSpec ?=> Unit): PowerChordDescription =
    given spec: PowerChordSpec = new PowerChordSpec
    body
    PowerChordDescription(
      root = spec.root.getOrElse(
        throw new IllegalStateException("Chord root not specified")
      )
    )

object PowerChordInterpreter:
  def interpret(desc: PowerChordDescription): PowerChord =
    PowerChord(
      root = desc.root
    )
