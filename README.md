# Chordal #

<img width="800" height="533" alt="chordallogo" src="https://github.com/user-attachments/assets/fc0c7cb3-bdc3-48aa-a149-e318278edd76" />


Another WIP DSL experiment to represent music elements. Work with Notes to create Chords and then use those same Chords to create Progressions.

### Features ###
 - Working enharmonics.
 - Extension methods to work with strings as notes.
 - Useful rendering of notes.
 - Harmony
   - Chord creation
     - Triads.
     - Tetrachords (subject to model changes, partial support).
     - Add chords.
     - Power chords.
     - Suspended chords.
     - Partial extension support.
   - Progressions
     - Generic list-like implementation.
     - Quick default methods to create common progressions.

## Usage ##
### DSL
```scala
//on progress, subject to changes
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

```
```text
C9
(C,E,G,D)
Cadd2
(C,D)
```
### Procedural
```scala
// let's create a C chord from scratch
val root  = "C".note
val third = root.transposeDiatonically(ThirdMajorInterval)
val fifth = root.transposeDiatonically(FifthPerfectInterval)

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
val thirdM     = root.transposeDiatonically(ThirdMinorInterval)
val fifthP     = root.transposeDiatonically(FifthPerfectInterval)
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

```
### Output ###
```text
(C,E,G)
(C,G)
(C,E,G,C#)
(C,D,G)
(C,D)
D
Cadd9
(C,D)
((C,E,G),(G,B,D),(A,C,E),(F,A,C))
(C,E,G,F#)
Cmaj7
C
C#
```
## TODO ##
1. [x] Work building progressions (partial regular triad support)
2. [x] Print chords with useful names such as C, Cmaj7, Cm, etc.
3. [x] Ponder about the validations of the sharpen and flatten ops. 
4. [x] Somehow work out enharmonics.
5. [ ] Model tetrachords better.
6. [ ] Print chord intervalic formulas.
7. [ ] Improve syntax to read like english.
