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

val c1 =
  chord {
    root(C)
    quality(MajorTriad)
    withExtensions(Ninth)
  }

println(c1)

val c2 =
  chord {
    root("D".note)
    sus(Sus4)
  }
println(c2)

val c3 =
  chord {
    root("E".note)
  }
println(c3)
```
### Output ###
```text
C
Dm
Eb°
(C,E,G)
(D,F,A)
Csus2
(C,D,G)
(C,F,G)
Cadd9
(C,E,G,D)
(C,E,G,A)
(C5)
(C,G)
Cmaj7
(C,E,G,B)
Dm7
(D,F,A,C)
Cmaj79
(C,E,G,B,D)
List(Dm7, G7, Cmaj7)
TriadDesc(C,MajorTriad,List(Ninth))
SuspendedDesc(D,sus4,List())
PowerDesc(E)
```
## TODO ##
1. [x] Improve syntax to read like english.
2. [x] Fix naming issues (fixed for the currently supported types).
3. [ ] Print chord intervalic formulas.
4. [x] Model tetrachords better.
