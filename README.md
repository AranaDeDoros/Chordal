# Chordal #

Another WIP DSL experiment. A DSL to represent music elements.
Work with Notes to create Chords and then use those same Chords to create Progressions.

### Usage ###
```scala
// let's create a C chord from scratch
val root = "C".note
val third = root.transposeBy(ThirdMajorInterval)
val fifth = root.transposeBy(FifthPerfectInterval)
// let's create the chord
val chord = Triad(root, third, fifth)
println(chord)
// but we're playing rock, let's drop that third and gets us a
val powerChord = chord.toPowerChord
println(powerChord)
// let's add a seventh
val seventhMinor = root.transposeBy(SeventhMinorInterval)
val seventhDom = chord.addExtension(seventhMinor)
println(s"$seventhDom") //will find out how to work with enharmonics later
val susChord = chord.sus2
println(susChord)
val addChord = chord.add9
println(addChord)
println(addChord.addedNote)
println(addChord.render)
println("C#")
println("C#".note.toPitch)
```
### Output ###
```text
(C,E,G)
(C,G)
(C,E,G,A#)
(C,D,G)
(C,E,G,D)
D
Cadd9
(C,D#,G,D)
((C,E,G),(G,B,D),(A,C,E),(F,A,C))
Cmaj7B <- to fix
C#
C
```
## TODO ##
1. [x] Work building progressions (partial regular triad support)
2. [x] Print chords with useful names such as C, Cmaj7, Cm, etc. (m,addN, susN work)
3. [ ] Ponder about the validations of the sharpen and flatten ops. 
4. [x] Somehow work out enharmonics
