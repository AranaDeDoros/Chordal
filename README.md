# Chordal #

Another WIP DSL experiment. A dsl to represent music elements.
Work with Notes to create Chords and then use those same chords to create Progressions.

### Usage ###
```scala
    // let's create a C chord from scratch
    val root = "C".note
    val third = root.transposeBy(ThirdMajor)
    val fifth = root.transposeBy(FifthPerfect)
    // let's create the chord
    val chord = RegularChord(root, third, fifth)
    println(chord)
    // but we're playing rock, let's drop that third and gets us a
    val powerChord = chord.toPowerChord
    println(powerChord)
    // let's add a seventh
    val seventhMinor = root.transposeBy(SeventhMinor)
    val seventhDom = chord.addExtension(seventhMinor :: Nil)
    println(s"$seventhDom") //will find out how to work with enharmonics later
```
### Output ###
```text
(C, E, G )
(C, G)
(C, E, G A#) <- working on this
```
## TODO in order ##
1. [ ] Work building progressions
2. [ ] Print chords with their quality such as C, Cmaj7, Cmin, etc.
3. [ ] Represent addN chords.
4. [ ] Ponder about the validations of the sharpen and flatten ops. 
5. [ ] Somehow work out enharmonics.
