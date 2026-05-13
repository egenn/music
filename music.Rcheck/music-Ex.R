pkgname <- "music"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('music')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("build_chord")
### * build_chord

flush(stderr()); flush(stdout())

### Name: build_chord
### Title: Build Chord
### Aliases: build_chord

### ** Examples

build_chord("C4", "minor")
build_chord("A4", "sus2", plot = TRUE)
## Not run: 
##D build_chord("B4", "sus2", play = TRUE)
## End(Not run)



cleanEx()
nameEx("build_progression")
### * build_progression

flush(stderr()); flush(stdout())

### Name: build_progression
### Title: Build Chord Progression
### Aliases: build_progression

### ** Examples

build_progression("C4", "minor")
build_progression("Bb4", "major")
## Not run: 
##D build_progression("Bb4", "major", play = TRUE, plot = TRUE)
## End(Not run)



cleanEx()
nameEx("build_scale")
### * build_scale

flush(stderr()); flush(stdout())

### Name: build_scale
### Title: Build Scale
### Aliases: build_scale

### ** Examples

build_scale("C4", "minor")
build_scale("B4", "minor", descending = TRUE, plot = TRUE)
## Not run: 
##D build_scale("B4", "minor", descending = TRUE, play = TRUE, plot = TRUE)
## End(Not run)



cleanEx()
nameEx("cplot_piano")
### * cplot_piano

flush(stderr()); flush(stdout())

### Name: cplot_piano
### Title: Console piano plot for notes
### Aliases: cplot_piano

### ** Examples

cplot_piano(build_scale("B4", "minor"))



cleanEx()
nameEx("delay_time")
### * delay_time

flush(stderr()); flush(stdout())

### Name: delay_time
### Title: Delay and Reverb Time Calculator
### Aliases: delay_time

### ** Examples

delay_time(120, "1/8")



cleanEx()
nameEx("format_notation")
### * format_notation

flush(stderr()); flush(stdout())

### Name: format_notation
### Title: Format Notation
### Aliases: format_notation

### ** Examples

format_notation(c("Db4", "D4", "E4", "Gb4", "G4", "A4", "B4", "C5"))



cleanEx()
nameEx("format_note")
### * format_note

flush(stderr()); flush(stdout())

### Name: format_note
### Title: Format notes
### Aliases: format_note

### ** Examples

format_note(c("D#4", "Ebb"))



cleanEx()
nameEx("freq2wave")
### * freq2wave

flush(stderr()); flush(stdout())

### Name: freq2wave
### Title: Frequency to waveform
### Aliases: freq2wave

### ** Examples

wave <- freq2wave(note2freq(build_chord("A4", "sus2")))



cleanEx()
nameEx("note2freq")
### * note2freq

flush(stderr()); flush(stdout())

### Name: note2freq
### Title: Convert musical notes to frequencies
### Aliases: note2freq

### ** Examples

note2freq(build_scale("B4", "minor"))



cleanEx()
nameEx("note_distance")
### * note_distance

flush(stderr()); flush(stdout())

### Name: note_distance
### Title: Note distance in semitones
### Aliases: note_distance

### ** Examples

note_distance(strings("C4 Eb4 Gb4 Bb4"))



cleanEx()
nameEx("play_chord")
### * play_chord

flush(stderr()); flush(stdout())

### Name: play_chord
### Title: Play Chord
### Aliases: play_chord

### ** Examples

## Not run: 
##D play_chord(build_chord("E4", "minor"))
## End(Not run)



cleanEx()
nameEx("play_freq")
### * play_freq

flush(stderr()); flush(stdout())

### Name: play_freq
### Title: Play frequency
### Aliases: play_freq

### ** Examples

## Not run: 
##D play_freq(440)
## End(Not run)



cleanEx()
nameEx("play_note")
### * play_note

flush(stderr()); flush(stdout())

### Name: play_note
### Title: Play Note
### Aliases: play_note

### ** Examples

## Not run: 
##D play_note("B4")
## End(Not run)



cleanEx()
nameEx("play_progression")
### * play_progression

flush(stderr()); flush(stdout())

### Name: play_progression
### Title: Play Progression
### Aliases: play_progression

### ** Examples

## Not run: 
##D play_progression(build_progression("G4", "minor"))
## End(Not run)



cleanEx()
nameEx("play_wave")
### * play_wave

flush(stderr()); flush(stdout())

### Name: play_wave
### Title: Minimal "Polyphonic" Wave Player
### Aliases: play_wave

### ** Examples

## Not run: 
##D play_wave(freq2wave(440))
## End(Not run)



cleanEx()
nameEx("strings")
### * strings

flush(stderr()); flush(stdout())

### Name: strings
### Title: Separate notes into vector of strings
### Aliases: strings

### ** Examples

strings("C4 Eb4 Gb4 Bb4")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
