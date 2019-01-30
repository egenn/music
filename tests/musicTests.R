# musicTests.R
# ::music::
# 2019 Efstathios D. Gennatas

library(music)

# note2freq
freq <- note2freq(note = "A4")

# freq2wave
wave <- freq2wave(frequency = freq, plot = TRUE, duration = .4)

# Build
buildChord("Ab4", "minor", plot = TRUE)
buildScale("G4", "minor", plot = TRUE)
buildProgression("A4", "minor")

# Note distance
noteDistance(strings("C4 Eb4 Gb4 Bb4"))

# All together now
mplot(freq2wave(note2freq(buildChord("C4", "minor")),
                oscillator = "square",
                duration = .3,
                attack.time = 10,
                inner.release.time = 10))
