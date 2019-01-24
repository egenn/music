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
