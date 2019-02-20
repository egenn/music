# musicTests.R
# ::music::
# 2019 Efstathios D. Gennatas egenn.github.io

library(music)
is_macos <- length(grep("darwin", sessionInfo()$platform)) == 1

# note2freq ----
freq <- note2freq(note = "A4")

# freq2wave ----
wave <- freq2wave(frequency = freq, plot = TRUE, duration = .1,
                  attack.time = 5,
                  inner.release.time = 5)

# Build ----
buildScale()
buildChord()
buildScale("G4", "minor", plot = TRUE)
buildScale("B4", "minor", descending = TRUE, play = is_macos)
Ab4minor <- buildChord("Ab4", "minor", plot = TRUE, play = is_macos)
A4minor_progression <- buildProgression("A4", "minor", plot = TRUE, play = is_macos)
buildScale("A4", "major")

# Play ----
if (is_macos) {
  playWave(wave, plot = TRUE)
  playChord(Ab4minor, plot = TRUE)
  playFreq(freq, plot = TRUE)
  playProgression(A4minor_progression, plot = TRUE)
  playNote("F5", plot = TRUE)
}

# Note distance ----
noteDistance(strings("C4 Eb4 Gb4 Bb4"))

# All together now ----
mplot(freq2wave(note2freq(buildChord("C4", "minor")),
                oscillator = "sine",
                duration = .05,
                attack.time = 5,
                inner.release.time = 5),
      main = "C4 minor")

# More coverage ----
formatNote(c("D#4", "Ebb"))
mplot(rnorm(200), main = "rnorm")

# formatNotation ----
formatNotation(c("C4", "D4", "E4", "Gb4", "G4", "A4", "B4", "C5"))
formatNotation(c("Db4", "D4", "E4", "Gb4", "G4", "A4", "B4", "C5"))
formatNotation(c("Db4", "E4", "Gb4", "G4", "A4", "B4", "C5"))

# Custom tuning ----
note2freq(buildScale("C4", "major"), tuning = "12ET")
note2freq(buildScale("C4", "major"), tuning = "custom",
          custom.ratios = c(1, 1.05, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 1.95, 2))
note2freq(buildScale("C4", "major"), tuning = "custom",
                   custom.ratios = seq(1, 2, length.out = 13))

buildScale("C5", "major", tuning = "12ET", play = is_macos, pairs = TRUE)
buildScale("C5", "major", tuning = "custom", play = is_macos, pairs = TRUE,
           custom.ratios = seq(1, 2, length.out = 13))
