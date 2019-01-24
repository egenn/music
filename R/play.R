# play.R
# ::music::
# 2019 Efstathios D. Gennatas


#' "Polyphonic" Wave Player
#'
#' Play one or more waveforms at the same time using \code{audio::play}
#'
#' @param wave Matrix or vector of waveforms. If a matrix, each column should be a waveform to be played
#' simultaneously
#' @param sample.rate Integer: Sample rate. Default = 44100
#' @param plot Logical: If TRUE: plot wave using \link{mplot}.
#'
#' @export
#' @author Efstathios D. Gennatas

playWave <- function(wave,
                     sample.rate = 44100,
                     plot = FALSE) {
  if (is.null(dim(wave))) wave <- matrix(wave, ncol = 1)
  n.notes <- NCOL(wave)
  for (i in seq(n.notes)) {
    audio::play(wave[, i], rate = sample.rate)
  }

  if (plot) mplot(wave)

} # music::playWave


#' Play frequency
#'
#' @param frequency Numeric, Vector: Frequency / frequencies to play
#' @param oscillator String: "sine", "square", "saw". Default = "sine"
#' @param duration Float: Note duration in beats. Default = 1
#' @param BPM Integer: Beats per minute. Default = 120
#' @param sample.rate Integer: Sample rate. Default = 44100
#' @param attack.time Integer: Attack time. Default = 50 (Helps prevent popping)
#' @param inner.release.time Integer: Release time, that ends on note OFF (instead of beginning at note OFF).
#' Deefault = 50 (Also helps prevent popping)
#' @param plot Logical: If TRUE, plot waveform
#' @export
#' @author Efstathios D. Gennatas

playFreq <- function(frequency,
                     oscillator = "sine",
                     duration = rep(1, length(frequency)),
                     BPM = 120,
                     sample.rate = 44100,
                     attack.time = 50,
                     inner.release.time = 50,
                     plot = FALSE) {

  wave <- c(mapply(freq2wave,
                   frequency,
                   oscillator,
                   duration,
                   BPM,
                   sample.rate,
                   attack.time,
                   inner.release.time))
  # if (plot) rtemis::mplot3.x(wave, type = 'l', pty = 'm', ylab = "Amplitude")
  if (plot) mplot(wave)
  playWave(wave)

} # music::playFreq

#' Play Note
#'
#' @inheritParams playFreq
#' @param note String, Vector: Note(s) to be played, e.g. c("Ab4", "B4")
#' @param plot Logical: If TRUE, plot notes using \link{cplot.piano}. This support only two octaves;
#' do not try plotting if your notes span more than two octaves.
#' @export
#' @author Efstathios D. Gennatas

playNote <- function(note,
                     oscillator = "sine",
                     duration = rep(1, length(note)),
                     BPM = 120,
                     sample.rate = 44100,
                     attack.time = 50,
                     inner.release.time = 50,
                     plot = FALSE) {

  freqs <- note2freq(note)

  if (plot) cplot.piano(note)

  playFreq(freqs,
           oscillator = oscillator,
           duration = duration,
           BPM = BPM,
           sample.rate = sample.rate,
           attack.time = attack.time,
           inner.release.time = inner.release.time)

} # music::playNote


#' Play Chord
#'
#' @inheritParams playFreq
#' @param chord String, vector: Notes making up chord. e.g. c("A4", "C5", "E5").
#' e.g. output of \link{buildChord}
#' @param type String: "harmonic", "ascending", "descending". Default = "harmonic"
#' @param plot Logical: If TRUE, plot chord using \link{cplot.piano}
#' @export
#' @return The constructed waveform (invisibly)
#' @author Efstathios D. Gennatas

playChord <- function(chord,
                      type = c("harmonic", "ascending", "descending"),
                      oscillator = "sine",
                      duration = 1,
                      sample.rate = 44100,
                      attack.time = 50,
                      inner.release.time = 50,
                      plot = FALSE) {

  type <- match.arg(type)

  wave <- lapply(chord, function(i) freq2wave(note2freq(i),
                                              oscillator = oscillator,
                                              duration = duration,
                                              sample.rate = sample.rate,
                                              attack.time = attack.time,
                                              inner.release.time = inner.release.time))

  wave <- switch(type,
                 harmonic = do.call(cbind, wave),
                 ascending = do.call(c, wave),
                 descending = do.call(c, rev(wave)))

  if (plot) cplot.piano(chord)
  playWave(wave)
  invisible(wave)

} # music::playChord


#' Play Progression
#'
#' @inheritParams playFreq
#' @param progression List of string vectors: Each element of the list is a chord.
#' e.g. output of \link{buildProgression}
#' @param plot Logical. If TRUE, plot each chord in the progression using \link{cplot.piano}
#' @export
#' @author Efstathios D. Gennatas

playProgression <- function(progression,
                            oscillator = c("sine", "square", "saw", "triangle"),
                            duration = 1,
                            BPM = 120,
                            sample.rate = 44100,
                            attack.time = 50,
                            inner.release.time = 50,
                            plot = FALSE) {

  oscillator <- match.arg(oscillator)
  wave <- lapply(progression, function(i)
    do.call(cbind,
            lapply(i, function(j) freq2wave(note2freq(j),
                                            oscillator = oscillator,
                                            duration = duration,
                                            BPM = BPM,
                                            sample.rate = sample.rate,
                                            attack.time = attack.time,
                                            inner.release.time = inner.release.time))))
  wave <- do.call(rbind, wave)
  playWave(wave)
  if (plot) {
    for (i in seq(progression)) {
      cplot.piano(progression[[i]])
      cat("\n")
    }
  }

} # playProgression
