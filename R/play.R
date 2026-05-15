# play.R
# ::music::
# 2019- EDG rtemis.org

#' Minimal "Polyphonic" Wave Player
#'
#' Play one or more waveforms at the same time using \code{audio::play}
#'
#' @param wave Matrix or vector of waveforms. If a matrix, each column should
#' be a waveform to be played simultaneously
#' @param sample.rate Integer: Sample rate. Default = 44100
#' @param plot Logical: If TRUE: plot wave using \link{mplot}.

#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#'   play_wave(freq2wave(440))
#' }
play_wave <- function(wave, sample.rate = 44100, plot = FALSE) {
  if (is.null(dim(wave))) {
    wave <- matrix(wave, ncol = 1)
  }
  n.notes <- NCOL(wave)

  # Plot
  if (plot) {
    mplot(wave)
  }

  # Combine waves
  if (n.notes > 1) {
    wave <- rowMeans(wave)
  }

  audio::play(wave, rate = sample.rate)
} # music::play_wave


#' Play frequency
#'
#' @param frequency Numeric, Vector: Frequency / frequencies to play
#' @param oscillator String: "sine", "square", "saw". Default = "sine"
#' @param duration Float: Note duration in beats. Default = 1
#' @param BPM Integer: Beats per minute. Default = 120
#' @param sample.rate Integer: Sample rate. Default = 44100
#' @param attack.time Integer: Attack time. Default = 50 (Helps prevent popping)
#' @param inner.release.time Integer: Release time, that ends on note OFF
#' (instead of beginning at note OFF). Default = 50 (Also helps prevent popping)
#' @param plot Logical: If TRUE, plot waveform
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#'   play_freq(440)
#' }
play_freq <- function(
  frequency,
  oscillator = "sine",
  duration = rep(1, length(frequency)),
  BPM = 120,
  sample.rate = 44100,
  attack.time = 50,
  inner.release.time = 50,
  plot = FALSE
) {
  wave <- unlist(c(mapply(
    freq2wave,
    frequency,
    oscillator,
    duration,
    BPM,
    sample.rate,
    attack.time,
    inner.release.time
  )))

  if (plot) {
    mplot(wave)
  }
  play_wave(wave)
} # music::play_freq

#' Play Note
#'
#' @inheritParams play_freq
#' @inheritParams note2freq
#' @param note String, Vector: Note(s) to be played, e.g. c("Ab4", "B4")
#' @param plot Logical: If TRUE, plot notes using \link{cplot_piano}. This
#' supports only two octaves; do not try plotting if your notes span more than
#' two octaves.
#' @param ... Additional arguments to pass to \link{note2freq}
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#'   play_note("B4")
#' }
play_note <- function(
  note,
  oscillator = "sine",
  duration = rep(1, length(note)),
  BPM = 120,
  sample.rate = 44100,
  attack.time = 50,
  inner.release.time = 50,
  A4 = 440,
  plot = FALSE,
  ...
) {
  freqs <- note2freq(note, A4 = A4, ...)

  if (plot) {
    cplot_piano(note)
  }

  play_freq(
    freqs,
    oscillator = oscillator,
    duration = duration,
    BPM = BPM,
    sample.rate = sample.rate,
    attack.time = attack.time,
    inner.release.time = inner.release.time
  )
} # music::play_note


#' Play Chord
#'
#' @inheritParams play_note
#' @param chord String, vector: Notes making up chord. e.g. c("A4", "C5", "E5").
#' e.g. output of \link{build_chord}
#' @param type String: "harmonic", "ascending", "descending".
#' Default = "harmonic"
#' @param plot Logical: If TRUE, plot chord using \link{cplot_piano}
#'
#' @return The constructed waveform (invisibly)
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#'   play_chord(build_chord("E4", "minor"))
#' }
play_chord <- function(
  chord,
  type = c("harmonic", "ascending", "descending"),
  oscillator = "sine",
  duration = 1,
  sample.rate = 44100,
  attack.time = 50,
  inner.release.time = 50,
  A4 = 440,
  plot = FALSE,
  ...
) {
  type <- match.arg(type)

  wave <- lapply(chord, function(i) {
    freq2wave(
      note2freq(i, A4 = A4, ...),
      oscillator = oscillator,
      duration = duration,
      sample.rate = sample.rate,
      attack.time = attack.time,
      inner.release.time = inner.release.time
    )
  })

  wave <- switch(
    type,
    harmonic = do.call(cbind, wave),
    ascending = do.call(c, wave),
    descending = do.call(c, rev(wave))
  )

  if (plot) {
    cplot_piano(chord)
  }
  play_wave(wave)
  invisible(wave)
} # music::play_chord


#' Play Progression
#'
#' @inheritParams play_note
#' @param progression List of string vectors: Each element of the list is a
#' chord. e.g. output of \link{build_progression}
#' @param plot Logical. If TRUE, plot each chord in the progression using
#' \link{cplot_piano}
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#'   play_progression(build_progression("G4", "minor"))
#' }
play_progression <- function(
  progression,
  oscillator = c("sine", "square", "saw", "triangle"),
  duration = 1,
  BPM = 120,
  sample.rate = 44100,
  attack.time = 50,
  inner.release.time = 50,
  A4 = 440,
  plot = FALSE,
  ...
) {
  oscillator <- match.arg(oscillator)
  wave <- lapply(progression, function(i) {
    do.call(
      cbind,
      lapply(i, function(j) {
        freq2wave(
          note2freq(j, A4 = A4, ...),
          oscillator = oscillator,
          duration = duration,
          BPM = BPM,
          sample.rate = sample.rate,
          attack.time = attack.time,
          inner.release.time = inner.release.time
        )
      })
    )
  })
  wave <- do.call(rbind, wave)
  play_wave(wave)
  if (plot) {
    for (i in seq(progression)) {
      cplot_piano(progression[[i]])
      cat("\n")
    }
  }
} # play_progression
