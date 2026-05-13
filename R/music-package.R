# 2019- EDG rtemis.org

#' \pkg{music}: Learn and use music theory
#'
#' The music package allows you to build, play, and visualize scales, chords,
#' and chord progression.
#' For playback, \pkg{music} builds waveforms as matrices and passes them to
#' the \pkg{audio} package which interfaces with the system's audio driver.
#' The default notation and frequencies used throughout the package are based
#' on twelve-tone equal temperament tuning (12ET). Custom tuning can be defined
#' by specifying frequency ratios and a root  note. See \link{note2freq}.
#' A4 defaults to 440Hz, and can be changed with the 'A4' argument.
#'
#' @name music-package
#' @import graphics utils crayon
"_PACKAGE"

NULL

# %% Deprecated functions ----
#' Deprecated functions
#'
#' @inheritParams build_scale
#' @param formatNotation Logical: If TRUE, format notes to include both flats
#' and sharps to avoid repeating the same letter. e.g. convert c("Gb4", "G4")
#' @rdname music-deprecated
#' @export
buildScale <- function(
  root,
  scale = "minor",
  descending = FALSE,
  plot = TRUE,
  play = FALSE,
  pairs = FALSE,
  formatNotation = TRUE,
  ...
) {
  .Deprecated("build_scale")
  build_scale(
    root = root,
    scale = scale,
    descending = descending,
    plot = plot,
    play = play,
    pairs = pairs,
    format_notation = formatNotation,
    ...
  )
}

#' @inheritParams build_chord
#' @param formatNotation Logical: If TRUE, format notes to include both flats
#' and sharps to avoid repeating the same letter. e.g. convert c("Gb4", "G4") to
#' c("F#4", "G4")
#' @rdname music-deprecated
#' @export
buildChord <- function(
  root,
  chord = "minor",
  plot = TRUE,
  play = FALSE,
  formatNotation = TRUE,
  ...
) {
  .Deprecated("build_chord")
  build_chord(
    root = root,
    chord = chord,
    plot = plot,
    play = play,
    format_notation = formatNotation,
    ...
  )
}

#' @inheritParams build_progression
#' @param formatNotation Logical: If TRUE, format notes to include both flats a
#' nd sharps to avoid repeating the same letter. e.g. convert c("Gb4", "G4") to
#' c("F#4", "G4")
#' @rdname music-deprecated
#' @export
buildProgression <- function(
  root = "A4",
  scale = "minor",
  plot = FALSE,
  play = FALSE,
  formatNotation = TRUE,
  ...
) {
  .Deprecated("build_progression")
  build_progression(
    root = root,
    scale = scale,
    plot = plot,
    play = play,
    format_notation = formatNotation,
    ...
  )
}

#' @rdname music-deprecated
#' @export
formatNote <- function(...) {
  .Deprecated("format_note")
  format_note(...)
}

#' @rdname music-deprecated
#' @export
formatNotation <- function(...) {
  .Deprecated("format_notation")
  format_notation(...)
}

#' @rdname music-deprecated
#' @export
noteDistance <- function(...) {
  .Deprecated("note_distance")
  note_distance(...)
}

#' @rdname music-deprecated
#' @export
playWave <- function(...) {
  .Deprecated("play_wave")
  play_wave(...)
}

#' @rdname music-deprecated
#' @export
playFreq <- function(...) {
  .Deprecated("play_freq")
  play_freq(...)
}

#' @rdname music-deprecated
#' @export
playNote <- function(...) {
  .Deprecated("play_note")
  play_note(...)
}

#' @rdname music-deprecated
#' @export
playChord <- function(...) {
  .Deprecated("play_chord")
  play_chord(...)
}

#' @rdname music-deprecated
#' @export
playProgression <- function(...) {
  .Deprecated("play_progression")
  play_progression(...)
}
