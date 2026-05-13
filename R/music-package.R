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
#' @param ... Arguments to be passed to the new function
#' @rdname myPackage-deprecated
#' @export
buildScale <- function(...) {
  .Deprecated("build_scale")
  build_scale(...)
}

#' @rdname myPackage-deprecated
#' @export
buildChord <- function(...) {
  .Deprecated("build_chord")
  build_chord(...)
}

#' @rdname myPackage-deprecated
#' @export
buildProgression <- function(...) {
  .Deprecated("build_progression")
  build_progression(...)
}

#' @rdname myPackage-deprecated
#' @export
formatNote <- function(...) {
  .Deprecated("format_note")
  format_note(...)
}

#' @rdname myPackage-deprecated
#' @export
formatNotation <- function(...) {
  .Deprecated("format_notation")
  format_notation(...)
}

#' @rdname myPackage-deprecated
#' @export
noteDistance <- function(...) {
  .Deprecated("note_distance")
  note_distance(...)
}

#' @rdname myPackage-deprecated
#' @export
playWave <- function(...) {
  .Deprecated("play_wave")
  play_wave(...)
}

#' @rdname myPackage-deprecated
#' @export
playFreq <- function(...) {
  .Deprecated("play_freq")
  play_freq(...)
}

#' @rdname myPackage-deprecated
#' @export
playNote <- function(...) {
  .Deprecated("play_note")
  play_note(...)
}

#' @rdname myPackage-deprecated
#' @export
playChord <- function(...) {
  .Deprecated("play_chord")
  play_chord(...)
}

#' @rdname myPackage-deprecated
#' @export
playProgression <- function(...) {
  .Deprecated("play_progression")
  play_progression(...)
}
