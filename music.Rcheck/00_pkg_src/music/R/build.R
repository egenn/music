# build.R
# ::music::
# 2019- EDG rtemis.org

.octave <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")
lapply(.octave, function(i) paste0(i, -2:8))
.allnotes <- c(t(sapply(.octave, function(i) paste0(i, -2:8))))[-c(129:132)]
pos <- seq(.allnotes)
names(pos) <- .allnotes

scaleSteps <- list(
  major = c(2, 2, 1, 2, 2, 2, 1),
  minor = c(2, 1, 2, 2, 1, 2, 2),
  dorian = c(2, 1, 2, 2, 2, 1, 2),
  phrygian = c(1, 2, 2, 2, 1, 2, 2),
  lydian = c(2, 2, 2, 1, 2, 2, 1),
  lydianAugmented = c(2, 2, 2, 2, 1, 2, 1),
  acoustic = c(2, 2, 2, 1, 2, 1, 2),
  mixolydian = c(2, 2, 1, 2, 2, 1, 2),
  locrian = c(1, 2, 2, 1, 2, 2, 2),
  majorLocrian = c(2, 2, 1, 1, 2, 2, 2),
  harmonicMajor = c(2, 2, 1, 2, 1, 3, 1),
  harmonicMinor = c(2, 1, 2, 2, 1, 3, 1),
  halfDiminished = c(2, 1, 2, 1, 2, 2, 2),
  minorPentatonic = c(3, 2, 2, 3, 2),
  majorPentatonic = c(2, 2, 3, 2, 3),
  blues = c(3, 2, 1, 1, 3, 2),
  altered = c(1, 2, 1, 2, 2, 2, 2),
  hirajoshi = c(4, 2, 1, 4, 1),
  insen = c(1, 4, 2, 4, 2),
  algerian = c(2, 1, 3, 1, 1, 3, 1, 2, 1, 2),
  hungarian = c(2, 1, 3, 1, 1, 3, 1),
  `in` = c(1, 4, 2, 1, 4)
)

#' Build Scale
#'
#' Build Scale / Mode
#'
#' @param root String: Root note.  e.g. "C4"
#' @param scale String: Scale to build. Default = "minor"
#' @param descending Logical: If TRUE, return notes in descending order,
#' otherwise in ascending
#' @param plot Logical: If TRUE, plot scale notes using \link{cplot_piano}
#' @param play Logical: If TRUE, play scale using \link{play_note}
#' @param pairs Logical: If TRUE and \code{play = TRUE}, play the root note
#' along with each other note, in sequence
#' @param format_notation Logical: If TRUE, format notes to include both flats
#' and sharps to avoid repeating the same letter. e.g. convert c("Gb4", "G4")
#' to c("F#4", "G4")
#' @param ... Additional arguments to be passed to \link{play_note} if
#' \code{play = TRUE}
#'
#' @author EDG
#' @export
#' @examples
#' build_scale("C4", "minor")
#' build_scale("B4", "minor", descending = TRUE, plot = TRUE)
#' \dontrun{
#' build_scale("B4", "minor", descending = TRUE, play = TRUE, plot = TRUE)
#' }

build_scale <- function(
  root,
  scale = "minor",
  descending = FALSE,
  plot = TRUE,
  play = FALSE,
  pairs = FALSE,
  format_notation = TRUE,
  ...
) {
  if (missing(root)) {
    cat(crayon::silver$bold("Available scales:\n"))
    cat(crayon::cyan$bold(paste(names(scaleSteps), collapse = ", ")), "\n")
    return(invisible(64))
  } else {
    root <- format_note(root)
  }
  scale <- match.arg(scale, names(scaleSteps))
  root.pos <- pos[root]
  .scale.pos <- cumsum(c(root.pos, scaleSteps[[scale]]))

  .notes <- .allnotes[.scale.pos]
  if (descending) {
    .notes <- rev(.notes)
  }

  if (play) {
    if (!pairs) {
      play_note(.notes, ...)
    } else {
      .progression <- lapply(
        seq(.notes)[-1],
        function(i) strings(paste(root, .notes[i]))
      )
      play_progression(.progression, ...)
    }
  }

  .notes1 <- if (format_notation) {
    format_notation(.notes)
  } else {
    .notes
  }
  if (plot) {
    cat(blue$bold(" ", root, scale, "scale\n"))
    cplot_piano(.notes1)
    return(invisible(.notes1))
  }

  .notes1
} # music::build_scale

stepsByDegree <- list(
  `1` = 0,
  `2` = 2,
  b3 = 3,
  `3` = 4,
  `4` = 5,
  b5 = 6,
  `5` = 7,
  `#5` = 8,
  `6` = 9,
  b7 = 10,
  `7` = 11
)

chordSteps <- list(
  major = c(4, 3),
  minor = c(3, 4),
  sus2 = c(2, 5),
  sus4 = c(5, 2),
  `7th` = c(4, 3, 3),
  major7th = c(4, 3, 4),
  minor7th = c(3, 4, 3),
  `6th` = c(4, 3, 2),
  minor6th = c(3, 4, 2),
  diminished = c(3, 3),
  diminished7th = c(3, 3, 3),
  halfDiminished7th = c(3, 3, 4),
  augmented = c(4, 4),
  `7th#5` = c(4, 4, 2),
  `9th` = c(4, 3, 3, 4),
  `7th#9` = c(4, 3, 3, 5),
  major9th = c(4, 3, 4, 3),
  added9th = c(4, 3, 7),
  minor9th = c(3, 4, 3, 4),
  minorAdd9th = c(3, 4, 7),
  `5th` = 7,
  flat5th = c(4, 2)
)

chords <- list()

#' Build Chord
#'
#' @param root String: Root note
#' @param chord String: Chord to build. Default = "minor"
#' @param plot Logical: If TRUE, plot chord notes using \link{cplot_piano}
#' @param play Logical: If TRUE, play chord using \link{play_chord}
#' @param format_notation Logical: If TRUE, format notes to include both flats
#' and sharps to avoid repeating the same letter. e.g. convert c("Gb4", "G4") to
#' c("F#4", "G4")
#' @param ... Additional arguments to be passed to \link{play_chord} if
#' \code{play = TRUE}
#'
#' @author EDG
#' @export
#' @examples
#' build_chord("C4", "minor")
#' build_chord("A4", "sus2", plot = TRUE)
#' \dontrun{
#' build_chord("B4", "sus2", play = TRUE)
#' }

build_chord <- function(
  root,
  chord = "minor",
  plot = TRUE,
  play = FALSE,
  format_notation = TRUE,
  ...
) {
  if (missing(root)) {
    cat(crayon::silver$bold("Available chords:\n"))
    cat(crayon::cyan$bold(paste(names(chordSteps), collapse = ", ")), "\n")
    return(invisible(64))
  } else {
    root <- format_note(root)
  }
  chord <- match.arg(chord, names(chordSteps))
  root.pos <- pos[root]
  .chord.pos <- cumsum(c(root.pos, chordSteps[[chord]]))
  .chord <- .allnotes[.chord.pos]
  if (play) {
    play_chord(.chord, ...)
  }
  .chord1 <- if (format_notation) format_notation(.chord) else .chord
  if (plot) {
    cat(blue$bold("  ", root, chord, "chord\n"))
    cplot_piano(.chord1)
    return(invisible(.chord1))
  }
  .chord1
} # music::build_chord

chordProgression <- list(
  major = c(
    "major",
    "minor",
    "minor",
    "major",
    "major",
    "minor",
    "diminished"
  ),
  minor = c(
    "minor",
    "diminished",
    "major",
    "minor",
    "minor",
    "major",
    "major"
  )
)

#' Build Chord Progression
#'
#' @param root String: Root note. Default = "A4"
#' @param scale String: "major" or "minor". Default = "minor"
#' @param plot Logical: If TRUE, plot each chord in the progression using
#' \link{cplot_piano}
#' @param play Logical: If TRUE, play scale using \link{play_progression}
#' @param format_notation Logical: If TRUE, format notes to include both flats a
#' nd sharps to avoid repeating the same letter. e.g. convert c("Gb4", "G4") to
#' c("F#4", "G4")
#' @param ... Additional arguments to be passed to \link{play_progression} if
#' \code{ play = TRUE}
#'
#' @author EDG
#' @export
#' @examples
#' build_progression("C4", "minor")
#' build_progression("Bb4", "major")
#' \dontrun{
#' build_progression("Bb4", "major", play = TRUE, plot = TRUE)
#' }

build_progression <- function(
  root = "A4",
  scale = "minor",
  plot = FALSE,
  play = FALSE,
  format_notation = TRUE,
  ...
) {
  root <- format_note(root)
  .scale <- build_scale(root = root, scale = scale, plot = FALSE)
  .progression <- chordProgression[[scale]]
  .progression <- c(.progression, .progression[1])
  .chords <- lapply(
    seq(.scale),
    function(i) {
      build_chord(
        root = .scale[i],
        chord = .progression[i],
        format_notation = format_notation,
        plot = plot
      )
    }
  )
  names(.chords) <- paste0(.scale, .progression)

  if (play) {
    play_progression(.chords)
  }

  #   if (plot) {
  #       for (i in seq(.chords)) {
  #           cplot_piano(.chords[[i]])
  #           cat("\n")
  #       }
  #   }
  .chords
} # music::build_progression
