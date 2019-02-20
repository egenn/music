# build.R
# ::music::
# 2019 Efstathios D. Gennatas

.octave <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")
lapply(.octave, function(i) paste0(i, -2:8))
.allnotes <- c(t(sapply(.octave, function(i) paste0(i, -2:8))))[-c(129:132)]
pos <- seq(.allnotes)
names(pos) <- .allnotes

scaleSteps <- list(major = c(2, 2, 1, 2, 2, 2, 1),
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
                   `in` = c(1, 4, 2, 1, 4))

#' Build Scale
#'
#' Build scale
#'
#' @param root String: Root note.  e.g. "C4"
#' @param scale String: Scale to build. Default = "minor"
#' @param descending Logical: If TRUE, return notes in descending order, otherwise in ascending
#' Default = FALSE
#' @param play Logical: If TRUE, play scale using \link{playNote}
#' @param pairs Logical: If TRUE and \code{play = TRUE}, play the root note along with each other note, in sequence
#' @param plot Logical: If TRUE, plot scale notes using \link{cplot.piano}
#' @param formatNotation Logical: If TRUE, format notes to include both flats and sharps
#' to avoid repeating the same letter. e.g. convert c("Gb4", "G4") to c("F#4", "G4")
#' @param ... Additional arguments to be passed to \link{playNote} if \code{play = TRUE}
#' @examples
#' buildScale("C4", "minor")
#' buildScale("B4", "minor", descending = TRUE, plot = TRUE)
#' \dontrun{
#' buildScale("B4", "minor", descending = TRUE, play = TRUE, plot TRUE)
#' }
#' @export
#' @author Efstathios D. Gennatas

buildScale <- function(root, scale = "minor",
                       descending = FALSE,
                       play = FALSE,
                       pairs = FALSE,
                       plot = FALSE,
                       formatNotation = TRUE, ...) {

  if (missing(root)) {
    cat(crayon::silver$bold("Available scales:\n"))
    cat(crayon::cyan$bold(paste(names(scaleSteps), collapse = ", ")), "\n")
    return(invisible(9))
  } else {
    root <- formatNote(root)
  }
  scale <- match.arg(scale, names(scaleSteps))
  root.pos <- pos[root]
  .scale.pos <- cumsum(c(root.pos, scaleSteps[[scale]]))

  .notes <- .allnotes[.scale.pos]
  if (descending) .notes <- rev(.notes)

  if (play) {
    if (!pairs) {
      playNote(.notes, ...)
    } else {
      .progression <- lapply(seq(.notes)[-1], function(i) strings(paste(root, .notes[i])))
      playProgression(.progression, ...)
    }
  }

  .notes1 <- if (formatNotation) formatNotation(.notes) else .notes
  if (plot) {
    cat(blue$bold("  ", root, scale, "scale\n"))
    cplot.piano(.notes1)
    return(invisible(.notes1))
  }

  .notes1

} # music::buildScale

stepsByDegree <- list(`1` = 0,
                      `2` = 2,
                      b3 = 3,
                      `3` = 4,
                      `4` = 5,
                      b5 = 6,
                      `5` = 7,
                      `#5` = 8,
                      `6` = 9,
                      b7 = 10,
                      `7` = 11)

chordSteps <- list(major = c(4, 3),
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
                   flat5th = c(4, 2))

chords <- list()

#' Build Chord
#'
#' Build chord
#'
#' @param root String: Root note
#' @param chord String: Chord to build. Default = "minor"
#' @param play Logical: If TRUE, play chord using \link{playChord}
#' @param plot Logical: If TRUE, plot chord notes using \link{cplot.piano}
#' @param formatNotation Logical: If TRUE, format notes to include both flats and sharps
#' to avoid repeating the same letter. e.g. convert c("Gb4", "G4") to c("F#4", "G4")
#' @param ... Additional arguments to be passed to \link{playChord} if \code{play = TRUE}
#' @examples
#' buildChord("C4", "minor")
#' buildChord("A4", "sus2", plot = TRUE)
#' \dontrun{
#' buildChord("B4", "sus2", play = TRUE)
#' }
#' @export
#' @author Efstathios D. Gennatas

buildChord <- function(root, chord = "minor",
                       play = FALSE,
                       plot = FALSE,
                       formatNotation = TRUE, ...) {

  if (missing(root)) {
    cat(crayon::silver$bold("Available chords:\n"))
    cat(crayon::cyan$bold(paste(names(chordSteps), collapse = ", ")), "\n")
    return(invisible(9))
  } else {
    root <- formatNote(root)
  }
  chord <- match.arg(chord, names(chordSteps))
  root.pos <- pos[root]
  .chord.pos <- cumsum(c(root.pos, chordSteps[[chord]]))
  .chord <- .allnotes[.chord.pos]
  if (play) playChord(.chord, ...)
  .chord1 <- if (formatNotation) formatNotation(.chord) else .chord
  if (plot) {
    cat(blue$bold("  ", root, chord, "chord\n"))
    cplot.piano(.chord1)
    return(invisible(.chord1))
  }
  .chord1

} # music::buildChord


chordProgression <- list(major = c("major", "minor", "minor", "major", "major", "minor", "diminished"),
                         minor = c("minor", "diminished", "major", "minor", "minor", "major", "major"))

#' Build Chord Progression
#'
#' @param root String: Root note. Default = "A4"
#' @param scale String: "major" or "minor". Default = "minor"
#' @param play Logical: If TRUE, play scale using \link{playProgression}
#' @param plot Logical: If TRUE, plot each chord in the progression using \link{cplot.piano}
#' @param formatNotation Logical: If TRUE, format notes to include both flats and sharps
#' to avoid repeating the same letter. e.g. convert c("Gb4", "G4") to c("F#4", "G4")
#' @param ... Additional arguments to be passed to \link{playProgression} if \code{ play = TRUE}
#' @examples
#' buildProgression("C4", "minor")
#' buildProgression("Bb4", "major")
#' \dontrun{
#' buildProgression("Bb4", "major", play = TRUE, plot = TRUE)
#' }
#' @export
#' @author Efstathios D. Gennatas

buildProgression <- function(root = "A4",
                             scale = "minor",
                             play = FALSE,
                             plot = FALSE,
                             formatNotation = TRUE, ...) {

  root <- formatNote(root)
  .scale <- buildScale(root = root, scale = scale)
  .progression <- chordProgression[[scale]]
  .progression <- c(.progression, .progression[1])
  .chords <- lapply(seq(.scale), function(i) buildChord(root = .scale[i],
                                                        chord = .progression[i],
                                                        formatNotation = formatNotation))
  names(.chords) <- paste0(.scale, .progression)

  if (play) playProgression(.chords)
  if (plot) {
    for (i in seq(.chords)) {
      cplot.piano(.chords[[i]])
      cat("\n")
    }
  }
  .chords

} # music::buildProgression
