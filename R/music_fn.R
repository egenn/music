# music_fn.R
# ::music::
# Efstathios D. Gennatas egenn.github.io

#' Format notes
#'
#' Format notes for use in other \pkg{music} functions
#'
#' Converts sharps to flats, adds octave number if missing (Default = 4), adn convert (rare) "bb" notes
#' to regular notes
#'
#' @param notes Vector, String: Input notes in the form \code{c("C4", "D4", "Eb4")}
#' @param default.octave Integer: Octave to use if missing in \code{notes}. Default = 4; i.e. \code{"C"}
#' becomes \code{"C4"}
#' @examples
#' formatNote(c("D#4", "Ebb"))
#' @author Efstathios D. Gennatas
#' @export

formatNote <- function(notes, default.octave = 4) {

  # Sharps to flats ====
  index <- grep("#", notes)
  notes[index] <- gsub("A", "B", notes[index])
  notes[index] <- gsub("G", "A", notes[index])
  notes[index] <- gsub("F", "G", notes[index])
  notes[index] <- gsub("D", "E", notes[index])
  notes[index] <- gsub("C", "D", notes[index])
  notes[index] <- gsub("#", "b", notes[index])

  # Add octave if missing
  index <- seq(notes)
  neg.index <- grep(paste(1:10, collapse = "|"), notes)
  index <- setdiff(index, neg.index)
  notes[index] <- paste0(notes[index], default.octave)

  # bb to regular notes ====
  index <- grep("bb", notes)
  for (i in index) {
    .noteup <- gsub("bb", "", notes[i])
    notes[i] <- names(pos)[which(names(pos) == .noteup) - 2]
  }

  notes

} # formatNote.R


#' Note distance in semitones
#'
#' Calculates note distance in semitones
#'
#' @param notes String, vector: Notes in form \code{c("C4", "Eb4", "Gb4")}
#' @param verbose Logical: If TRUE, print to console
#' @return Vector of length \code{1 - length(notes)} with semitone distances between notes
#' @examples
#' noteDistance(strings("C4 Eb4 Gb4 Bb4"))
#' @author Efstathios D. Gennatas
#' @export

noteDistance <- function(notes, verbose = TRUE) {

  notes <- formatNote(notes)

  .diff <- diff(pos[notes])

  if (verbose) {
    .diff1 <- c(0, .diff)
    names(.diff1)[1] <- notes[1]
    print(.diff1)
  }

  invisible(.diff)

} # apollo:noteDistance


#' Separate notes into vector of strings
#'
#' Convenience function to separate notes into vector of strings
#'
#' Makes it easy to copy-paste notes into other functions
#' e.g. \code{playChord(strings("C4 Eb4 G4 D5"))}
#' @param x String: A single character object which consists of multiple notes separated by \code{sep}
#' e.g. \code{"C4 Eb4 G4 D5"}
#' @param sep String: the character that separates notes in \code{x}. Default = " "
#' @examples
#' strings("C4 Eb4 Gb4 Bb4")
#' @export
#' @author Efstathios D. Gennatas

strings <- function(x, sep = " ") {

  strsplit(x, split = sep)[[1]]

} # apollo::strings
