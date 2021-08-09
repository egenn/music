# music_fn.R
# ::music::
# 2019 Efstathios D. Gennatas egenn.github.io

#' Format notes
#'
#' Format notes for use in other \pkg{music} functions
#'
#' Converts sharps to flats, adds octave number if missing (Default = 4), and converts (rare) "bb" notes
#' to regular notes
#'
#' @param notes Vector, String: Input notes in the form \code{c("C4", "D4", "Eb4")}
#' @param default.octave Integer: Octave to use if missing in \code{notes}. Default = 4; i.e. \code{"C"}
#' becomes \code{"C4"}
#' 
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
#' 
#' @return Vector of length \code{length(notes)} with semitone distances between successive notes
#' @examples
#' noteDistance(strings("C4 Eb4 Gb4 Bb4"))
#' @author Efstathios D. Gennatas
#' @export

noteDistance <- function(notes) {

  notes <- formatNote(notes)
  .diff <- diff(pos[notes])
  .diff <- c(0, .diff)
  names(.diff)[1] <- notes[1]
  .diff

} # music:noteDistance


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
#' @author Efstathios D. Gennatas
#' @export

strings <- function(x, sep = " ") {

  strsplit(x, split = sep)[[1]]

} # music::strings


#' Format Notation
#'
#' Converts the internal note representation which uses only flats, to the notation
#' commonly used to write scales and chords, where a mix of sharps and flats is used to avoid
#' repeating the same letter note. (e.g. \code{"G#5" "A5"}, instead of \code{"Ab5" "A5"})
#' e.g. convert the C4 Lydian from:
#' "C4"  "D4"  "E4"  "Gb4" "G4"  "A4"  "B4"  "C5"
#' to:
#' "C4"  "D4"  "E4"  "F#4" "G4"  "A4"  "B4"  "C5"
#' or convert the A4 major from:
#' "A4"  "B4"  "Db5" "D5"  "E5"  "Gb5" "Ab5" "A5"
#' to:
#' "A4"  "B4"  "C#5" "D5"  "E5"  "F#5" "G#5" "A5"
#'
#' @param notes String, vector: Notes to format
#' @examples
#' formatNotation(c("Db4", "D4", "E4", "Gb4", "G4", "A4", "B4", "C5"))
#' @author Efstathios D. Gennatas
#' @export

formatNotation <- function(notes) {

  note.letters <- LETTERS[seq(7)]

  fN <- function(notes) {
    a <- 1
    while (a < length(notes)) {
      i <- a
      a <- a + 1
      .notes <- substring(notes, 1, 1)
      .match <- which(.notes[i] == .notes[-i])[1] + 1
      .diff <- if (!is.na(.match)) {
        note1 <- gsub(".$", "", notes[i])
        note2 <- gsub(".$", "", notes[.match])
        index <- c(i, .match)
        note1 != note2
      } else {
        FALSE
      }
      if (.diff) {
        # find which is b
        is.b <- grep(pattern = "b", ignore.case = FALSE, x = c(note1, note2))
        .change <- index[is.b]
        note.tochange <- notes[.change]
        octave <- substring(note.tochange, nchar(note.tochange))
        new.note.letter <- note.letters[which(note.letters == .notes[.change]) - 1]
        if (length(new.note.letter) == 0) new.note.letter <- "G"
        new.note <- paste0(new.note.letter, "#", octave)
        notes[.change] <- new.note
      }
    }
    notes
  }
  # Get letters

  fN(fN(notes))

} # music::formatNotation
