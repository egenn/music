# note2freq.R
# ::music::
# 2019 Efstathios D. Gennatas

rt_octave <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")
rt_notes <- c(t(sapply(rt_octave, function(i) paste0(i, -2:8))))[-c(129:132)]
rt_pos <- seq(rt_notes)
names(rt_pos) <- rt_notes
rt_semitones <- rt_pos - rt_pos['A4']

#' Convert musical notes to their frequencies
#'
#' @param note String: Note(s) to convert to frequencies
#' @param A4 Float: Frequency for A4. Default = 440
#' @param default.octave Integer: If \code{note} is provided without octave number (e.g. "C"), default to this
#' octave. Default = 4
#' @export
#' @author Efstathios D. Gennatas

note2freq <- function(note,
                      A4 = 440,
                      default.octave = 4) {

  .notes <- formatNote(note, default.octave = default.octave)
  .frequencies <- A4 * 2 ^ (rt_semitones * (1/12))
  .freqs <- .frequencies[.notes]
  names(.freqs) <- .notes
  .freqs

} # music::note2freq

