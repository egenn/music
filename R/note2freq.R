# note2freq.R
# ::music::
# 2019 Efstathios D. Gennatas

rt_octave <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")
rt_notes <- c(t(sapply(rt_octave, function(i) paste0(i, -2:8))))[-c(129:132)]
rt_pos <- seq(rt_notes)
names(rt_pos) <- rt_notes

#' Convert musical notes to frequencies
#'
#' Convert notes to frequencies
#'
#' @param note String: Note(s) to convert to frequencies
#' @param tuning String: "12ET": 12-note equal temperament, "custom": Intonation defined by \code{customRatios}
#' @param custom.ratios Numeric, vector, length 13: Custom ratios for a 12-note scale, starting with 1 (root)
#' and ending in 2 (octave) to use when \code{tuning = "custom"}. The A4 note will be set to \code{A4} Hz and the
#' rest of the frequencies will be built based on these ratios and the \code{customRoot}
#' @param A4 Float: Frequency for A4 in Hz. Default = 440
#' @param custom.root String: Root note for just intonation (\code{tuning = "custom"}). Default = "C"
#' @param default.octave Integer: If \code{note} is provided without octave number (e.g. "C"), default to this
#' octave. Default = 4
#' @examples
#' note2freq(buildScale("B4", "minor"))
#' @author Efstathios D. Gennatas
#' @export

note2freq <- function(note,
                      tuning = c("12ET", "custom"),
                      custom.ratios = c(1, 16/15, 9/8, 6/5, 5/4, 4/3, 45/32, 3/2, 8/5, 5/3, 9/5, 15/8, 2),
                      A4 = 440,
                      custom.root = "C",
                      default.octave = 4) {

  tuning <- match.arg(tuning)
  .notes <- formatNote(note, default.octave = default.octave)

  if (tuning == "12ET") {
    rt_semitones <- rt_pos - rt_pos["A4"]
    .frequencies <- A4 * 2 ^ (rt_semitones/12)
  } else if (tuning == "custom") {
    custom.root <- formatNote(custom.root)
    rt_semitones <- rt_pos - rt_pos[custom.root]
    .octaves <- 2 ^ floor(rt_semitones / 12)
    .index <- rt_semitones %% 12 + 1
    A4.index <- noteDistance(c(custom.root, "A4"))[2] + 1
    .root.freq <- solve(custom.ratios[A4.index], A4)
    .frequencies <- .root.freq * .octaves * custom.ratios[.index]
  }

  .freqs <- .frequencies[.notes]
  names(.freqs) <- .notes
  .freqs

} # music::note2freq
