# cplot.piano.R
# ::music::
# 2019 Efstathios D. Gennatas

#' Console piano plot for notes
#'
#' Build an ASCII plot of notes on a piano
#'
#' @param notes String, vector: Notes to highlight. Default = \code{buildScale("C4", "minor")}
#' @param blackKey.col Color to use for black keys. Default = "white" for use on a dark terminal.
#' Set to "black" for use on a light terminal.
#' @export
#' @examples
#' cplot.piano(buildScale("B4", "minor"))
#' @author Efstathios D. Gennatas

cplot.piano <- function(notes = buildScale("C4", "minor"),
                        blackKey.col = "white") {

  notes0 <- notes
  notes <- formatNote(notes)
  bg <- if (blackKey.col == "white") crayon::bgWhite else crayon::bgBlack
  sel <- if (blackKey.col == "white") crayon::bgCyan$black else crayon::bgCyan$white
  notes.pos <- sort(pos[notes])
  first.note <- gsub(paste0(c(1:9, "-"), collapse = "|"), "", names(notes.pos[1]))
  steps <- diff(notes.pos)
  cp.pos <- 1:12
  names(cp.pos) <- .octave
  first.pos <- cp.pos[first.note]
  notes.sel <- c(first.pos, first.pos + cumsum(steps))
  col <- vector("list", 26)
  names(col) <- paste0("n", seq(26))
  black.keys <- c(2, 4, 7, 9, 11, 14, 16, 19, 21, 23, 26)
  for (i in seq(26)[-black.keys]) col[[i]] <- crayon::silver
  for (i in black.keys) col[[i]] <- bg
  for (i in notes.sel) col[[i]] <- sel

  cat("\n")
  cat("  | ", " |",bg(" "),"|", " |",bg(" "),"| ", " | ", " |",bg(" "),"|", " |",bg(" "),"|", " |",bg(" "),"|","  |  |",bg(" "),"|", " |",bg(" "),"| ", " | ", " |",bg(" "),"|", " |",bg(" "),"|", " |",bg(" "),"|","  |  |",bg(" "),"\n", sep = "")
  cat("  | ", " |",col$n2(" "),"|", " |",col$n4(" "),"| ", " | ", " |",col$n7(" "),"|", " |",col$n9(" "),"|", " |",col$n11(" "),"|","  |  |",col$n14(" "),"|", " |",col$n16(" "),"| ", " | ", " |",col$n19(" "),"|", " |",col$n21(" "),"|", " |",col$n23(" "),"|","  |  |",col$n26(" "),"\n", sep = "")
  cat("  |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |\n")
  cat("  |",col$n1("C"),"|",col$n3("D"),"|",col$n5("E"),"|",col$n6("F"),"|",col$n8("G"),"|",col$n10("A"),"|",col$n12("B"),"|",col$n13("C"),"|",col$n15("D"),"|",col$n17("E"),"|",col$n18("F"),"|",col$n20("G"),"|",col$n22("A"),"|",col$n24("B"),"|",col$n25("C"),"|\n")

  cat("\n ", silver("Notes:"), cyan(notes0), "\n")

} # music::cplot.piano
