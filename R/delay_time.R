# delay_time.R
# ::music::
# 2017 E.D. Gennatas www.lambdamd.org

#' Delay and Reverb Time Calculator
#' 
#' Calculates delay and reverb time in milliseconds given tempo in beats per 
#' minute (BPM) and delay/reverb time in note duration
#' 
#' This function originally appeared in the \code{rtemis} package
#' 
#' @param bpm Integer: Beats per minute. Default = 120
#' @param note Character: Delay/Reverb time in note duration: "2", "1", "1/2", 
#' "1/2T", "1/4D", "1/4", "1/4T", "1/8D", "1/8", "1/8T", "1/16D", "1/16", 
#' "1/16T", "1/32D", "1/32", "1/32T".
#' "2" means a double note, "1" a whole, and so on. "T" denotes a triple note, 
#' "D" denotes a dotted note.
#' Case insensitive. Default = "1/4" (quarter note)
#' @param verbose Logical: If TRUE print message to console
#' 
#' @return Delay time, invisibly
#' 
#' @author E.D. Gennatas
#' @export
#' 
#' @examples
#' delay_time(120, "1/8")

delay_time <- function(bpm = 120, 
                       note = c("2", "1", "1/2", "1/2T", "1/4D", "1/4", "1/4T", 
                                "1/8D", "1/8", "1/8T", "1/16D", "1/16", "1/16T", 
                                "1/32D", "1/32", "1/32T"),
                       verbose = TRUE) {
  
  note <- match.arg(note)
  # 60k milliseconds in 1 minute
  # .t is delay time for quarter note
  .t <- 60000 / bpm
  
  note <- toupper(note)
  
  # Adjust .t for requested echo note duration
  delayt <- switch(note,
                   "2" = .t * 8,
                   "1" = .t * 4,
                   "1/2" = .t * 2,
                   "1/2T" = .t * 4 / 3,
                   "1/4D" = .t * 1.5,
                   "1/4" = .t,
                   "1/4T" = .t * 2 / 3,
                   "1/8D" = .t / 2 * 1.5,
                   "1/8" = .t / 2,
                   "1/8T" = .t / 2 * 2 / 3,
                   "1/16D" = .t / 4 * 1.5,
                   "1/16" = .t / 4,
                   "1/16T" = .t / 4 * 2 / 3,
                   "1/32D" = .t / 8 * 1.5,
                   "1/32" = .t / 8,
                   "1/32T" = .t / 8 * 2 / 3)
  if (verbose) {
    cat("  BPM:", blue$bold(bpm), "Note:", blue$bold(note), "\n")
    cat("  Delay time: ", blue$bold(round(delayt, 2)), blue("ms\n"), sep = "")
  }
  invisible(delayt)
  
} # music::delay_time
