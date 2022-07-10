# zzz.R
# ::music::
# 2019 E.D. Gennatas lambdamd.org

#' \pkg{music}: Learn and use music theory
#'
#' The music package allows you to build, play, and visualize scales, chords, and chord progression.
#' For playback, \pkg{music} builds waveforms as matrices and passes them to the \pkg{audio} package which
#' interfaces with the system's audio driver.
#' The default notation and frequencies used throughout the package are based on twelve-tone equal temperament tuning
#' (12ET). Custom tuning can be defined by specifying frequency ratios and a root  note. See \link{note2freq}.
#' A4 defaults to 440Hz, and can be changed with the 'A4' argument.
#' @docType package
#' @name music-package
#' @import graphics utils crayon

NULL

.onAttach <- function(libname, pkgname) {
  music.ver <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),
                         fields = "Version")
  packageStartupMessage(paste0("  .:", pkgname, " v", music.ver, ": Welcome, ", Sys.getenv("USER"),
                               "\n  [", sessionInfo()[2], "]"))

}
