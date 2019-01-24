# zzz.R
# ::music::
# 2019 Efstathios D. Gennatas egenn.github.io

#' \pkg{music}: A little bit of music in R
#'
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
