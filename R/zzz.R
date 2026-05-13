# zzz.R
# ::music::
# 2019- EDG rtemis.org

.onAttach <- function(libname, pkgname) {
  music.ver <- read.dcf(
    file = system.file("DESCRIPTION", package = pkgname),
    fields = "Version"
  )
  packageStartupMessage(paste0(
    "  .:",
    pkgname,
    " v",
    music.ver,
    " \U1F3A7 Welcome, ",
    Sys.getenv("USER")
  ))
}
