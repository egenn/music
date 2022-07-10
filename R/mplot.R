# mplot.R
# ::music::
# 2019 E.D. Gennatas lambdamd.org

#' Plot waveform
#'
#' @param x Input
#' @param type String: "l" for lines, "p" for points. Default = "l"
#' @param main String: Plot title
#' @param legend Logical: If TRUE, show legends on plot, if \code{x} has column names
#' @param lwd Float: Line width. Default = 1
#' @param pty String: "m" to fill available device space, "s" for square plot. Default = "m"
#' @param bg Color: background color
#' @param fg Color: foreground color
#' @param col Color: Point/line color
#' @param col.axis Color: Axes' color
#' @param col.lab Color: Label color
#' @param col.main Color: Title color
#' @param col.legend Color: Legend color
#' @param tcl The 'tcl' param of par
#' @param xaxt The 'xaxt' param of par
#' @param yaxt The 'yaxt' param of par
#' @param new The 'new' param of par
#' @param mgp The 'mgp' param of par
#' @param mar Vector, length 4: Margins for \code{par}
#' @param oma Vector, length 4: The 'oma' param of par
#' @param ... Additional parameters to pass to \code{plot}
#' 
#' @export
#' @author E.D. Gennatas

mplot <- function(x,
                  type = "l",
                  main = NULL,
                  legend = TRUE,
                  lwd = 1,
                  pty = "m",
                  bg = "black",
                  fg = "gray50",
                  col = "cyan",
                  col.axis = "gray50",
                  col.lab = "gray50",
                  col.main = "gray80",
                  col.legend = "white",
                  tcl = .3,
                  xaxt = "s",
                  yaxt = "s",
                  new = FALSE,
                  mgp = c(2, 0, 0),
                  mar = NULL,
                  oma = NULL, ...) {

  par.orig <- par(no.readonly = TRUE)
  on.exit(par(par.orig))
  .ncol <- NCOL(x)
  if (is.null(mar)) mar <- if (.ncol == 1) c(1.2, 1.2, .4, .4) else c(0, 0, 0, 0)
  if (is.null(oma)) {
    if (.ncol == 1) {
      oma <- if (is.null(main)) c(0, 0, 0, .5) else c(0, 0, 1, .5)
    } else {
      oma <- if (is.null(main)) c(1.2, 1.2, 1.2, 0.5) else c(1.2, 1.2, 2.2, 0.5)
    }
  }
  par(bg = bg, pty = pty, mar = mar, mfrow = c(.ncol, 1), oma = oma, new = new)
  .names <- colnames(x)

  if (.ncol == 1) {
    x <- matrix(x, ncol = 1)
  } else {
    if (length(col) < .ncol) col <- rep(col, length.out = .ncol)
  }
  for (i in seq(.ncol)) {
    plot(x[, i], y = NULL,
         type = type,
         lwd = lwd,
         xlab = "",
         ylab = "",
         fg = fg,
         col = col[i],
         col.axis = col.axis,
         col.lab = col.lab,
         tcl = tcl,
         xaxt = xaxt,
         yaxt = yaxt,
         xaxs = "i",
         yaxs = "r",
         mgp = mgp, ...)
    if (i == 1) if (!is.null(main)) mtext(main, col = col.main, line = 0.3, adj = 0, font = 2, xpd = TRUE)
    if (legend & !is.null(.names)) mtext(.names[i], col = col.legend, line = -1.5, adj = .99)
  }

} # music::mplot
