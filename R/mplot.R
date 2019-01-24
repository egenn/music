# mplot.R
# ::music::
# 2019 Efstathios D. Gennatas

#' Plot waveform
#'
#' @param x Input
#' @param y Optional input
#' @param type String: "l" for lines, "p" for points. Default = "l"
#' @param main String: Plot title
#' @param pty String: "m" to fill available device space, "s" for square plot. Default = "m"
#' @param bg Color: background color
#' @param fg Color: foreground color
#' @param col Color: Point/line color
#' @param col.axis Color: Axes' color
#' @param col.lab Color: Label color
#' @param col.main Color: Title color
#' @param tcl The tcl param of par
#' @param xaxt The xaxt param of par
#' @param yaxt The yaxt param of par
#' @param mgp The mgp param of par
#' @param mar Vector: Margins for \code{par}
#' @param oma Vector: The oma param of par
#' @export
#' @author Efstathios D. Gennatas

mplot <- function(x, y = NULL,
                  type = "l",
                  main = NULL,
                  pty = "m",
                  bg = "black",
                  fg = "gray50",
                  col = "cyan",
                  col.axis = "gray50",
                  col.lab = "gray50",
                  col.main = "gray50",
                  tcl = .3,
                  xaxt = "s",
                  yaxt = "s",
                  mgp = c(2, 0, 0),
                  mar = NULL,
                  oma = NULL) {

  par.orig <- par(no.readonly = TRUE)
  on.exit(par(par.orig))
  .ncol <- NCOL(x)
  if (is.null(mar)) mar <- if (.ncol == 1) c(1.2, 1.2, .4, .4) else c(0, 0, 0, 0)
  if (is.null(oma)) oma <- if (.ncol == 1) rep(0, 4) else c(1.2, 1.2, 1.2, 1.2)
  par(bg = bg, mar = mar, mfrow = c(.ncol, 1), oma = oma)

  if (.ncol == 1) {
    plot(x, y = y,
         type = type,
         xlab = "",
         ylab = "Amplitude",
         fg = fg,
         pty = pty,
         col = col,
         col.axis = col.axis,
         col.lab = col.lab,
         tcl = tcl,
         xaxt = xaxt,
         yaxt = yaxt,
         mgp = mgp)
    if (!is.null(main)) mtext(main, col = col.main, line = 1, adj = 0, font = 2, xpd = TRUE)
  } else {
    for (i in seq(.ncol)) {
      plot(x[, i], y = y,
           type = type,
           xlab = "",
           ylab = "Amplitude",
           fg = fg,
           pty = pty,
           col = col,
           col.axis = col.axis,
           col.lab = col.lab,
           tck = tcl/(2 * .ncol),
           xaxt = xaxt,
           yaxt = yaxt,
           mgp = mgp)
      if (i == 1) if (!is.null(main)) mtext(main, col = col.main, line = 1, adj = 0, font = 2, xpd = TRUE)
    }
  }

} # music::mplot
