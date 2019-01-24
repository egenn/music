# freq2wave.R
# ::music::
# 2019 Efstathios D. Gennatas

#' Frequency to waveform
#'
#' @inheritParams playFreq
#' @param frequency Float: Frequency to convert to waveform
#' @param plot Logical: If TRUE, plot wave
#' @export
#' @author Efstathios D. Gennatas

freq2wave <- function(frequency,
                      oscillator = c("sine", "square", "saw", "triangle"),
                      duration = 1,
                      BPM = 120,
                      sample.rate = 44100,
                      attack.time = 50,
                      inner.release.time = 50,
                      plot = FALSE) {

  oscillator <- match.arg(oscillator)
  t <- seq(0, duration / BPM * 60, 1 / sample.rate)

  wave <- switch(oscillator,
                 sine = sin(t * frequency * 2 * pi),
                 square = sign(sin(t * frequency * 2 * pi)),
                 saw = 2 * (t * frequency - floor(.5 + t * frequency)),
                 triangle = 2 * abs(2 * (t * frequency - floor(.5 + t * frequency))) - 1)

  if (attack.time > 0) {
    attack <- rep(1, length(wave))
    n.attack.samples <- attack.time / 1000 * sample.rate
    attack[seq(n.attack.samples)] <- seq(0, 1, length.out = n.attack.samples)
    wave <- wave * attack
  }
  if (inner.release.time > 0) {
    total <- length(wave)
    inner.release <- rep(1, total)
    n.inner.release.samples <- inner.release.time / 1000 * sample.rate
    inner.release[total - (seq(n.inner.release.samples) - 1)] <- seq(0, 1, length.out = n.inner.release.samples)
    wave <- wave * inner.release
  }

  if (plot) mplot(wave)

  wave

} # music::freq2wave
