# freq2wave.R
# ::music::
# 2019 Efstathios D. Gennatas

#' Frequency to waveform
#'
#' @inheritParams playFreq
#' @param frequency Float, vector: Frequency/ies to convert to waveform
#' @param plot Logical: If TRUE, plot wave(s) using \link{mplot}
#' 
#' @examples
#' wave <- freq2wave(note2freq(buildChord("A4", "sus2")))
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
  .t <- seq(0, duration / BPM * 60, 1 / sample.rate)
  n.freqs <- length(frequency)

  # Wave ====
  wave <- sapply(seq(n.freqs), function(i)
    switch(oscillator,
           sine = sin(.t * frequency[i] * 2 * pi),
           square = sign(sin(.t * frequency[i] * 2 * pi)),
           saw = 2 * (.t * frequency[i] - floor(.5 + .t * frequency[i])),
           triangle = 2 * abs(2 * (.t * frequency[i] - floor(.5 + .t * frequency[i]))) - 1))

  n.samples <- NROW(wave)

  # Attack ====
  if (attack.time > 0) {
    attack <- rep(1, n.samples)
    # If attack.time was inappropriately long for your waveform, you may have longer attack than wave
    n.attack.samples <- round(min(n.samples, attack.time / 1000 * sample.rate))
    attack[seq(n.attack.samples)] <- seq(0, 1, length.out = n.attack.samples)
    wave <- wave * attack
  }

  # Inner Release ====
  if (inner.release.time > 0) {
    inner.release <- rep(1, n.samples)
    n.inner.release.samples <- round(min(n.samples, inner.release.time / 1000 * sample.rate))
    inner.release[n.samples - (seq(n.inner.release.samples) - 1)] <- seq(0, 1, length.out = n.inner.release.samples)
    wave <- wave * inner.release
  }

  .names <- if (!is.null(names(frequency))) names(frequency) else paste(frequency, "Hz")
  colnames(wave) <- .names
  if (plot) mplot(wave)
  wave

} # music::freq2wave
