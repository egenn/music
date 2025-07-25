% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freq2wave.R
\name{freq2wave}
\alias{freq2wave}
\title{Frequency to waveform}
\usage{
freq2wave(
  frequency,
  oscillator = c("sine", "square", "saw", "triangle"),
  duration = 1,
  BPM = 120,
  sample.rate = 44100,
  attack.time = 50,
  inner.release.time = 50,
  plot = FALSE
)
}
\arguments{
\item{frequency}{Float, vector: Frequency/ies to convert to waveform}

\item{oscillator}{String: "sine", "square", "saw". Default = "sine"}

\item{duration}{Float: Note duration in beats. Default = 1}

\item{BPM}{Integer: Beats per minute. Default = 120}

\item{sample.rate}{Integer: Sample rate. Default = 44100}

\item{attack.time}{Integer: Attack time. Default = 50 (Helps prevent popping)}

\item{inner.release.time}{Integer: Release time, that ends on note OFF
(instead of beginning at note OFF). Default = 50 (Also helps prevent popping)}

\item{plot}{Logical: If TRUE, plot wave(s) using \link{mplot}}
}
\description{
Frequency to waveform
}
\examples{
wave <- freq2wave(note2freq(buildChord("A4", "sus2")))
}
\author{
E.D. Gennatas
}
