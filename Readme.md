**music** R package [![CRAN](https://www.r-pkg.org/badges/version/music)](https://cran.r-project.org/package=music) [![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![Build Status](https://travis-ci.com/egenn/music.svg?branch=master)](https://travis-ci.com/egenn/music) [![codecov](https://codecov.io/gh/egenn/music/branch/master/graph/badge.svg)](https://codecov.io/gh/egenn/music)
======================
_A little music theory in R_

## Description
Build, play, and visualize scales, chords, and chord progressions

_ASCII piano console plots:_  
<img align = "center" src="https://egenn.github.io/imgs/music_cplot.png" width="600">


_Waveform time series plots:_  
<img align = "center" src="https://egenn.github.io/imgs/music_mplot_B4minor.png" width="600">


## Installation
You can either install from [CRAN](https://cran.r-project.org/package=music) or grab the latest version directly from this repository.
### Install from CRAN:
```r
install.packages("music")
```

### Install latest version from GitHub:
Install dependencies, if not already installed:
```r
install.packages(c("audio", "crayon", "devtools"))
```
Install using devtools:
```r
devtools::install_github("egenn/music")
```

## Documentation
[Official R manual](https://egenn.github.io/docs/music.pdf)

[music Vignette](https://egenn.github.io/music/music-vignette)

<img align = "center" src="http://egenn.github.io/imgs/music_hex_logo.png" width="250">
