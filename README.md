<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN
Version](http://www.r-pkg.org/badges/version/soundgen)](https://cran.r-project.org/package=soundgen)
[![Downloads](http://cranlogs.r-pkg.org/badges/soundgen)](https://CRAN.R-project.org/package=soundgen)

R package for sound synthesis and acoustic analysis.  
Homepage with help, demos, etc: <http://cogsci.se/soundgen.html>  
Source code on github: <https://github.com/tatters/soundgen>

Performs parametric synthesis of sounds with harmonic and noise
components such as animal vocalizations or human voice. Also includes
tools for spectral analysis, pitch tracking, audio segmentation,
self-similarity matrices, morphing, etc.

# Key functions

  - Sound synthesis from R console: `soundgen()`
  - Shiny app for sound synthesis (opens in a browser): `soundgen_app()`
  - Acoustic analysis of a wav/mp3 file / folder: `analyze()` / `analyzeFolder()`
  - Shiny app for editing intonation contours (opens in a browser):
    `pitch_app()`
  - Shiny app for measuring formants (opens in a browser):
    `formant_app()`
  - Measuring syllables, pauses, and bursts in a wav/mp3 file / folder:
    `segment()` / `segmentFolder`

For more information, please see the vignettes on sound synthesis and
acoustic analysis:

`vignette("sound_generation", package="soundgen")`

`vignette("acoustic_analysis", package="soundgen")`

Or, to open the vignettes in a browser:  
`RShowDoc('sound_generation', package = 'soundgen')`

`RShowDoc('acoustic_analysis', package = 'soundgen')`

# Example of sound synthesis

Use the `soundgen()` function to create a breathy moan:

``` r
s = soundgen(
  sylLen = 230,               # duration of voiced part, ms
  pitch = c(305, 280),        # pitch, Hz (goes down from 305 to 380 Hz)
  ampl = c(0, -20),           # amplitude, dB (gradual fade-out by 20 dB)
  rolloff = -30,              # strong f0, weak harmonics
  temperature = 0.05,         # some stochasticity in generation
  formants = c(260, 960, 1500, 2200,  # F1-F8 formant frequencies, Hz
               2600, 3600, 4200, 4500),
  noise = data.frame(
    time = c(-50, 120, 700),  # time of noise anchors
    value = c(-15, -5, -50)   # noise amplitude, dB
  ),
  rolloffNoise = 0,           # flat noise spectrum before adding formants
  addSilence = 0, samplingRate = 44100, pitchSamplingRate = 44100,
  play = TRUE, plot = TRUE, osc = TRUE, ylim = c(0, 6)
)
```

<img src="man/figures/README-synthsesis-1.png" width="80%" style="display: block; margin: auto;" />

<p>
</p>

# Example of acoustic analysis

Use the `analyze()` function to detect pitch and obtain other spectral
descriptives of the sound we have just synthesized:

``` r
a = analyze(s, 44100, ylim = c(0, 6))
#> Scale not specified. Assuming that max amplitude is 1
```

<img src="man/figures/README-analysis-1.png" width="80%" style="display: block; margin: auto;" />

``` r
a[1:5, c('pitch', 'peakFreq', 'harmHeight', 'HNR', 'ampl', 'loudness')]
#>      pitch peakFreq harmHeight       HNR      ampl  loudness
#> 1       NA       NA         NA        NA 0.0305833        NA
#> 2       NA 305.2747         NA  1.083205 0.1442435  9.636858
#> 3 300.0882 305.2747   1197.802 12.934929 0.3642711 17.808124
#> 4 299.4505 305.2747   1197.802 14.042033 0.5131389 22.289716
#> 5 296.0563 305.2747   1110.114 13.450248 0.4959925 22.193244
colnames(a)
#>  [1] "duration"           "duration_noSilence" "time"               "ampl"              
#>  [5] "amplVoiced"         "dom"                "entropy"            "f1_freq"           
#>  [9] "f1_width"           "f2_freq"            "f2_width"           "f3_freq"           
#> [13] "f3_width"           "harmEnergy"         "harmHeight"         "HNR"               
#> [17] "loudness"           "peakFreq"           "pitch"              "quartile25"        
#> [21] "quartile50"         "quartile75"         "specCentroid"       "specSlope"         
#> [25] "voiced"
```

# Installation

To install the current release from CRAN: `install.packages("soundgen")`

NB: Make sure all dependencies have been installed correctly\! For
problems with seewave, see <http://rug.mnhn.fr/seewave/>

On Macs, you may need to do the following:

  - First install brew according to the instructions here:
    <https://brew.sh/>
  - Then run the following from the terminal  
    `brew install libsndfile`  
    `brew install fftw`
  - Finally, install soundgen in R:  
    `install.packages("soundgen")`
