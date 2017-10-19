# soundgen
R package for sound synthesis and acoustic analysis, version 1.1.0
Homepage with help, demos, etc: http://cogsci.se/soundgen.html
Source code on github: https://github.com/tatters/soundgen

Performs parametric synthesis of sounds with harmonic and noise components 
such as animal vocalizations or human voice. Also includes tools for spectral analysis, 
pitch tracking, audio segmentation, self-similarity matrices, morphing, etc.

# Key functions:
* Sound synthesis from R console: soundgen()
* Shiny app for sound synthesis (opens in a browser): soungen_app()
* Acoustic analysis of a .wav file: analyze()
* Measuring syllables, pauses, and bursts in a .wav file: segment()

For more information, please see the vignettes on sound synthesis and acoustic analysis:
vignette("acoustic_analysis", package="soundgen")
vignette("sound_generation", package="soundgen")

# Installation
install.packages("soundgen")

NB: Make sure all dependencies have been installed correctly! For problems with seewave, see http://rug.mnhn.fr/seewave/
On Macs, you may need to do the following:
* First install brew according to the instructions here: https://brew.sh/
* Then run the following in a terminal
    brew install libsndfile
    brew install fftw
* Finally, install soundgen in R:
  install.packages("soundgen")
