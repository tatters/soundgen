## Test environments
* local Linux Mint 20, R 4.0.2
* rhub:
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
  - Windows Server 2008 R2 SP1, R-devel

## R CMD check results
One NOTE:
------
* checking installed package size ... NOTE
  installed size is  6.3Mb
  sub-directories of 1Mb or more:
    doc   4.4Mb
    
Comment: I do have two large vignettes (the .Rmd text alone is ~200 KB, plus illustrations) that have not increased in size over the past few updates.

"Possibly mis-spelled words in DESCRIPTION: formant" This is correct: a formant is a resonance frequency of the vocal tract. "Anikin": this is my name in the published reference to the package.
