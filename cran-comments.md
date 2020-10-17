## Test environments
* local Linux Mint 20, R 4.0.2
* rhub:
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
  - Windows Server 2008 R2 SP1, R-devel
* check_win_devel

## R CMD check results
One NOTE on all platforms:
------
* checking installed package size ... NOTE
  installed size is  6.4Mb
  sub-directories of 1Mb or more:
    doc   4.5Mb
Comment: I do have two large vignettes (the .Rmd text alone is ~200 KB, plus illustrations) that have not increased in size over the past few updates.
    
* checking examples ... NOTE (on Linux in rhub)
Examples with CPU (user + system) or elapsed time > 5s
Comment: I think rhub is just unusually slow today - the build time was ~1.5 h
