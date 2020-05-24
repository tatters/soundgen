## Test environments
* local Linux Mint 19.3, R 3.6.3
* rhub:
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
  - Windows Server 2008 R2 SP1, R-devel

## R CMD check results
One NOTE:
------
* checking installed package size ... NOTE
  installed size is  5.6Mb
  sub-directories of 1Mb or more:
    doc   4.3Mb
    
Comment: I do have two large vignettes (the .Rmd text alone is ~200 KB, plus illustrations) that have not increased in size over the past few updates.
