## Test environments
* local Linux Mint 20, R 4.0.2
* rhub:
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
  - Windows Server 2008 R2 SP1, R-devel

## R CMD check results
One or two NOTEs:
------
* checking installed package size ... NOTE
  installed size is  6.4Mb
  sub-directories of 1Mb or more:
    doc   4.5Mb
Comment: I do have two large vignettes (the .Rmd text alone is ~200 KB, plus illustrations) that have not increased in size over the past few updates.
    
* checking for future file timestamps ... NOTE
unable to verify current time
Comment: seems to be some server issue, nothing to do with the package
