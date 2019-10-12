## Test environments
* local Linux Mint 19.1, R 3.6.1
* rhub:
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
  - Windows Server 2008 R2 SP1, R-release, 32/64 bit
  - macOS 10.11 El Capitan, R-release (experimental)

## R CMD check results
One NOTE on all platforms except macOS:

NOTES:
------
* checking installed package size ... NOTE
  installed size is  5.1Mb
  sub-directories of 1Mb or more:
    doc   3.9Mb
    
Comment: I do have two large vignettes (the .Rmd text alone is ~200 KB, plus illustrations). They have not increased in size over the past few updates. I cannot realistically shrink them below 1 MB because every demonstrated sound manipulation requires at least a visualization if not an accompanying audio file. I could remove them from the package and store them on my website instead, although then they would be harder for users to find. I'll let CRAN decide!
