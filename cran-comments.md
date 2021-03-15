## Test environments
* local Linux Mint 20.1, R 4.0.2
* rhub:
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  - Windows Server 2008 R2 SP1, R-devel
* check_win_devel

## R CMD check results
One NOTE on all platforms:
------
* checking installed package size ... NOTE
  installed size is  7.0Mb
  sub-directories of 1Mb or more:
    doc   4.9Mb
Comment: I do have two large vignettes (the .Rmd text alone is ~200 KB, plus illustrations).
    
* checking examples ... NOTEs (on Linux in rhub)
Several examples with CPU (user + system) or elapsed time > 5s
Comment: I think rhub is unusually slow (I tried twice) - the build time for Ubuntu was >12 h, and I never heard back about the Fedora version
