## Test environments
* local Linux Mint 17.3 install, R 3.4.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Comments
CRAN checks of the released version 1.0.0 show that installation of soundgen failed on Mac OS X because dependency "seewave" was not available. Is there any way to deal with this problem? Seewave is a major, well-established package for working with sounds in R. The maintainer of seewave offers specific advice for Mac users here: http://rug.mnhn.fr/seewave/

