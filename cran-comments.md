## Resubmission
This is a resubmission. In this version I have:

* Reformatted package description, removing duplication of "description" and replacing all double-spaces with single spaces
* Unwrapped portions of long examples from "\dontrun{}", leaving a short example of each function for CRAN tests and keeping more extensive, longer examples within "\dontrun{}". One exception is optimizePars(), which requires a training corpus and cannot be meaningfully demonstrated in under 5 s.

## Test environments
* local Linux Mint 17.3 install, R 3.4.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:
  Maintainer: ‘Andrey Anikin <rty.anik@rambler.ru>’
  New submission

