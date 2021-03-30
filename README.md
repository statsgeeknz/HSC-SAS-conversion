# HSC-SAS-conversion
This repo contains code for the conversion of SAS to R for HSC (_pers. comm._ E. Smith, HSC Analysis, March 2021).

## Contents

The repo contains the following:

* Folder `SAS code` - contains the original SAS macro (and prelim code) within `National Summary.sas`. Also contains dummy datasets in SAS .sas7bdat format.
* Folder `R conversion` - contains an R-version of the same. The main file is in `National Summary.r`, with associated `tools.r`, which is sourced within the former.
* Folder `Data` - contains the test/dummy data as originally provided in CSV format.
* Folder `Docs` - documentation e.g. r-markdown with an example run and notes.

## Notes

* The SAS code was run in version 9.4 for windows (64-bit) for the purposes of conversion.

* The R code has been written to run on version 3.6.3 

* The I/O for the users is not part of the specification (e.g. input  data filenames, their locations, format, output locations), so modifications to the preamble will be required. Currently this sets the working directory to `R conversion`, with a data folder contained within (in keeping with running from the `.rproj` in the repo base directory). 

* The R version is markedly slower than SAS. The R-package `srvyr` is used and summary tables from this are currently a limiting factor. To mitigate, the R code is parallelised. As a rough indication, SAS completes on the test sets in the order of 10-20 seconds on a 10th-Gen i7 laptop, whereas R is near 3 minutes. Parallelisation over 7 cores brings this to near 1 minute. 

* PSU with only 1 observation cannot provide a variance estimate. In SAS the log notes "Single-observation strata are not included in the variance estimates" but is not immediately explicit in the helpfiles. The R-version uses the option `options(survey.lonely.psu="remove")` which appears to give the same effect.

* The SAS code includes an IF for a question type of "Indicator" which does not appear in the data. This is not replicated in the R code (but there is a catch for alternative question types, returning NULL).

* Code has been tested using:
  * R version 3.6.3 (includes `parallel` v 3.6.3)
  * R Studio 1.2.5042 (vs the requested 1.2.5033 - not archived)
  * `tidyverse` version 1.3.0
  * `srvyr`, version 1.0.0
  * `pbapply`, version 1.4-2
  * `plyr`, version 1.8.6

All legacy packages downloaded from http://cran.us.r-project.org (accessed 30/03/2021). R and RStudio from their respective websites.
  
  
