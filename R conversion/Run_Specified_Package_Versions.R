#================================================================================================================================
#' Testing works on older versions - run through RStudio 1.2.5033 (actually 1.2.5042 - other is not achived)

  library(devtools)
  
  install_version("tidyverse", version = "1.3.0", repos = "http://cran.us.r-project.org")
  install_version("srvyr", version = "1.0.0", repos = "http://cran.us.r-project.org")
  install_version("pbapply", version = "1.4-2", repos = "http://cran.us.r-project.org")
  install_version("plyr", version = "1.8.6", repos = "http://cran.us.r-project.org")
  
  


  source("National Summary.r")
  
  