# ===================================================================================================================
#' Conversion of HSC-supplied SAS code to an R variant
#' 
#'  Author: CRD
#'  Created: 22/03/2021
#'  
#'   Provides functionality similar to the provided SAS code - mainly an argument-free macro within "National Summary.sas"
#'   Code below is markedly slower than SAS, due mainly to speed of the srvyr (wraps survey package)
#'   
#'   Users will have to modify several inputs in the preamble to suit their situation - currently written around the dummy CSV data provided.
#'   

# Preamble --------------------------------------------------------------------------------------------------------------------------------------

  library(tidyverse)
  library(srvyr)
  library(pbapply)
  library(parallel)
  
  setwd("R conversion/")
  
  source("tools.R")

  #= only plays a role in the names of files - redundant in the SAS example
  Year <- 1920

  #= paths - going to assume named in a consistent way, only need path and year - expect only " - Dummy data" be removed in practice
  dataPath <- "data/"
  
  questionPath <- paste0(dataPath, "HACE", Year, "_QUESTIONS.csv")
  strataPath <- paste0(dataPath, "HACE", Year, "_STRATA_DATA - Dummy data.csv")
  weightPath <- paste0(dataPath, "HACE", Year, "_WEIGHTED - Dummy data.csv")
  
  #= read in example datasets - data management in reality TBD
  HACE_Questions <- read_csv(questionPath)
  HACE_Strata <- read_csv(strataPath)
  HACE_Weighted <- read_csv(weightPath)

  #= range of questions to ingest (indexed by variable iref)
  first_question <- 101
  last_question <- 153

  #= gives a suffix for a variable called from the data
  geography <- "NAT"

  # SAS Note regarding 1 obs in PSU "Single-observation strata are not included in the variance estimates". Also passed to parallel nodes.
  options(survey.lonely.psu="remove")

  # Calculate the number of cores - optional parallelisation via makeParallel = T/F
  makeParallel <- T
  noCores <- detectCores() - 1
  myCl <- NULL # populated later if makeParallel = T
  
# Pre-macro data manipulation -------------------------------------------------------------------------------------------------------------------


  # HSC comment: "Set up strata populations"
  Strata_Pop <- HACE_Strata %>% rename(total = n_eligible) %>% select(strata, total) %>% arrange(strata) %>% rename(Strata = strata)


  # equivalent of work.HACE&Year._QUESTIONS
  workingQuestions <- HACE_Questions %>% filter(between(iref, first_question, last_question)) %>% 
             mutate(geography = geography, Weight2 = ifelse(Weight == "No_Weight", "No_Weight", paste0(Weight, geography)))

# Macro conversion to function ------------------------------------------------------------------------------------------------------------------

  # Create a list of questions to loop over - refer pbapply later
  
 questionList <- workingQuestions %>% split(., .$Question)

# Prepare parallelisation -----------------------------------------------------------------------------------------------------------------------

  if(makeParallel){
    
    # Initiate cluster
    myCl <- makeCluster(noCores)
    
    # Load necessary bits to the cluster nodes
    
    clusterEvalQ(myCl, {library(tidyverse); library(srvyr);  source("tools.r"); options(survey.lonely.psu="remove")})
    
    clusterExport(myCl, c("Strata_Pop", "processQuestions", "HACE_Weighted", "questionList"))
    
    }
  
# Extract data from "Weighted" dataset as indicated by rows of "Question" data ------------------------------------------------------------------

  # srvyr functions are a bottle-neck - parallelising to mitigate
  
  questionTables <- pbapply::pblapply(questionList[1:10], processQuestions, weightData = HACE_Weighted, strataData = Strata_Pop, cl = myCl)

# Tidy up ---------------------------------------------------------------------------------------------------------------------------------------
  
  # = aggregate lists into flat files
  
  
  
  
  # = close out the cluster nodes if needed
  
  if(makeParallel){
    
    stopCluster(myCl)
    gc()
  
    } # end if
  
    
     