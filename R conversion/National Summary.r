# ===================================================================================================================
#' Conversion of HSC-supplied SAS code to an R variant
#' 
#'  Author: CRD
#'  Created: 22/03/2021
#'  
#'   
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

  #= paths - going to assume named in a consistent way, only need path and year
  dataPath <- "../data/"
  
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

  # SAS Note regarding 1 obs in PSU "Single-observation strata are not included in the variance estimates"
  options(survey.lonely.psu="remove")

  # Calculate the number of cores
  noCores <- detectCores() - 1
  
# Pre-macro data manipulation -------------------------------------------------------------------------------------------------------------------


  # HSC comment: "Set up strata populations"
  Strata_Pop <- HACE_Strata %>% rename(total = n_eligible) %>% select(strata, total) %>% arrange(strata) %>% rename(Strata = strata)


  # equivalent of work.HACE&Year._QUESTIONS
  workingQuestions <- HACE_Questions %>% 
    filter(between(iref, first_question, last_question)) %>% 
             mutate(geography = geography, 
                    Weight2 = ifelse(Weight == "No_Weight", "No_Weight", paste0(Weight, geography))
                    )

# Macro conversion to function ------------------------------------------------------------------------------------------------------------------

  # Three broad types of questions that get different treatment
  
  infoQuestions <- workingQuestions %>% filter(QuestionType == "Information") %>% split(., .$Question)
  
  indicatorQuestions <- workingQuestions %>% filter(QuestionType == "Indicator") %>% split(., .$Question)
  
  positiveQuestions <- workingQuestions %>% filter(QuestionType == "Percent positive") %>% split(., .$Question)
  
  

# Prepare parallelisation -----------------------------------------------------------------------------------------------------------------------

  # Initiate cluster
  myCl <- makeCluster(noCores)
  
  # Load necessary bits to the cluster nodes
  
  clusterEvalQ(myCl, {library(tidyverse); library(srvyr);  source("tools.r"); options(survey.lonely.psu="remove")})
  
  clusterExport(myCl, c("Strata_Pop", "processLikert", "HACE_Weighted", "positiveQuestions", "indicatorQuestions", "infoQuestions"))
  
# Extract data from "Weighted" dataset as indicated by rows of "Question" data ------------------------------------------------------------------

  # srvyr functions are a bottle-neck - parallelising to mitigate
  
  positiveQTables <- pbapply::pblapply(positiveQuestions, processLikert, weightData = HACE_Weighted, strataData = Strata_Pop, cl = myCl)

# Non  Indicator OR Information questions -------------------------------------------------------------------------------------------------------

  #= Note the lists for these types of questions might be empty
  
  if(length(indicatorQuestions) > 0) {  
    
    indicatorQTables <- pbapply::pblapply(indicatorQuestions, processInformation, weightData = HACE_Weighted, strataData = Strata_Pop, cl = myCl)
  
    }

  #= Note the lists for these types of questions might be empty
  
  if(length(infoQuestions) > 0) {  
    
    infoQTables <- pbapply::pblapply(infoQuestions, processInformation, weightData = HACE_Weighted, strataData = Strata_Pop, cl = myCl)
    
  }
  
  
  
# Tidy up ---------------------------------------------------------------------------------------------------------------------------------------

  stopCluster(myCl)
  gc()
  
    
     