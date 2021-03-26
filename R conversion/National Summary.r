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

  #= only plays a role in the names of files - redundant in the SAS example
  Year <- 1920

  #= paths - going to assume named in a consistent way, only need path and year
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

# Pre-macro data manipulation -------------------------------------------------------------------------------------------------------------------


  # HSC comment: "Set up strata populations"
  Strata_Pop <- HACE_Strata %>% rename(total = n_eligible) %>% select(strata, total) %>% arrange(strata) %>% rename(Strata = strata)


  # HSC comment: "Calculate the iref for the first percent positive question in the questions of interest:"
  PctPosQuest_Only <- HACE_Questions %>% filter(between(iref, first_question, last_question),
                                                    QuestionType == "Percent positive")
  
  # Create equiv of the SAS variable &Min_iref, which is long-winded use of symput after proc means
  Min_iref <- min(PctPosQuest_Only$iref)
  
  # equivalent of work.HACE&Year._QUESTIONS
  workingQuestions <- HACE_Questions %>% mutate(
    geography = geography, Weight2 = ifelse(Weight == "No_Weight", "No_Weight", paste0(Weight, geography)))

  

# Macro conversion to function ------------------------------------------------------------------------------------------------------------------

  # Three broad types of questions that get different treatment
  
  infoQuestions <- workingQuestions %>% filter(QuestionType == "Information") %>% split(., .$iref)
  
  indicatorQuestions <- workingQuestions %>% filter(QuestionType == "Indicator") %>% split(., .$iref)
  
  positiveQuestions <- workingQuestions %>% filter(QuestionType == "Percent positive") %>% split(., .$iref)
  
  
# Extract data from "Weighted" dataset as indicated by rows of "Question" data ------------------------------------------------------------------


  currentQuestion <- positiveQuestions[[1]]
  
  test <- processLikert(currentQuestion, HACE_Weighted)
  
  test <- pbapply::pblapply(positiveQuestions[8], processLikert, weightData = HACE_Weighted)
  
  processLikert <- function(questData, weightData){
    print(questData$iref)
  #= select & rename from weighted dataset, drop exclusions and zero weights
    workingWeights <- weightData %>% 
      select(GP_PRAC_NO, n_eligible, questData$Question, questData$Weight2) %>%
      mutate(Exclude = questData$Exclude, 
             Positive = questData$Positive, 
             Neutral = questData$Neutral,
             Negative = questData$Negative) %>%
      rename(Strata = GP_PRAC_NO, Question = questData$Question, Weight = questData$Weight2) %>%
      mutate(Question = as.character(Question)) %>%
      filter(Question != 995, Question != 999, Weight != 0) %>%
      filter(!(is.na(Exclude)==F & Exclude == Question))
    
  #= Code whether positive (1/2), or pos/neut/neg (1/2/3) depending on definition - note case_when is very slow, so nested ifelse

    workingWeights <- workingWeights %>% rowwise() %>%  
      mutate(
        PercentPositive = ifelse(grepl(Question, Positive), '% Positive', '% Not Positive'),
        PosNeutNeg = ifelse(grepl(Question, Positive), '% Positive', ""), 
        PosNeutNeg = ifelse(grepl(Question, Neutral), '% Neutral', PosNeutNeg), 
        PosNeutNeg = ifelse(grepl(Question, Negative), '% Negative', PosNeutNeg)
      )
    

  #= Generate the equivalent of the SAS one-way table, as defined:

    workingSurvey <- workingWeights %>% right_join(Strata_Pop, by = "Strata")
    
    workingSurvey <- as_survey(workingSurvey, strata = Strata, weight = Weight, fpc = total) 
    
  #= create the required summary tables
    
    questTable <- workingSurvey %>% group_by(Question) %>% summarise(Freq = n(), WeightFreq = survey_total(), pct = survey_prop(vartype = c("se", "ci"), deff = T))
    percentPosTable <- workingSurvey %>% group_by(PercentPositive) %>% summarise(Freq = n(), WeightFreq = survey_total(), pct = survey_prop(vartype = c("se", "ci"), deff = T))
    posNeutNegTable <- workingSurvey %>% group_by(PosNeutNeg) %>% summarise(Freq = n(), WeightFreq = survey_total(), pct = survey_prop(vartype = c("se", "ci"), deff = T))
   
  list(questTable, percentPosTable, posNeutNegTable)  
  
  } # end processLikert function
  
  
  
    
# Non  Indicator OR Information questions -------------------------------------------------------------------------------------------------------

  #= Note the lists for these types of questions might be empty
    
    currentQuestion <- indicatorQuestions[[1]]
 
    workingWeights <- HACE_Weighted %>% 
      select(GP_PRAC_NO, n_eligible, currentQuestion$Question, currentQuestion$Weight2) %>%
      mutate(Exclude = currentQuestion$Exclude) %>%
      rename(Strata = GP_PRAC_NO, Question = currentQuestion$Question, Weight = currentQuestion$Weight2) %>%
      mutate(Question = as.character(Question)) %>%
      filter(Question != 995, Question != 999, Weight != 0) %>%
      filter(!(is.na(Exclude)==F & Exclude == Question))
    
    test <- as_survey(workingWeights, strata = Strata, weight = Weight)
    test %>% group_by(Question) %>% summarise(pct = survey_prop(deff = T))
    
    
#     %skip2:

      
#       %IF &QuestionType = Indicator %THEN %DO;
#     TITLE "Results: &Question";
#     PROC SURVEYMEANS DATA=Responses_&Question Total=Strata_Pop;
#     STRATA Strata; WEIGHT &Weight; VAR &Question;
#     RUN;
#     %END;
    
  
    currentQuestion <- infoQuestions[[1]]
    
    
    
    workingWeights <- HACE_Weighted %>% 
      select(GP_PRAC_NO, n_eligible, currentQuestion$Question, currentQuestion$Weight2) %>%
      mutate(Exclude = currentQuestion$Exclude) %>%
      rename(Strata = GP_PRAC_NO, Question = currentQuestion$Question, Weight = currentQuestion$Weight2) %>%
      mutate(Question = as.character(Question)) %>%
      filter(Question != 995, Question != 999, Weight != 0) %>%
      filter(!(is.na(Exclude)==F & Exclude == Question))
    
    test <- as_survey(workingWeights, strata = Strata, weight = Weight)
    test %>% group_by(Question) %>% summarise(pct = survey_prop(deff = "replace"))
    
    
  
#     
#     %IF &QuestionType = Information %THEN %DO;
#     TITLE "Results: &Question";
#     PROC SURVEYFREQ DATA=Responses_&Question Total=Strata_Pop;
#     TABLES &Question / cl row(deff) deff; STRATA Strata; WEIGHT &Weight;
#     RUN;
#     
     