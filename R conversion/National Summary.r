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

  #= read in example datasets - data management in reality TBD
  HACE1920_QUESTIONS <- read_csv("data/HACE1920_QUESTIONS.csv")
  HACE1920_STRATA_DATA <- read_csv("data/HACE1920_STRATA_DATA - Dummy data.csv")
  HACE1920_WEIGHTED <- read_csv("data/HACE1920_WEIGHTED - Dummy data.csv")


  Year <- 1920
  first_question <- 101
  last_question <- 153
  geography <- "NAT"



# Pre-macro data manipulation -------------------------------------------------------------------------------------------------------------------


  # HSC comment: "Set up strata populations"
  Strata_Pop <- HACE1920_STRATA_DATA %>% rename(total = n_eligible) %>% select(strata, total) %>% arrange(strata)


  # HSC comment: "Calculate the iref for the first percent positive question in the questions of interest:"
  PctPosQuest_Only <- HACE1920_QUESTIONS %>% filter(between(iref, first_question, last_question),
                                                    QuestionType == "Percent positive")
  
  # Create equiv of the SAS variable &Min_iref, which is long-winded use of symput after proc means
  Min_iref <- min(PctPosQuest_Only$iref)
  
  # equivalent of work.HACE&Year._QUESTIONS
  workingQuestions <- HACE1920_QUESTIONS %>% mutate(
    geography = geography, Weight2 = ifelse(Weight == "No_Weight", "No_Weight", paste0(Weight, geography)))

  

# Macro conversion to function ------------------------------------------------------------------------------------------------------------------

  # Three broad types of questions that get different treatment
  
  infoQuestions <- workingQuestions %>% filter(QuestionType == "Information") %>% split(., .$iref)
  
  indicatorQuestions <- workingQuestions %>% filter(QuestionType == "Indicator") %>% split(., .$iref)
  
  positiveQuestions <- workingQuestions %>% filter(QuestionType == "Percent positive") %>% split(., .$iref)
  
  
# Extract data from "Weighted" dataset as indicated by rows of "Question" data ------------------------------------------------------------------


    currentQuestion <- positiveQuestions[[1]]
  
    
  #= select & rename from weighted dataset, drop exclusions and zero weights
    workingWeights <- HACE1920_WEIGHTED %>% 
      select(GP_PRAC_NO, n_eligible, currentQuestion$Question, currentQuestion$Weight2) %>%
      mutate(Exclude = currentQuestion$Exclude, 
             Positive = currentQuestion$Positive, 
             Neutral = currentQuestion$Neutral,
             Negative = currentQuestion$Negative) %>%
      rename(Strata = GP_PRAC_NO, Question = currentQuestion$Question, Weight = currentQuestion$Weight2) %>%
      mutate(Question = as.character(Question)) %>%
      filter(Question != 995, Question != 999, Weight != 0) %>%
      filter(!(is.na(Exclude)==F & Exclude == Question))
    
  #= Code whether positive (1/2), or pos/neut/neg (1/2/3) depending on definition 
   
    workingWeights <- workingWeights %>%
      rowwise() %>%
      mutate(
            PercentPositive = ifelse(grepl(Question, Positive), '% Positive', '% Not Positive'),
             PosNeutNeg = case_when(
               grepl(Question, Positive) ~ '% Positive',
               grepl(Question, Neutral) ~ '% Neutral',
               grepl(Question, Negative) ~ '% Negative'
             )
      )
    
    
  #= Generate the equivalent of the SAS one-way table, as defined:
    #     TITLE "Results: &Question";
    #     PROC SURVEYFREQ DATA=Responses_&Question TOTAL=Strata_Pop NOSUMMARY;
    #     TABLES &Question PosNeutNeg_&Question PercentPositive_&Question / cl row(deff) deff;
    #     STRATA Strata; WEIGHT &Weight;
    
    test <- as_survey(workingWeights, strata = Strata, weight = Weight)
    test %>% group_by(Question) %>% summarise(pct = survey_prop(deff = "replace"))
   
    
    
# Non  Indicator OR Information questions -------------------------------------------------------------------------------------------------------

 
      
 
      
#     %skip2:

      
#       %IF &QuestionType = Indicator %THEN %DO;
#     TITLE "Results: &Question";
#     PROC SURVEYMEANS DATA=Responses_&Question Total=Strata_Pop;
#     STRATA Strata; WEIGHT &Weight; VAR &Question;
#     RUN;
#     %END;
    
    
    
    
    
#     
#     %IF &QuestionType = Information %THEN %DO;
#     TITLE "Results: &Question";
#     PROC SURVEYFREQ DATA=Responses_&Question Total=Strata_Pop;
#     TABLES &Question / cl row(deff) deff; STRATA Strata; WEIGHT &Weight;
#     RUN;
#     
     