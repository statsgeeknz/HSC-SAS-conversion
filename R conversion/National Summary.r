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

  # Macro has no arguments - start with similar structure
  
  HACE1920_RESULTS <- function(){
  
    # loop over questions
    for(i in first_question:last_question){
      
      currentIref <- workingQuestions %>% filter(iref == i)
      
      Question <- currentIref$Question
      QuestionType <- currentIref$QuestionType
      Positive <- currentIref$Positive
      Neutral <- currentIref$Neutral
      Negative <- currentIref$Negative
      Exclude <- currentIref$Exclude
      Weight <- currentIref$Weight2
      
  
   
  
      
    } # end of i loop
    
  }


#     
#     PROC FORMAT;
#     VALUE PosNeutNeg
#     1 = '% Positive'
#     2 = '% Neutral'
#     3 = '% Negative';
#     
#     VALUE PercentPositive
#     1 = '% Positive'
#     2 = '% Not Positive';
#     RUN;
#     
  
  responses <- HACE1920_WEIGHTED %>% select(GP_PRAC_NO, n_eligible, as.name(Question), as.name(Weight)) %>%
    filter(as.name(Question)  == 3) 
  
      mutate(Strata = gp_prac_no) %>%
    filter(Weight != 0) %>%
    
    
    if(Question in Positive){
      
    } elseif {
      
    }
    
  
#     DATA Responses_&Question;
#     SET PESURVEY.HACE&Year._WEIGHTED
#     (KEEP= gp_prac_no n_eligible &Question. &Weight.);
#     WHERE &Question NOT IN (&Exclude 995 999 .); 
#     
#     Strata = gp_prac_no;
#     
#     IF &Weight. = 0 THEN DELETE;
#     
      
#     %IF &QuestionType = Indicator OR &QuestionType = Information %THEN %GOTO skip1;
      
#     
#     IF &Question IN (&Positive) THEN DO;
#     PercentPositive_&Question = 1;
#     PosNeutNeg_&Question = 1;
#     END;
#     
#     ELSE IF &Question IN (&Neutral) THEN DO;
#     PercentPositive_&Question = 2;
#     PosNeutNeg_&Question = 2;
#     END;
#     
#     ELSE IF &Question IN (&Negative) THEN DO;
#     PercentPositive_&Question = 2;
#     PosNeutNeg_&Question = 3;
#     END;
#     
#     FORMAT PercentPositive_&Question PercentPositive. PosNeutNeg_&Question PosNeutNeg.;
#     
#     %skip1:
#       RUN;
#     
      
      
      
      
#     %IF &QuestionType = Indicator OR &QuestionType = Information %THEN %GOTO skip2;
#     
#     TITLE "Results: &Question";
#     PROC SURVEYFREQ DATA=Responses_&Question TOTAL=Strata_Pop NOSUMMARY;
#     TABLES &Question PosNeutNeg_&Question PercentPositive_&Question / cl row(deff) deff;
#     STRATA Strata; WEIGHT &Weight;
#     ODS OUTPUT OneWay=OneWayTable_&Question;
#     RUN;
#     
      
      
#     %IF &i = &Min_iref %THEN %DO;
#     DATA OneWay;
#     SET OneWayTable_&Question; FORMAT i 8. Question $8. WgtFreq 8.2;
#     Question = "&Question"; i = "&i"; 
#     
#     IF PercentPositive_&Question ^= 1 THEN DELETE;
#     
#     DROP F_&Question F_PercentPositive_&Question F_PosNeutNeg_&Question PercentPositive_&Question 
#     PosNeutNeg_&Question Table _SkipLine &Question;
#     RUN;
#     
      
      
#     PROC DATASETS LIBRARY=WORK NOLIST;
#     DELETE OneWayTable_&Question;
#     QUIT;
#     %END;
#     
      
      
#     %ELSE %IF &i ^= &Min_iref %THEN %DO;
#     DATA OneWayTable_&Question;
#     SET OneWayTable_&Question; FORMAT i 8. Question $8. WgtFreq 8.2;
#     Question = "&Question"; i = "&i";			
#     
#     IF PercentPositive_&Question ^= 1 THEN DELETE;
#     
#     DROP F_&Question F_PercentPositive_&Question F_PosNeutNeg_&Question PercentPositive_&Question 
#     PosNeutNeg_&Question Table _SkipLine &Question;
#     RUN;
#   
      
      
      
#     PROC APPEND BASE=OneWay DATA=OneWayTable_&Question; RUN;
#     
#     PROC DATASETS LIBRARY=WORK NOLIST;
#     DELETE OneWayTable_&Question;
#     QUIT;
#     %END;
#    
      
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
#     %END;
#     
#     PROC DATASETS LIBRARY=WORK NOLIST;
#     DELETE Responses_&Question MACRO_SET_UP;
#     QUIT;
#     
#     %END;
#     
#     %MEND HACE1920_RESULTS;
#     
#     %HACE1920_RESULTS;
#     
#     PROC DATASETS LIBRARY=WORK NOLIST;
#     DELETE strata_pop pctposquest_only first_pospct_quest min_iref hace1920_questions;
#     QUIT;
#     