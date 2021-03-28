# ==================================================================================================================
#' Functions/tools for the HSC SAS conversion
#' Essentially 3 functions to address questions of type "Perent positive", "Information" or "Indicator"



# processLikert function ------------------------------------------------------------------------------------------------------------------------
  #' Function to process questions of type "Percent positive", in the HACEYYYY_QUESTIONS dataset
  #' 
  #' Matches rows of the questions dataset to columns of the weights dataset.
  #' 
  #' Args:
  #'  - questData: a row of data from the questions dataset. Ought to be filtered prior so QuestionType == "Percent positive"
  #'  - weightData: the XX..XX_WEIGHTED dataset. Has columns of responses to the particular questions and GP_PRAC_NO,	, T5_Wt_FinalNAT, n_eligible
  #'  - strataData: the XX..XX_STRATA dataset, has cols for strata and n_eligible (the stata_pop of the SAS code)

  #'  
  #' Value:
  #'  - Returns list with 3 tables from running summaries from srvyr giving:
  #'    - questTable: response levels of the input question
  #'    - percentPosTable: input question responses codes as postive or negative
  #'    - posNeutNegTable: input question responses codes as postive, neutral or negative
  
  
  processLikert <- function(questData, weightData, strataData){
    
    # For debugging purposes
    print(paste0("iref: ", questData$iref, " - Question:", questData$Question))
    
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
    
    workingSurvey <- workingWeights %>% inner_join(strataData, by = "Strata")
    
    workingSurvey <- as_survey(workingSurvey, strata = Strata, weight = Weight, fpc = total) 
    
    #= create the required summary tables
    
    questTable <- workingSurvey %>% group_by(Question) %>% summarise(Freq = n(), WeightFreq = survey_total(), pct = survey_prop(vartype = c("se", "ci"), deff = T))
    percentPosTable <- workingSurvey %>% group_by(PercentPositive) %>% summarise(Freq = n(), WeightFreq = survey_total(), pct = survey_prop(vartype = c("se", "ci"), deff = T))
    posNeutNegTable <- workingSurvey %>% group_by(PosNeutNeg) %>% summarise(Freq = n(), WeightFreq = survey_total(), pct = survey_prop(vartype = c("se", "ci"), deff = T))
    
    list(questTable, percentPosTable, posNeutNegTable)  
    
  } # end processLikert function
  

  
# processIndicator function ------------------------------------------------------------------------------------------------------------------------
  #' Function to process questions of type "Indicator", in the HACEYYYY_QUESTIONS dataset
  #' 
  #' Matches rows of the questions dataset to columns of the weights dataset.
  #' 
  #' Args:
  #'  - questData: a row of data from the questions dataset. Ought to be filtered prior so QuestionType == "Percent positive"
  #'  - weightData: the XX..XX_WEIGHTED dataset. Has columns of responses to the particular questions and GP_PRAC_NO,	, T5_Wt_FinalNAT, n_eligible
  #'  - strataData: the XX..XX_STRATA dataset, has cols for strata and n_eligible (the stata_pop of the SAS code)
  
  #'  
  #' Value:
  #'  - Returns table from running summaries from srvyr
  
  processIndicator <- function(questData, weightData, strataData){
    
    # For debugging purposes
    print(paste0("iref: ", questData$iref, " - Question:", questData$Question))
    
    #= select & rename from weighted dataset, drop exclusions and zero weights
    workingWeights <- weightData %>% 
      select(GP_PRAC_NO, n_eligible, questData$Question, questData$Weight2) %>%
      mutate(Exclude = questData$Exclude) %>%
      rename(Strata = GP_PRAC_NO, Question = questData$Question, Weight = questData$Weight2) %>%
      mutate(Question = as.character(Question)) %>%
      filter(Question != 995, Question != 999, Weight != 0) %>%
      filter(!(is.na(Exclude)==F & Exclude == Question))
    
    #= Generate the equivalent of the SAS one-way table, as defined:
    
    workingSurvey <- workingWeights %>% inner_join(strataData, by = "Strata")
    
    workingSurvey <- as_survey(workingSurvey, strata = Strata, weight = Weight, fpc = total) 
    
    #= create the required summary tables
    
    questTable <- workingSurvey %>% group_by(Question) %>% summarise(Freq = n(), WeightMean = survey_mean(), pct = survey_prop(vartype = c("se", "ci"), deff = T))
    
    list(questTable)  
  
  } # end of processIndicator
  
  
  
# processInformation function ------------------------------------------------------------------------------------------------------------------------
  #' Function to process questions of type "Information", in the HACEYYYY_QUESTIONS dataset
  #' 
  #' Matches rows of the questions dataset to columns of the weights dataset.
  #' 
  #' Args:
  #'  - questData: a row of data from the questions dataset. Ought to be filtered prior so QuestionType == "Percent positive"
  #'  - weightData: the XX..XX_WEIGHTED dataset. Has columns of responses to the particular questions and GP_PRAC_NO,	, T5_Wt_FinalNAT, n_eligible
  #'  - strataData: the XX..XX_STRATA dataset, has cols for strata and n_eligible (the stata_pop of the SAS code)
  
  #'  
  #' Value:
  #'  - Returns table from running summaries from srvyr
  
  
  processInformation <- function(questData, weightData, strataData){
    
    # For debugging purposes
    print(paste0("iref: ", questData$iref, " - Question:", questData$Question))
    
    #= select & rename from weighted dataset, drop exclusions and zero weights
    workingWeights <- weightData %>% 
      select(GP_PRAC_NO, n_eligible, questData$Question, questData$Weight2) %>%
      mutate(Exclude = questData$Exclude) %>%
      rename(Strata = GP_PRAC_NO, Question = questData$Question, Weight = questData$Weight2) %>%
      mutate(Question = as.character(Question)) %>%
      filter(Question != 995, Question != 999, Weight != 0) %>%
      filter(!(is.na(Exclude)==F & Exclude == Question))
    
    #= Generate the equivalent of the SAS one-way table, as defined:
    
    workingSurvey <- workingWeights %>% inner_join(strataData, by = "Strata")
    
    workingSurvey <- as_survey(workingSurvey, strata = Strata, weight = Weight, fpc = total) 
    
    #= create the required summary tables
    
    questTable <- workingSurvey %>% group_by(Question) %>% summarise(Freq = n(), WeightFreq = survey_total(), pct = survey_prop(vartype = c("se", "ci"), deff = T))
    
    list(questTable)  
    
  } # end processInformation function
  
  
  