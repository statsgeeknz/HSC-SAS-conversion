/*==================================================================================================================*/
/*Preamble code to ingest the dummy dataset*/


	%let path = C:\Users\stats\OneDrive\Documents\GitHub\HSC-SAS-conversion;

	libname	PESURVEY "&path\SAS code\SASData";
	run;

	/*= Question data*/

	PROC IMPORT OUT= PESURVEY.HACE1920_QUESTIONS DATAFILE= "&path\data\HACE1920_QUESTIONS.csv" 
	            DBMS=CSV REPLACE;
	     GETNAMES=YES;
	     DATAROW=2; 
	RUN;

	/*= Strata data*/

	PROC IMPORT OUT= PESURVEY.HACE1920_STRATA_DATA DATAFILE= "&path\data\HACE1920_STRATA_DATA - Dummy data.csv" 
	            DBMS=CSV REPLACE;
	     GETNAMES=YES;
	     DATAROW=2; 
	RUN;

	/*= Weight data*/

	PROC IMPORT OUT= PESURVEY.HACE1920_WEIGHTED DATAFILE= "&path\data\HACE1920_WEIGHTED - Dummy data.csv" 
	            DBMS=CSV REPLACE;
	     GETNAMES=YES;
	     DATAROW=2; 
	RUN;


/*HSC supplied code below here*/
/*==================================================================================================================*/


/*3. ANALYSIS - NATIONAL LEVEL*/
/*Calculates the national level results for the 2019/20 HACE survey.*/

/*To calculate national level weights for the 2019/20 HACE survey:*/
%LET Year = 1920;
/*NOTE: Year will need to be updated in the macro statements manually.*/

/*Select questions of interest by relevant iref (from 101 to 189):*/
%LET first_question = 101; /*min=101 */
%LET last_question = 153; /*max=203 */
%LET geography = NAT; /*could probably set this up to do this by different geographies, but just use NAT for now */


/*Set up strata populations:*/
DATA Strata_Pop;
	SET PESURVEY.HACE&Year._STRATA_DATA;
	_TOTAL_ = n_eligible;
	KEEP Strata _TOTAL_;
RUN;

PROC SORT DATA=Strata_Pop NODUPKEY; BY Strata; RUN;


/*Calculate the iref for the first percent positive question in the questions of interest:*/
DATA PctPosQuest_Only;
	SET PESURVEY.HACE&Year._QUESTIONS;

	IF iref < &first_question THEN DELETE;
	ELSE IF iref > &last_question THEN DELETE;

	IF QuestionType ^= "Percent positive" THEN DELETE;
RUN;

PROC MEANS NOLABELS DATA=PctPosQuest_Only MIN NOPRINT;
	VAR iref; OUTPUT OUT=First_PosPct_Quest (DROP=_FREQ_ _TYPE_);
RUN;

DATA Min_iref;
	SET First_PosPct_Quest;
	WHERE _STAT_ = "MIN";
	CALL SYMPUT ('Min_iref', TRIM(LEFT(iref)));
RUN;

data work.HACE&Year._QUESTIONS;
	set pesurvey.HACE&Year._QUESTIONS;
		geography = symget('geography');
		weight2=cats(weight,TRIM(LEFT(geography)));
		if weight="No_Weight" then weight2="No_Weight";

run;


/*Macro to calculate results for questions of interest:*/
%MACRO HACE1920_RESULTS;

	/*Loop through questions:*/
	%DO i=&first_question %TO &last_question;

	/*Define macro variables for each loop:*/
	DATA MACRO_SET_UP;
		SET work.HACE&Year._QUESTIONS;
		WHERE iref = &i;
		CALL SYMPUT ('Question', TRIM(LEFT(Question)));
		CALL SYMPUT ('QuestionType', TRIM(LEFT(QuestionType)));
		CALL SYMPUT ('Positive', TRIM(LEFT(Positive)));
		CALL SYMPUT ('Neutral', TRIM(LEFT(Neutral)));
		CALL SYMPUT ('Negative', TRIM(LEFT(Negative)));
		CALL SYMPUT ('Exclude', TRIM(LEFT(Exclude)));
		CALL SYMPUT ('Weight', TRIM(LEFT(Weight2)));
	RUN;

	PROC FORMAT;
		VALUE PosNeutNeg
			1 = '% Positive'
			2 = '% Neutral'
			3 = '% Negative';

		VALUE PercentPositive
			1 = '% Positive'
			2 = '% Not Positive';
	RUN;

	DATA Responses_&Question;
		SET PESURVEY.HACE&Year._WEIGHTED
			(KEEP= gp_prac_no n_eligible &Question. &Weight.);
		WHERE &Question NOT IN (&Exclude 995 999 .); 
		
		Strata = gp_prac_no;

		IF &Weight. = 0 THEN DELETE;

		%IF &QuestionType = Indicator OR &QuestionType = Information %THEN %GOTO skip1;

		IF &Question IN (&Positive) THEN DO;
			PercentPositive_&Question = 1;
			PosNeutNeg_&Question = 1;
		END;

		ELSE IF &Question IN (&Neutral) THEN DO;
			PercentPositive_&Question = 2;
			PosNeutNeg_&Question = 2;
		END;

		ELSE IF &Question IN (&Negative) THEN DO;
			PercentPositive_&Question = 2;
			PosNeutNeg_&Question = 3;
		END;

		FORMAT PercentPositive_&Question PercentPositive. PosNeutNeg_&Question PosNeutNeg.;

		%skip1:
	RUN;

	%IF &QuestionType = Indicator OR &QuestionType = Information %THEN %GOTO skip2;

	TITLE "Results: &Question";
	PROC SURVEYFREQ DATA=Responses_&Question TOTAL=Strata_Pop NOSUMMARY;
		TABLES &Question PosNeutNeg_&Question PercentPositive_&Question / cl row(deff) deff;
		STRATA Strata; WEIGHT &Weight;
		ODS OUTPUT OneWay=OneWayTable_&Question;
	RUN;

	%IF &i = &Min_iref %THEN %DO;
		DATA OneWay;
			SET OneWayTable_&Question; FORMAT i 8. Question $8. WgtFreq 8.2;
			Question = "&Question"; i = "&i"; 

			IF PercentPositive_&Question ^= 1 THEN DELETE;

			DROP F_&Question F_PercentPositive_&Question F_PosNeutNeg_&Question PercentPositive_&Question 
				PosNeutNeg_&Question Table _SkipLine &Question;
		RUN;

		PROC DATASETS LIBRARY=WORK NOLIST;
			DELETE OneWayTable_&Question;
		QUIT;
	%END;

	%ELSE %IF &i ^= &Min_iref %THEN %DO;
		DATA OneWayTable_&Question;
			SET OneWayTable_&Question; FORMAT i 8. Question $8. WgtFreq 8.2;
			Question = "&Question"; i = "&i";			

			IF PercentPositive_&Question ^= 1 THEN DELETE;

			DROP F_&Question F_PercentPositive_&Question F_PosNeutNeg_&Question PercentPositive_&Question 
				PosNeutNeg_&Question Table _SkipLine &Question;
		RUN;

		PROC APPEND BASE=OneWay DATA=OneWayTable_&Question; RUN;

		PROC DATASETS LIBRARY=WORK NOLIST;
			DELETE OneWayTable_&Question;
		QUIT;
	%END;

	%skip2:

	%IF &QuestionType = Indicator %THEN %DO;
		TITLE "Results: &Question";
		PROC SURVEYMEANS DATA=Responses_&Question Total=Strata_Pop;
			STRATA Strata; WEIGHT &Weight; VAR &Question;
		RUN;
	%END;

	%IF &QuestionType = Information %THEN %DO;
		TITLE "Results: &Question";
		PROC SURVEYFREQ DATA=Responses_&Question Total=Strata_Pop;
			TABLES &Question / cl row(deff) deff; STRATA Strata; WEIGHT &Weight;
		RUN;

	%END;

	PROC DATASETS LIBRARY=WORK NOLIST;
		DELETE Responses_&Question MACRO_SET_UP;
	QUIT;

	%END;

%MEND HACE1920_RESULTS;

%HACE1920_RESULTS;

PROC DATASETS LIBRARY=WORK NOLIST;
		DELETE strata_pop pctposquest_only first_pospct_quest min_iref hace1920_questions;
	QUIT;
