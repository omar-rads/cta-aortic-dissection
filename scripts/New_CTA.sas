dm 'log;clear;output;clear;odsresults;clear';
options mprint nodate pageno=1;
%SYSMSTORECLEAR;

/* Set the library */
libname lib 'Y:\Documents\Research Folder\SAS Folder\CTA_Study';
/* Check the contents */
proc contents data=lib.ASER; run;

data final;
    set lib.ASER;

    length ChiefGroup $30 
           Dispo_Clean $20 
           ED_Dispo_Clean $30 
           RaceGroup $20 
           ClinicalPainRisk $10 
           UrgencyGroup $10
			ProviderGroup $12;

    /* ---- Discharge Disposition (Clean 4-group version) ---- */
    if Discharge_Disposition_DESC = "HOME SELF CARE" then Dispo_Clean = "Home";
    else if Discharge_Disposition_DESC = "LEFT AMA" then Dispo_Clean = "Left AMA";
    else if Discharge_Disposition_DESC = "EXPIRED" then Dispo_Clean = "Expired";
    else Dispo_Clean = "Transferred/Other";

    /* ---- ED Disposition (Collapsed for Table 1) ---- */
    if ED_Disposition = "Discharge" then ED_Dispo_Clean = "Home";
    else if ED_Disposition in ("Admit", "Transfer") then ED_Dispo_Clean = "Admitted/Transferred";
    else if ED_Disposition = "Discharge to SNF" then ED_Dispo_Clean = "Post-Acute Facility";
    else if ED_Disposition in ("AMA", "Left W/out Being Seen") then ED_Dispo_Clean = "Left AMA/LWBS";
    else if ED_Disposition = "Expired" then ED_Dispo_Clean = "Expired";
    else ED_Dispo_Clean = "Unknown";

    /* ---- Chief Complaint Grouping ---- */
    if index(lowcase(ED_Chief_Complaint), "chest pain") > 0 then ChiefGroup = "Chest Pain";
    else if index(lowcase(ED_Chief_Complaint), "abdominal") > 0 or 
             lowcase(ED_Chief_Complaint) in ("bloating", "diarrhea", "constipation") then ChiefGroup = "Abdominal Pain";
    else if index(lowcase(ED_Chief_Complaint), "back pain") > 0 then ChiefGroup = "Back Pain";
    else if index(lowcase(ED_Chief_Complaint), "flank") > 0 then ChiefGroup = "Flank Pain";
    else if index(lowcase(ED_Chief_Complaint), "shortness") > 0 or 
             index(lowcase(ED_Chief_Complaint), "sob") > 0 or 
             index(lowcase(ED_Chief_Complaint), "dyspnea") > 0 or 
             index(lowcase(ED_Chief_Complaint), "cough") > 0 then ChiefGroup = "SOB / Respiratory";
    else if index(lowcase(ED_Chief_Complaint), "syncope") > 0 or 
             index(lowcase(ED_Chief_Complaint), "fainting") > 0 or 
             index(lowcase(ED_Chief_Complaint), "mental status") > 0 or 
             index(lowcase(ED_Chief_Complaint), "dizziness") > 0 then ChiefGroup = "Syncope / AMS";
    else if index(lowcase(ED_Chief_Complaint), "weakness") > 0 or 
             index(lowcase(ED_Chief_Complaint), "fatigue") > 0 or 
             index(lowcase(ED_Chief_Complaint), "general") > 0 or 
             index(lowcase(ED_Chief_Complaint), "body aches") > 0 then ChiefGroup = "Weakness / Fatigue";
    else if index(lowcase(ED_Chief_Complaint), "headache") > 0 or 
             index(lowcase(ED_Chief_Complaint), "ha") > 0 then ChiefGroup = "Headache";
    else if index(lowcase(ED_Chief_Complaint), "fall") > 0 or 
             index(lowcase(ED_Chief_Complaint), "mvc") > 0 or 
             index(lowcase(ED_Chief_Complaint), "gun") > 0 or 
             index(lowcase(ED_Chief_Complaint), "trauma") > 0 then ChiefGroup = "Trauma";
    else if index(lowcase(ED_Chief_Complaint), "dysuria") > 0 or 
             index(lowcase(ED_Chief_Complaint), "hematuria") > 0 or 
             index(lowcase(ED_Chief_Complaint), "testicular") > 0 or 
             index(lowcase(ED_Chief_Complaint), "pelvic") > 0 then ChiefGroup = "GU Complaints";
    else if lowcase(ED_Chief_Complaint) = "not recorded" then ChiefGroup = "Not Recorded";
    else ChiefGroup = "Other";

    /* ---- Clinical Pain Risk ---- */
 if strip(Highriskpain) in ('1', 'chest pain (severe)') then ClinicalPainRisk = 'Yes';
    else if strip(Highriskpain) = '0' then ClinicalPainRisk = 'No';
    else ClinicalPainRisk = 'Missing';

    /* ---- Urgency ---- */
    if strip(Urgency) = 'Stat' then UrgencyGroup = 'STAT';
    else UrgencyGroup = 'Non-STAT';

    /* ---- Race Grouping ---- */
    if Race = 'African American or Black' then RaceGroup = 'Black';
    else if Race = 'Caucasian or White' then RaceGroup = 'White';
    else if Race = 'Asian' then RaceGroup = 'Asian';
    else if Race in (
        'American Indian or Alaskan Native',
        'Multiple',
        'Native Hawaiian or Other Pacific Islander'
    ) then RaceGroup = 'Other/Multiracial';
    else if Race = 'Unknown, Unavailable or Unreported' then RaceGroup = 'Unknown';
    else RaceGroup = 'Missing';

if ordering_provider_type in (
        'ED Physician Director',
        'ED Provider',
        'Physician',
        'Physician Orthopaedic',
        'Resident',
        'Resident Medicine',
        'Resident Results Route to Resident'
    ) then ProviderGroup = 'Physician';

    else if ordering_provider_type in (
        'ED MLP',
        'Nurse Practitioner w Rx Emergency Medicine',
        'Nurse Practitioner wo Rx Emergency Medicine',
        'Physicians Assistant CT Surgery',
        'Physicians Assistant Emergency Medicine'
    ) then ProviderGroup = 'Midlevel';

    else ProviderGroup = 'Other';

run;
*clumped up the two dif CT chest into 1*;
data ASER1;
set final;
if OrderedItem in ('CTA Chest w/ + w/o Contrast', 'CTA Chest w/ + w/o Contrast (Aorta)') then
        ScanType = 'Chest'; *Clumping two different CT chests into 1*;
    else if OrderedItem = 'CTA Abdomen + Pelvis w/Contrast' then
        ScanType = 'AbdPelvis';

 Is_phys_clean = upcase(Is_the_ordering_personnel_a_phys);

 length PositiveFlag 8;
if lowcase(strip(Positive_Y_N_)) = 'y' then PositiveFlag = 1;
    else PositiveFlag = 0;
run;
*Confirmed that the case didn't matter anymore and they were collapsed into one group*;
proc freq data= ASER1;
tables ordering_provider_type;
run;

/* Sort the data by Encounter */
proc sort data=ASER1 out=ASER_sorted;
	by Encounter;
run;

/* Process by encounter to identify complete studies */
data CompleteAASStudies;
	set ASER_sorted;
	by Encounter; /*<-- Tells SAS to process in Encounter groups*/

	/* These variables will hold their values across rows for one encounter */
	retain HasChest HasAbdPelvis 0;

	/* When SAS sees the FIRST row for an encounter, reset the flags */
	if first.Encounter then do;
		HasChest = 0;
		HasAbdPelvis = 0;
	end;

	/* For each row, check the ScanType and set the flag to 1 if it matches */
	if ScanType = 'Chest' then HasChest = 1;
	if ScanType = 'AbdPe' then HasAbdPelvis = 1;

	/* When SAS sees the LAST row for an encounter, make a decision */
	if last.Encounter then do;
		if HasChest = 1 and HasAbdPelvis = 1 then do;
			AAS_Protocol_Complete = 1; /* Create a new variable indicating a complete study */
			output; /* Write this single row to the new dataset */
		end;
	end;

	/* Keep only the variables you need for the final dataset - so i must add more if i want to keep them */
	keep Encounter AAS_Protocol_Complete Is_phys_clean PositiveFlag age gender 
			RaceGroup Dispo_Clean ED_Dispo_Clean ChiefGroup ClinicalPainRisk UrgencyGroup ProviderGroup
			Healthcare_Entity High_risk_physical_exam_features High_risk_conditions_for_AD_ 
			High_risk_clinical_pain_features ordering_provider_type Positive_Y_N_;

run;

proc freq data=CompleteAASStudies;
    tables 
        Is_phys_clean * Dispo_Clean
        Is_phys_clean * ED_Dispo_Clean
        Is_phys_clean * ChiefGroup
        Is_phys_clean * ClinicalPainRisk
        Is_phys_clean * UrgencyGroup
        Is_phys_clean * RaceGroup
        Is_phys_clean * Gender
        Is_phys_clean * Healthcare_Entity
        Is_phys_clean * ClinicalPainRisk
        Is_phys_clean *High_risk_conditions_for_AD_
        Is_phys_clean *High_risk_physical_exam_features
		Is_phys_clean *High_risk_clinical_pain_features
    
    / chisq;
run;

data phys0;
set CompleteAASStudies;
if Is_phys_clean='Y' then delete;
run;

proc freq data=Phys0;
tables ordering_provider_type;
run;
data phys1;
set CompleteAASStudies;
if Is_phys_clean='N' then delete;
run;
proc freq data=Phys1;
tables ordering_provider_type;
run;

proc freq data =completeAASStudies;
tables Is_phys_clean*PositiveFlag/chisq; 
*here we compared, for all the complete AAS studies, was there a difference in correct scans by provider*;
run;

proc ttest data=CompleteAASStudies;
class Is_phys_clean;
var age;
run;

proc contents data=CompleteAASStudies; run;


***Above this is for compelte***;

*-- Diagnostic Step 1: Check the actual values in ScanType --*;
proc freq data=ASER_sorted;
	tables ScanType / missing; /* The '/ missing' option will also show if there are any missing values */
run;

*-- Diagnostic Step 2: Check for unique vs. total encounters --*;
proc sort data=ASER_sorted out=UniqueEncounters nodupkey;
	by Encounter; *1704 unique encounters - which means *;
run;

/* Process by encounter to identify complete studies */
data IncompleteAASStudies;
	set ASER_sorted;
	by Encounter; /*<-- Tells SAS to process in Encounter groups*/

	/* These variables will hold their values across rows for one encounter */
	retain HasChest HasAbdPelvis 0;

	/* When SAS sees the FIRST row for an encounter, reset the flags */
	if first.Encounter then do;
		HasChest = 0;
		HasAbdPelvis = 0;
	end;

	/* For each row, check the ScanType and set the flag to 1 if it matches */
	if ScanType = 'Chest' then HasChest = 1;
	if ScanType = 'AbdPe' then HasAbdPelvis = 1;

	/* When SAS sees the LAST row for an encounter, make a decision */
	if last.Encounter then do;
		if HasChest = 0 or HasAbdPelvis = 0 then do;
			AAS_Protocol_Incomplete = 1; /* Create a new variable indicating a complete study */
			output; /* Write this single row to the new dataset */
		end;
	end;

	/* Keep only the variables you need for the final dataset - so i must add more if i want to keep them */
	keep Encounter AAS_Protocol_Incomplete Is_phys_clean PositiveFlag scantype age;

run;

proc freq data =IncompleteAASStudies;
tables scantype; 
run;



proc freq data=CompleteAASStudies;
tables is_phys_clean*Positive_Y_N_ / chisq;
run;

data final_noresidents;
    set CompleteAASStudies;
    if ordering_provider_type not in (
        'Resident',
        'Resident Medicine',
        'Resident Results Route to Resident'
    );
run;

proc freq data=final_noresidents;
tables is_phys_clean*Positive_Y_N_ / chisq;
run;
***New Study Below this***;

proc contents data=CompleteAASStudies; run;

*Checking the distribution of the variables on interest*;
proc freq data=CompleteAASStudies;
    tables High_risk_physical_exam_features
           High_risk_conditions_for_AD_
           High_risk_clinical_pain_features / missing;
run;

data ADDRS;
set completeAASstudies;
if strip(high_risk_physical_exam_features)   = '1' then exam_flag  = 1;
else                                            exam_flag  = 0;
if strip(High_risk_conditions_for_AD_)   = '1' then condition_flag  = 1;
else                                            exam_flag  = 0;
if strip(High_risk_clinical_pain_features)   = '1' then pain_flag  = 1;
else                                            exam_flag  = 0;

ADD_RS = sum(exam_flag, condition_flag, pain_flag);
/* SUM treats missing as 0 automatically */

if ADD_RS > 1 then Appropriate = 1;
else               Appropriate = 0;

run;

proc freq data=ADDRS;
    tables ADD_RS appropriate;
run;

proc freq data=ADDRS;
    tables Is_phys_clean * Appropriate / chisq;
run;

data ADDfinal_noresidents;
    set ADDRS;
    if ordering_provider_type not in (
        'Resident',
        'Resident Medicine',
        'Resident Results Route to Resident'
    );
run;

proc freq data=ADDfinal_noresidents;
    tables Is_phys_clean * Appropriate / chisq;
run;
