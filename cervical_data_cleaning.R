##############################################################
#### This is R code to generate cervical quarterly report ####
##############################################################

## put the following files in the working directory ##
# patient_cervical_index.rda #
# patient_cervical.csv #
# eq5dscore.csv #
# cervical_report.rnw #

## create the following two folders in the working directory ##
# figs #
# bayes #

## generated pdf reports will be in the working directory ##

## Possible values for reportType are 'Quarter', 'Fast Quarter'
## and 'Annual'

rm(list=ls())

### set working directory###
setwd("C:\\Users\\karacb1\\Desktop\\QOD-reporting")

library(Hmisc)
library(rms)
library(MASS)
library(survival)
library(plyr)
#################################################
#### organize data set and prepare variables ####
#################################################

### read in Thomas' index data ###
load('data\\patient_cervical_index.rda')

# download date #
dldate <- as.character(as.Date(DataTimestamp))

d$death3m <- ifelse(d$deadhospital %in% TRUE | d$dead30day %in% TRUE | d$dead3month %in% TRUE, TRUE, FALSE)
d$analyzed_3month <- ifelse(d$analysis3month %in% TRUE | d$death3m %in% TRUE, TRUE, FALSE)
d$analyzed_12month <- ifelse(d$analysis12month %in% TRUE | d$dead12month %in% TRUE, TRUE, FALSE)

# only include followed up at 3 month or 12 month #
d <- subset(d,!scorefail_baseline)
d_follow_up<-d


d <- subset(d, analyzed_3month | analyzed_12month)

### read in patient data set ###
data=read.csv('data\\patient_cervical.csv', stringsAsFactors=FALSE)
# combine baseline data and 3/12-month data #
dat1 <- subset(data, redcap_event_name=="baseline_arm_1")
dat1 <- dat1[!duplicated(dat1$pt_study_id),]
dat2 <- subset(data, redcap_event_name=="3month_arm_1")
dat2 <- dat2[!duplicated(dat2$pt_study_id),]
dat3 <- subset(data, redcap_event_name=="12month_arm_1")
dat3 <- dat3[!duplicated(dat3$pt_study_id),]
# change 3month ODI and EQ5D colnames #
vid <- which(names(data) %in% c('patient_baseline_interview_complete', 'thirty_day_morbidity_complete', 'patient_interview_complete', 'questionnaires_complete', 'date_surgery_performed'))
names(dat2)[(vid[2]+1):vid[4]] <- paste(names(dat1)[(vid[2]+1):vid[4]], ".3m", sep="")
names(dat3)[(vid[2]+1):vid[4]] <- paste(names(dat1)[(vid[2]+1):vid[4]], ".12m", sep="")
# merge the data sets #
dat1b <- dat1[c(1:vid[1], (vid[3]+1):vid[5])]
dat2b <- dat2[c(1,(vid[1]+1):vid[4])]
dat3b <- dat3[c(1,(vid[2]+1):vid[4])]
data <- merge(dat1b, dat2b, by=c("pt_study_id"), all=TRUE)
data <- merge(data, dat3b, by=c('pt_study_id'), all=TRUE)

### extract practice/center name, patient id, surgeon id, hospital/surg_location ###
n <- nrow(data)
data$practice <- sub("^([^*]+)(\\*(.+))?_CP[0-9]{4}$", "\\1", data$pt_study_id)
data$sub_practice <- sub("^([^*]+)(\\*(.+))?_CP[0-9]{4}$", "\\3", data$pt_study_id)
data$pt_id <- sub(".*_", "", as.character(data$pt_study_id))
data$surgeon <- sub('^.*\\(([0-9]+)\\)$', '\\1', as.character(data$surgeon))
data$surg_location <- sub('^.*\\(([0-9]+)\\)$', '\\1', as.character(data$surg_location))
#liste=c("Semmes","Vanderbilt","Duke")
#data <- subset(data, practice %in% liste)
# merge the data with index #
data_follow_up <- merge(data, d_follow_up[,c('pt_study_id', 'analyzed_3month', 'analyzed_12month', 'analysis3month', 'analysis12month',"usefull3month","usefull12month")], by='pt_study_id', all.y=TRUE)



data <- merge(data, d[,c('pt_study_id', 'analyzed_3month', 'analyzed_12month', 'analysis3month', 'analysis12month')], by='pt_study_id', all.y=TRUE)

data$surgery_date <- as.Date(data$date_surgery_performed)
data$surgery_date[is.na(data$surgery_date)] <- as.Date(data$surgerydte1[is.na(data$surgery_date)])


## only include sites with at least 20 patients followed up at 3 month #
tab <- with(subset(data, analysis3month), table(practice))
pracs1 <- names(tab)[tab>=20]
data <- subset(data, practice %in% pracs1)
## create 12m analysis data indicator #

tab <- with(subset(data, analysis12month), table(practice))
pracs2 <- names(tab)[tab>=20]

data$select12m <- data$practice %in% pracs2 & data$analysis12month

## prepare variables ##
data$pgender = factor(data$pgender,levels=c('1','2'))
data$age_entered <- as.numeric(data$age_entered)
data$pt_age <- as.numeric(as.Date(data$date_surgery_performed, format='%Y-%m-%d') - as.Date(data$pt_dob, format='%Y-%m-%d'))/365.25
data$ptage2 <- data$age_entered
data$ptage2[is.na(data$age_entered)] <- data$pt_age[is.na(data$age_entered)]
data$prace___1 = factor(data$prace___1,levels=c("0","1"))
data$prace___2 = factor(data$prace___2,levels=c("0","1"))
data$prace___3 = factor(data$prace___3,levels=c("0","1"))
data$prace___4 = factor(data$prace___4,levels=c("0","1"))
data$prace___5 = factor(data$prace___5,levels=c("0","1"))
data$prace___6 = factor(data$prace___6,levels=c("0","1"))
data$ptethnicity = factor(data$ptethnicity,levels=c("1","2"))
data$pt_education_level = factor(data$pt_education_level,levels=c("1","2","3","4","5"))
data$insurance1 = factor(data$insurance1,levels=c("1","2","3","4","5"))
data$workers_comp = factor(data$workers_comp,levels=c("1","0"))
data$liability_claim1 = factor(data$liability_claim1,levels=c("1","0"))
data$employment = factor(data$employment,levels=c("1","2","3","4"))
data$full_part_time = factor(data$full_part_time,levels=c("1","2"))
data$unemployed = factor(data$unemployed,levels=c("1","2","3","4"))
data$disability_reason = factor(data$disability_reason,levels=c("1","2"))
data$plan_return_work = factor(data$plan_return_work,levels=c("1","0","3"))
data$activity_out_home = factor(data$outside_activities,levels=c("1","0"))
data$activity_inside_home = factor(data$activities_home,levels=c("1","0"))
data$any_major_surgery_in_the_p = factor(data$any_major_surgery_in_the_p,levels=c("1","0"))
data$diabetes = factor(data$diabetes,levels=c("0","1"))
data$cad = factor(data$cad,levels=c("1","0"))
data$osteoporosis = factor(data$osteoporosis,levels=c("1","0"))
data$anxiety = factor(data$anxiety,levels=c("1","0"))
data$depression = factor(data$depression,levels=c("1","0"))
data$smoker = factor(data$smoker,levels=c("1","2","3"))
data$pt_height_in2 <- 2.54*data$pt_height_in
data$pt_height_cm2 <- as.numeric(data$pt_height_cm)
data$pt_height <- rowMeans(data[c("pt_height_in2", "pt_height_cm2")], na.rm=TRUE)
data$pt_wt_lb2 <- 0.453592*data$pt_wt_lb
data$pt_wt <- rowMeans(data[c("pt_wt_lb2", "pt_wt_kg")], na.rm=TRUE)
data$bmi <- 10000*data$pt_wt/(data$pt_height)^2
data$symptom_duration2 = factor(data$symptom_duration2,levels=c("1","2","3"))
data$asa_grade = factor(data$asa_grade,levels=c("1","2","3","4"))
data$surgical_approach <- factor(data$surgical_approach, levels=c('1','2','3'))
data$levels <- apply(data[,c('laminectomy_level1', 'foraminotomy_levels1', 'laminoplasty_levels1','laminectomy_level2', 'foraminotomy_levels2', 'laminoplasty_levels2', "segments_fused_ant1","segments_fused_post1", "segments_fused_ant2", "segments_fused_post2", 'acdf_motion_seg_fused1', 'acdf_motion_seg_fused2', 'num_motion_seg_tx1', 'num_motion_seg_tx2')], MARGIN=1, FUN=max, na.rm=TRUE)
data$levels[data$levels==-Inf] <- NA
data$place_discharged_to = factor(data$place_discharged_to,levels=c("1","2","3","4","5","6","7"))
data$patient_died_within_30_day = factor(data$patient_died_within_30_day,levels=c("1","0"))

data$pulmonary_embolism_30day[data$pulmonary_embolism_30day %in% 2] <- NA
data$cva_30days[data$cva_30days %in% 2] <- NA
data$mi_30day[data$mi_30day %in% 2] <- NA
data$surgical_site_infect_30day[data$surgical_site_infect_30day %in% 2] <- NA
data$hematoma[data$hematoma %in% 2] <- NA
data$new_neuro_deficit[data$new_neuro_deficit %in% 2] <- NA
data$uti_30days[data$uti_30days %in% 2] <- NA
data$dvt_30day[data$dvt_30day %in% 2] <- NA
data$dysphagia_ng_tube[data$dysphagia_ng_tube %in% 2] <- NA
data$dysphagia_no_ng_tube[data$dysphagia_no_ng_tube %in% 2] <- NA
data$vocal_cord_paralysis[data$vocal_cord_paralysis %in% 2] <- NA
data$csf_leak[data$csf_leak %in% 2] <- NA
data$wound_dehiscence[data$wound_dehiscence %in% 2] <- NA
data$pneumonia[data$pneumonia %in% 2] <- NA
data$majoradep <- rowSums(data[c("pulmonary_embolism_30day", "cva_30days", "mi_30day", "surgical_site_infect_30day", "hematoma", "new_neuro_deficit")], na.rm=TRUE)
data$majorade <- ifelse(data$majoradep%in%c(1:6),1,0)
data$majorade[is.na(data$majoradep)] <- NA
data$majorade <- factor(data$majorade, levels=c("1", "0"))
data$pulmonary_embolism_30day = factor(data$pulmonary_embolism_30day,levels=c("1","0"))
data$cva_30days = factor(data$cva_30days,levels=c("1","0"))
data$mi_30day = factor(data$mi_30day,levels=c("1","0"))
data$surgical_site_infect_30day = factor(data$surgical_site_infect_30day,levels=c("1","0"))
data$hematoma = factor(data$hematoma,levels=c("1","0"))
data$new_neuro_deficit_30day = factor(data$new_neuro_deficit,levels=c("1","0"))

data$minoradep <- rowSums(data[c("uti_30days", "dvt_30day", "dysphagia_ng_tube", "dysphagia_no_ng_tube", "vocal_cord_paralysis", "csf_leak", "wound_dehiscence", "pneumonia")], na.rm=TRUE)
data$minorade <- ifelse(data$minoradep%in%c(1:8),1,0)
data$minorade[is.na(data$minoradep)] <- NA
data$minorade <- factor(data$minorade, levels=c("1", "0"))
data$uti_30days = factor(data$uti_30days,levels=c("1","0"))
data$dvt_30day = factor(data$dvt_30day,levels=c("1","0"))
data$dysphagia_ng_tube = factor(data$dysphagia_ng_tube,levels=c("1","0"))
data$dysphagia_no_ng_tube = factor(data$dysphagia_no_ng_tube,levels=c("1","0"))
data$vocal_cord_paralysis = factor(data$vocal_cord_paralysis,levels=c("1","0"))
data$csf_leak = factor(data$csf_leak,levels=c("1","0"))
data$wound_dehiscence = factor(data$wound_dehiscence,levels=c("1","0"))
data$pneumonia = factor(data$pneumonia,levels=c("1","0"))


data$pt_satisfaction_index2.3m <- ifelse(data$pt_satisfaction_index.3m%in%c(1,2), 1,2)
data$pt_satisfaction_index2.3m[is.na(data$pt_satisfaction_index.3m)] <- NA
data$pt_satisfaction_index2.3m <- factor(data$pt_satisfaction_index2.3m, levels=c("1", "2"))
data$pt_satisfaction_index.3m = factor(data$pt_satisfaction_index.3m,levels=c("1","2","3","4"))
data$pt_satisfaction_index2.12m <- ifelse(data$pt_satisfaction_index.12m%in%c(1,2), 1,2)
data$pt_satisfaction_index2.12m[is.na(data$pt_satisfaction_index.12m)] <- NA
data$pt_satisfaction_index2.12m <- factor(data$pt_satisfaction_index2.12m, levels=c("1", "2"))
data$pt_satisfaction_index.12m = factor(data$pt_satisfaction_index.12m,levels=c("1","2","3","4"))
data$los <- as.numeric(data$los)
data$los3 <- ifelse(!is.na(data$los_calculated), data$los_calculated, data$los)
data$los3[data$los3>90] <- NA
data$estimated_blood_loss_cc3 <- as.numeric(sub(".*[-]+", "", gsub("[^0-9-]*", "", data$estimated_blood_loss_cc)))
data$returned_to_or_with_30_day = factor(data$returned_to_or_with_30_day,levels=c("1","0"))

data$date_surgery <- as.Date(data$date_surgery_performed, format='%Y-%m-%d')
data$readmission_date <- as.Date(data$readmission_date, format='%Y-%m-%d')
data$date_readmission.3m <- as.Date(data$date_readmission.3m, format='%Y-%m-%d')
data$days1 <- as.numeric(data$readmission_date - data$date_surgery)
data$days2 <- as.numeric(data$date_readmission.3m - data$date_surgery)
data$days.readmit <- ifelse(is.na(data$days1), data$days2, data$days1)
data$readmit30day <- data$re_admitted_within_30_days
data$readmit30day[!is.na(data$days.readmit)] <- 0
data$readmit30day[!is.na(data$days.readmit) & data$days.readmit<31] <- 1
## FIX ME -- HACKY QUICK FIX
data$readmit30day[is.na(data$readmit30day)] <- 0
data$readmit30day <- factor(data$readmit30day, levels=0:1)
data$readmit3m <- ifelse(data$readmit_3months%in%c(1) | data$readmit_3mth_surg.3m%in%c(1) | data$readmit_3mth_surg.12m%in%c(1), 1, 0)
data$readmit3m <- factor(data$readmit3m, levels=c("1", "0"))

data$return_to_work.3m[!data$plan_return_work.3m %in% 1] <- NA
data$return_to_work.3m <- factor(data$return_to_work.3m, levels=c('1','0','2'))
data$return_to_work.12m[!data$plan_return_work.12m %in% 1] <- NA
data$return_to_work.12m <- factor(data$return_to_work.12m, levels=c('1','0','2'))
data$return_to_work.12m[data$return_to_work.3m %in% '1'] <- '1'
data$mort30d <- ifelse(data$place_discharged_to %in% c('6') | data$patient_died_within_30_day %in% c('1'), 1, 0)
data$mort30d[is.na(data$place_discharged_to) & is.na(data$patient_died_within_30_day)] <- NA
data$mort30d <- factor(data$mort30d, level=0:1)
data$mort3m <- ifelse(data$mort30d %in% c('1') | data$patient_interview_status.3m %in% c('9'), 1, 0)
data$mort3m[is.na(data$mort30d) & is.na(data$patient_interview_status.3m)] <- NA
data$mort3m <- factor(data$mort3m, level=0:1)
data$revision_surg_3mths2 <- ifelse(data$revision_surg_3mths.3m %in% 1 | data$revision_surg_3mths.12m %in% 1, 1, 0)
data$revision_surg_3mths2[is.na(data$revision_surg_3mths.3m) & is.na(data$revision_surg_3mths.12m)] <- NA
data$revision_surg_3mths2 <- factor(data$revision_surg_3mths2, levels=c("1", "0"))

data$arthrodesis <- ifelse(data$arthrodesis_performed %in% 1 | data$acdf_performed1 %in% 1, 1, 0)
data$arthrodesis[is.na(data$surgical_approach)] <- NA
data$arthrodesis <- factor(data$arthrodesis, levels=c('1','0'))
data$disability_reason1 <- data$disability_reason2 <- data$disability_reason
data$disability_reason1[data$employment %in% 3] <- NA
data$disability_reason2[data$employment %in% 2] <- NA

# NDI score #
data$ndiscore <- ifelse(!is.na(data$total_ndi_sum_score_int), data$total_ndi_sum_score_int, data$total_ndi_sum_score_self) * 2
data$ndiscore.3m <- ifelse(!is.na(data$total_ndi_sum_score_int.3m), data$total_ndi_sum_score_int.3m, data$total_ndi_sum_score_self.3m) * 2
data$ndiscore.12m <- ifelse(!is.na(data$total_ndi_sum_score_int.12m), data$total_ndi_sum_score_int.12m, data$total_ndi_sum_score_self.12m) * 2

# EQ5D score #
data$mobility <- ifelse(is.na(data$mobility_int), data$mobility_self, data$mobility_int)
data$self_care <- ifelse(is.na(data$self_care_int), data$self_care_self, data$self_care_int)
data$usual_activities <- ifelse(is.na(data$usual_activities_int), data$usual_activities_self, data$usual_activities_int)
data$pain_discomfort <- ifelse(is.na(data$pain_discomfort_int), data$pain_discomfort_self, data$pain_discomfort_int)
data$anxiety_depression <- ifelse(is.na(data$anxiety_depression_int), data$anxiety_depression_self, data$anxiety_depression_int)
data$mobility.3m <- ifelse(is.na(data$mobility_int.3m), data$mobility_self.3m, data$mobility_int.3m)
data$self_care.3m <- ifelse(is.na(data$self_care_int.3m), data$self_care_self.3m, data$self_care_int.3m)
data$usual_activities.3m <- ifelse(is.na(data$usual_activities_int.3m), data$usual_activities_self.3m, data$usual_activities_int.3m)
data$pain_discomfort.3m <- ifelse(is.na(data$pain_discomfort_int.3m), data$pain_discomfort_self.3m, data$pain_discomfort_int.3m)
data$anxiety_depression.3m <- ifelse(is.na(data$anxiety_depression_int.3m), data$anxiety_depression_self.3m, data$anxiety_depression_int.3m)
data$mobility.12m <- ifelse(is.na(data$mobility_int.12m), data$mobility_self.12m, data$mobility_int.12m)
data$self_care.12m <- ifelse(is.na(data$self_care_int.12m), data$self_care_self.12m, data$self_care_int.12m)
data$usual_activities.12m <- ifelse(is.na(data$usual_activities_int.12m), data$usual_activities_self.12m, data$usual_activities_int.12m)
data$pain_discomfort.12m <- ifelse(is.na(data$pain_discomfort_int.12m), data$pain_discomfort_self.12m, data$pain_discomfort_int.12m)
data$anxiety_depression.12m <- ifelse(is.na(data$anxiety_depression_int.12m), data$anxiety_depression_self.12m, data$anxiety_depression_int.12m)
eq <- read.csv('data\\eq5dscore.csv')
eq$com <- paste(eq$MO, eq$SC, eq$UA, eq$PD, eq$AD, sep='')
data$com <- with(data, paste(mobility, self_care, usual_activities, pain_discomfort, anxiety_depression, sep=''))
data$com.3m <- with(data, paste(mobility.3m, self_care.3m, usual_activities.3m, pain_discomfort.3m, anxiety_depression.3m, sep=''))
data$com.12m <- with(data, paste(mobility.12m, self_care.12m, usual_activities.12m, pain_discomfort.12m, anxiety_depression.12m, sep=''))

data$eq5dscore <- eq$score[match(data$com, eq$com)]
data$eq5dscore.3m <- eq$score[match(data$com.3m, eq$com)]
data$eq5dscore.12m <- eq$score[match(data$com.12m, eq$com)]

data$revision <- ifelse(data$primary_revision %in% 2, 1, 0)


# joa #
data$joa <- data$mjoa_score_baseline
data$joa.3m <- data$mjoa_score_3mnth.3m
data$joa.12m <- data$mjoa_score_12mnth.12m

# surgery indicator variables #
data$ind1 <- ifelse(data$indication_surgery___1 %in% 1, 1, NA)
data$ind2 <- ifelse(data$indication_surgery___2 %in% 1, 1, NA)
data$ind3 <- ifelse(data$indication_surgery___3 %in% 1, 1, NA)
data$ind4 <- ifelse(data$underlying_pathology___1%in% 1, 1, NA)
data$ind5 <- ifelse(data$underlying_pathology___2%in% 1, 1, NA)
data$ind6 <- ifelse(data$underlying_pathology___3%in% 1, 1, NA)
data$ind7 <- ifelse(data$underlying_pathology___4%in% 1, 1, NA)
data$ind8 <- ifelse(data$underlying_pathology___2 %in% 1 | data$underlying_pathology___3 %in% 1, 1, NA)
data$ind9 <- ifelse(data$underlying_pathology___5%in% 1, 1, NA)
data$ind10 <- ifelse(data$underlying_pathology___6%in% 1, 1, NA)

# new categories #
data$newcat1 <- ifelse(data$surgical_approach %in% 1 & data$laminectomy_performed1 %in% 1  & data$laminoplasty_performed1 %nin% 1 & data$arthrodesis_performed %nin% 1, 1, NA)
data$newcat2 <- ifelse(data$surgical_approach %in% 1 & data$laminectomy_performed1 %in% 1  & data$arthrodesis_performed %in% 1, 1, NA)
data$newcat3 <- ifelse(data$surgical_approach %in% 1 & data$minimally_invasive %nin% 1 & data$laminectomy_performed1 %nin% 1 & data$foraminotomy_performed1 %in% 1 & data$laminoplasty_performed1 %nin% 1 & data$arthrodesis_performed %nin% 1, 1, NA)
data$newcat4 <- ifelse(data$acdf_performed1 %in% 1 & data$corpectomy_performed1 %nin% 1 & data$arthroplasty1 %nin% 1, 1, NA)
data$newcat5 <- ifelse(data$corpectomy_performed1 %in% 1, 1, NA)

## return to work ##
data$date_of_contact.3m <- as.Date(data$date_of_contact.3m, format='%Y-%m-%d')
data$date_return_work.3m <- as.Date(data$date_return_work.3m, format='%Y-%m-%d')
data$date_surgery <- as.Date(data$date_surgery_performed, format='%Y-%m-%d')
data$day1 <- as.numeric(data$date_of_contact.3m - data$date_surgery)
data$day2 <- as.numeric(data$date_return_work.3m- data$date_surgery)
data$day1[data$day1<0 | data$day1>200] <- NA
data$day2[data$day2<0 | data$day2>200] <- NA
data$rtw.3m <- as.numeric(ifelse(as.character(data$return_to_work.3m) %in% 2, NA, as.character(data$return_to_work.3m)))
data$day <- data$day1
ind <- data$return_to_work.3m %in% 1
data$day[ind] <- ifelse(is.na(data$day2[ind]), data$day1[ind], data$day2[ind])
data$rtw.3m[is.na(data$day)] <- NA






##################################################################
#### model fitting to get plots and tables of expected values ####
##################################################################

    ds <- data
    ds$race <- ifelse(ds$prace___5==1, 'white', 'other')
    ds$race[ds$prace___3==1] <- 'black'
    ds$race <- factor(ds$race, levels=c('white', 'black', 'other'), labels=c('1','2','3'))
    ds$predominat_symptom_durg <- factor(ds$predominat_symptom_durg, levels=c('1','2','3'))
    ds$underlying_pathology <- NA
    ds$underlying_pathology[ds$underlying_pathology___1 %in% 1] <- 1
    ds$underlying_pathology[ds$underlying_pathology___2 %in% 1] <- 2
    ds$underlying_pathology[ds$underlying_pathology___3 %in% 1] <- 3
    ds$underlying_pathology[ds$underlying_pathology___4 %in% 1] <- 4
    ds$underlying_pathology[ds$underlying_pathology___5 %in% 1] <- 5
    ds$underlying_pathology[ds$underlying_pathology___6 %in% 1] <- 6
    ds$underlying_pathology <- factor(ds$underlying_pathology, levels=c('1','2','3','4','5','6'))
    ds$occupation <- factor(ds$pre_illness_work2, levels=1:4)
    ds$work <- ds$pre_illness_work2
    ds$work[ds$employment %in% c(3,4)] <- 7
    ds$work[ds$unemployed %in% 1] <- 5
    ds$work[ds$unemployed %in% 2] <- 6
    ds$work <- factor(ds$work, levels=c(1,2,3,4,5,6,7))
    ds$pt_satisfaction_index.3m <- as.numeric(ds$pt_satisfaction_index.3m)
    ds$pt_satisfaction_index.12m <- as.numeric(ds$pt_satisfaction_index.12m)
    ds <- subset(ds, select=c(pgender, ptage2, race, ptethnicity,pt_education_level, workers_comp, liability_claim1, any_major_surgery_in_the_p, diabetes, cad, osteoporosis, anxiety, depression, smoker, bmi, predominat_symptom_durg, underlying_pathology, symptom_duration2, asa_grade, arthrodesis, surgical_approach, occupation, work, ndiscore, eq5dscore, neck_pain_vas, arm_pain_vas1, joa, ndiscore.3m, ndiscore.12m, eq5dscore.3m, eq5dscore.12m, neck_pain_vas.3m, neck_pain_vas.12m, arm_pain_vas1.3m, arm_pain_vas1.12m, pt_satisfaction_index.3m, pt_satisfaction_index.12m, los3, estimated_blood_loss_cc3, joa.3m, joa.12m, rtw.3m, day, practice, pt_study_id, analysis3month, select12m))
    rtwdata <- subset(ds, !is.na(rtw.3m))

    ## single impute missing covariates ##
    tmp <- transcan(~ pgender + ptage2 + race + ptethnicity + pt_education_level + workers_comp + liability_claim1 + any_major_surgery_in_the_p + diabetes + cad + osteoporosis + anxiety + depression + smoker + bmi + predominat_symptom_durg + underlying_pathology + symptom_duration2 + asa_grade + arthrodesis + surgical_approach + work + ndiscore + eq5dscore + neck_pain_vas + arm_pain_vas1 + joa, data=ds, imputed=TRUE, transformed=TRUE, pl=FALSE, pr=FALSE)
    imp <- impute(tmp, data=ds, list.out=TRUE)
    ## get complete data set ##
    for (i in names(tmp$imputed)) {
        x <- ds[[i]]
        if (sum(is.na(x))>0) {
            if (is.numeric(x)) {
                ds[[i]][is.na(x)] <- tmp$imputed[[i]]
            } else {
                ds[[i]][is.na(x)] <- levels(ds[[i]])[tmp$imputed[[i]]]
            }
        }
    }

    ## single impute missing covariates for rtw data ##
    tmp <- transcan(~ pgender + ptage2 + race + ptethnicity + pt_education_level + workers_comp + liability_claim1 + any_major_surgery_in_the_p + diabetes + cad + osteoporosis + anxiety + depression + smoker + bmi + predominat_symptom_durg + underlying_pathology + symptom_duration2 + asa_grade + arthrodesis + surgical_approach + occupation + ndiscore + eq5dscore + neck_pain_vas + arm_pain_vas1 + joa, data=rtwdata, imputed=TRUE, transformed=TRUE, pl=FALSE, pr=FALSE)
    imp <- impute(tmp, data=rtwdata, list.out=TRUE)
    ## get complete data set ##
    for (i in names(tmp$imputed)) {
        x <- rtwdata[[i]]
        if (sum(is.na(x))>0) {
            if (is.numeric(x)) {
                rtwdata[[i]][is.na(x)] <- tmp$imputed[[i]]
            } else {
                rtwdata[[i]][is.na(x)] <- levels(rtwdata[[i]])[tmp$imputed[[i]]]
            }
        }
    }


                                        # to get bootstrap CI #
    btfun <- function(x, B=2000) {
        set.seed(123)
        n <- length(x)
        if (n > 0) {
            m <- mean(x, na.rm=TRUE)
            v <- numeric(B)
            for (i in seq(B)) {
                v[i] <- mean(x[sample(1:n, size=n, replace=TRUE)], na.rm=TRUE)
            }
            ci <- quantile(v, probs=c(0.025,0.975))
            return(c(n,m,ci))
        }
        return(c(0,NA,NA,NA))
    }

                                        # fit models and get observed and expected mean for each site #
    modfun <- function(dt, fmla, resp1, resp2, prs=data.frame(prac=pracs1)) {
        dt1 <- dt[dt$analysis3month & !is.na(dt[[resp1]]),]
        tmp <- aggregate(dt1[[resp1]], by=list(dt1$practice), FUN=btfun)
        obs1 <- data.frame(prac=tmp[[1]], n=tmp[[2]][,1], m=tmp[[2]][,2], lower=tmp[[2]][,3], upper=tmp[[2]][,4])
        dd <<- datadist('dt1')
        options(datadist='dd')
        fmla1 <- as.formula(paste(resp1, '~', fmla))
        fit1 <- orm(fmla1, data=dt1, x=TRUE, y=TRUE)
        pred1 <- predict(fit1, type='mean')
        tmp <- aggregate(pred1, by=list(dt1$practice), FUN=mean, na.rm=TRUE)
        obs1$pred <- tmp[[2]]
        obs1 <- merge(obs1, prs, by='prac', all.y=TRUE)
        
        if (resp2 != '') {
            dt2 <- dt[dt$select12m & !is.na(dt[[resp2]]), ]
            tmp <- aggregate(dt2[[resp2]], by=list(dt2$practice), FUN=btfun)
            obs2 <- data.frame(prac=tmp[[1]], n2=tmp[[2]][,1], m2=tmp[[2]][,2], lower2=tmp[[2]][,3], upper2=tmp[[2]][,4])
            dd <<- datadist('dt2')
            options(datadist='dd')
            fmla2 <- as.formula(paste(resp2, '~', fmla))
            fit2 <- orm(fmla2, data=dt2, x=TRUE, y=TRUE)
            pred2 <- predict(fit2, type='mean')
            tmp <- aggregate(pred2, by=list(dt2$practice), FUN=mean, na.rm=TRUE)
            obs2$pred2 <- tmp[[2]]
            obs1 <- merge(obs1, obs2, by='prac', all.x=TRUE)
        } else {
            obs1 <- cbind(obs1, n2=NA, m2=NA, lower2=NA, upper2=NA, pred2=NA)
        }
        return(obs1)
    }

    
    

    

    fmla0 <- "pgender + ptage2 + race + ptethnicity + pt_education_level + workers_comp + liability_claim1 + any_major_surgery_in_the_p + diabetes + cad + osteoporosis + anxiety + depression + smoker + rcs(bmi,4) + predominat_symptom_durg + underlying_pathology + symptom_duration2 + asa_grade + arthrodesis + surgical_approach + work + rcs(ndiscore,3) + rcs(eq5dscore,3) + rcs(neck_pain_vas,3) + rcs(arm_pain_vas1,3)"
    fmla1 <- "pgender + ptage2 + race + ptethnicity + pt_education_level + workers_comp + liability_claim1 + any_major_surgery_in_the_p + diabetes + cad + osteoporosis + anxiety + depression + smoker + rcs(bmi,4) + predominat_symptom_durg + underlying_pathology + symptom_duration2 + asa_grade + arthrodesis + surgical_approach + work + rcs(ndiscore,3) + rcs(eq5dscore,3) + rcs(neck_pain_vas,3) + rcs(arm_pain_vas1,3) + rcs(joa,3)"

                                   # the following models may take a few minutes to run #
    ndi <- modfun(dt=ds, fmla=fmla0, resp1="ndiscore.3m", resp2="ndiscore.12m", prs=data.frame(prac=pracs1))
    eq5d <- modfun(dt=ds, fmla=fmla0, resp1="eq5dscore.3m", resp2="eq5dscore.12m", prs=data.frame(prac=pracs1))
    neck <- modfun(dt=ds, fmla=fmla0, resp1="neck_pain_vas.3m", resp2="neck_pain_vas.12m", prs=data.frame(prac=pracs1))
    arm <- modfun(dt=ds, fmla=fmla0, resp1="arm_pain_vas1.3m", resp2="arm_pain_vas1.12m", prs=data.frame(prac=pracs1))
    satisfy <- modfun(dt=ds, fmla=fmla0, resp1="pt_satisfaction_index.3m", resp2="pt_satisfaction_index.12m", prs=data.frame(prac=pracs1))
    los <- modfun(dt=ds, fmla=fmla0, resp1="los3", resp2="", prs=data.frame(prac=pracs1))
    blood <- modfun(dt=ds, fmla=fmla0, resp1="estimated_blood_loss_cc3", resp2="", prs=data.frame(prac=pracs1))
    joa <- modfun(dt=ds, fmla=fmla1, resp1="joa.3m", resp2="joa.12m", prs=data.frame(prac=pracs1))

                                        # organize output results #
    np <- length(pracs1)
    modrst <- vector('list', np)
    for (i in 1:np) {
        mat <- matrix(NA, nrow=8, ncol=10, dimnames=list(c('neck', 'arm', 'odi', 'eq5d', 'joa', 'satisfy', 'blood', 'los'), c('n', 'm', 'lower', 'upper', 'pred', 'n2', 'm2', 'lower2', 'upper2', 'pred2')))
        mat[1,] <- as.numeric(neck[i,2:11])
        mat[2,] <- as.numeric(arm[i,2:11])
        mat[3,] <- as.numeric(ndi[i,2:11])
        mat[4,] <- as.numeric(eq5d[i,2:11])
        mat[5,] <- as.numeric(joa[i,2:11])
        mat[6,] <- as.numeric(satisfy[i,2:11])
        mat[7,] <- as.numeric(blood[i,2:11])
        mat[8,] <- as.numeric(los[i,2:11])
        modrst[[i]] <- data.frame(mat)
    }
    names(modrst) <- names(table(ds$practice))

                                        # model of return to work #
    fit <- coxph(Surv(day, rtw.3m) ~ pgender + ptage2 + race + ptethnicity + pt_education_level + workers_comp + liability_claim1 + any_major_surgery_in_the_p + diabetes + cad + osteoporosis + anxiety + depression + smoker + rcs(bmi,4) + predominat_symptom_durg + underlying_pathology + symptom_duration2 + asa_grade + arthrodesis + surgical_approach + occupation + rcs(ndiscore,3) + rcs(eq5dscore,3) + rcs(neck_pain_vas,3) + rcs(arm_pain_vas1,3), data=rtwdata)
    pred <- survfit(fit, newdata=rtwdata, se.fit=FALSE)
    tmp <- apply(pred$surv, MARGIN=1, FUN=function(x) {
        zxcv <- aggregate(x, by=list(rtwdata$practice), FUN=mean)
        return(zxcv[[2]])
    })
    survs <- data.frame(cbind(t(tmp), pred$time))
    colnames(survs) <- c(names(table(rtwdata$practice)), 'time')

                                        # generate plot of predicted values #
    rerange <- function(x, ranges=c(10,10,100,1,3,10,600)) {
        y <- matrix(NA, nrow=nrow(x), ncol=ncol(x))
        n <- nrow(x)
        for (k in 1:n) {
            y[k,] <- as.numeric(x[k,]*100/ranges[k])
        }
        return(y)
    }
#####################################################################   
    
    for (j in 1:np) {
      tprac <- pracs1[j]
      sp <- 10
      pnam <- paste('figs/',tprac, '_model.pdf', sep='')
      pdf(pnam, width=16, height=6)
      par(mar=c(2,10,4,0), xpd=NA, mfrow=c(1,1))
      plot(0:210, seq(from=0.5, to=7.5, length=211), xlim=c(-2,220), ylim=c(0.8,9.5), xlab='', ylab='', main='', axes=FALSE, type='n')
      for (i in 8:3) {
        segments(0, i, 100, i, lty=2, col='grey60')
        segments(100+sp, i, 200+sp, i, lty=2, col='grey60')
      }
      for (i in 2:1) {
        segments(0, i, 100, i, lty=2, col='grey60')
      }
      trst <- modrst[[tprac]]
      trst3 <- trst2 <- trst
      trst2['satisfy', c(2:5,7:10)] <- trst['satisfy', c(2:5,7:10)] - 1
      trst3[,c(2:5,7:10)] <- rerange(x=trst2[,c(2:5,7:10)], ranges=c(10,10,100,1,17,3,700,10))
      trst4 <- format(round(trst,2), nsmall=2)
      trst4[c(3,7),] <- format(round(trst[c(3,7),],1), nsmall=1)
      for (i in 1:8) {
        if(!is.nan(trst3[i,'pred'])){
          if (trst3[i,'pred'] > trst3[i,'upper'] | trst3[i,'pred'] < trst3[i,"lower"]){
            points(as.numeric(trst3[i,c('m', 'pred')]), c(9-i, 9-i), pch=c(16,17), col=c('blue', 'red'))
            points(as.numeric(trst3[i,c('lower', 'upper')]), c(9-i, 9-i), pch=c('(', ')'), col=c('blue', 'blue'))
            text(trst3[i,'m'], 9-i, trst4[i,'m'], pos=3, col='blue', cex=0.8)
            text(trst3[i,'pred'], 9-i, trst4[i,'pred'], pos=1, col='red', cex=0.8)
            text(0, 9-i, c(0,0,0,0,0,1,0,0)[i], pos=1)
            text(100, 9-i, c(10,10,100,1,17,4,700,10)[i], pos=1)
          }
          else{
            points(as.numeric(trst3[i,c('m', 'pred')]), c(9-i, 9-i), pch=c(16,18), col=c('blue', 'forestgreen'))
            points(as.numeric(trst3[i,c('lower', 'upper')]), c(9-i, 9-i), pch=c('(', ')'), col=c('blue', 'blue'))
            text(trst3[i,'m'], 9-i, trst4[i,'m'], pos=3, col='blue', cex=0.8)
            text(trst3[i,'pred'], 9-i, trst4[i,'pred'], pos=1, col='forestgreen', cex=0.8)
            text(0, 9-i, c(0,0,0,0,0,1,0,0)[i], pos=1)
            text(100, 9-i, c(10,10,100,1,17,4,700,10)[i], pos=1)
          }
        }
        else{
          points(as.numeric(trst3[i,c('m', 'pred')]), c(9-i, 9-i), pch=c(16,17), col=c('blue', 'red'))
          points(as.numeric(trst3[i,c('lower', 'upper')]), c(9-i, 9-i), pch=c('(', ')'), col=c('blue', 'blue'))
          text(trst3[i,'m'], 9-i, trst4[i,'m'], pos=3, col='blue', cex=0.8)
          text(trst3[i,'pred'], 9-i, trst4[i,'pred'], pos=1, col='red', cex=0.8)
          text(0, 9-i, c(0,0,0,0,0,1,0,0)[i], pos=1)
          text(100, 9-i, c(10,10,100,1,17,4,700,10)[i], pos=1)
        }
      }
      
      if (tprac %in% pracs2) {
        for (i in 1:6) {
          if(!is.nan(trst3[i,'pred2'])){
            if (trst3[i,'pred2'] > trst3[i,'upper2'] | trst3[i,'pred2'] < trst3[i,"lower2"]){
              points(as.numeric(trst3[i,c('m2', 'pred2')]) + 100 + sp, c(9-i, 9-i), pch=c(16,17), col=c('blue', 'red'))
              points(as.numeric(trst3[i,c('lower2', 'upper2')]) + 100 + sp, c(9-i, 9-i), pch=c('(', ')'), col=c('blue', 'blue'))
              text(trst3[i,'m2'] + 100 + sp, 9-i, trst4[i,'m2'], pos=3, col='blue', cex=0.8)
              text(trst3[i,'pred2'] + 100 + sp, 9-i, trst4[i,'pred2'], pos=1, col='red', cex=0.8)
              text(100+sp, 9-i, c(0,0,0,0,0,1,0,0)[i], pos=1)
              text(200+sp, 9-i, c(10,10,100,1,17,4,700,10)[i], pos=1)
            }
            else{
              points(as.numeric(trst3[i,c('m2', 'pred2')]) + 100 + sp, c(9-i, 9-i), pch=c(16,18), col=c('blue', 'forestgreen'))
              points(as.numeric(trst3[i,c('lower2', 'upper2')]) + 100 + sp, c(9-i, 9-i), pch=c('(', ')'), col=c('blue', 'blue'))
              text(trst3[i,'m2'] + 100 + sp, 9-i, trst4[i,'m2'], pos=3, col='blue', cex=0.8)
              text(trst3[i,'pred2'] + 100 + sp, 9-i, trst4[i,'pred2'], pos=1, col='forestgreen', cex=0.8)
              text(100+sp, 9-i, c(0,0,0,0,0,1,0,0)[i], pos=1)
              text(200+sp, 9-i, c(10,10,100,1,17,4,700,10)[i], pos=1)
            }
          }
          else{
            points(as.numeric(trst3[i,c('m2', 'pred2')]) + 100 + sp, c(9-i, 9-i), pch=c(16,17), col=c('blue', 'red'))
            points(as.numeric(trst3[i,c('lower2', 'upper2')]) + 100 + sp, c(9-i, 9-i), pch=c('(', ')'), col=c('blue', 'blue'))
            text(trst3[i,'m2'] + 100 + sp, 9-i, trst4[i,'m2'], pos=3, col='blue', cex=0.8)
            text(trst3[i,'pred2'] + 100 + sp, 9-i, trst4[i,'pred2'], pos=1, col='red', cex=0.8)
            text(100+sp, 9-i, c(0,0,0,0,0,1,0,0)[i], pos=1)
            text(200+sp, 9-i, c(10,10,100,1,17,4,700,10)[i], pos=1)
          }
        }
      }
      
      axis(side=2, at=8:1, labels=c('Neck Pain', 'Arm Pain', 'NDI', 'EQ5D', 'mJOA', 'Patient Satisfaction', 'Blood Loss \n (ml)', 'Length of Hospital Stay \n (day)'), las=2, col='white', font=2, line=-2)
      legend(x=130, y=2,  legend=c('QOD risk adjusted estimate (significantly different from site)','QOD risk adjusted estimate (no significant difference with site)',latexTranslate(tprac)), pch=c(17,18,16), col=c('red','forestgreen', 'blue'), bty='n', xpd=NA, ncol=1)
      text(50, 8, '')
      title(main=paste(tprac, ': Self Benchmark Patient Reported Outcomes and Utilization', sep=''))
      text(50, 8.3, '3-Month Post-Surgery', pos=3)
      text(150+sp, 8.3, '12-Month Post-Surgery', pos=3)
      dev.off()
      
      # survival plot #
      nsample <- sum(rtwdata$practice==tprac)
      if (nsample>0) {
        pnam <- paste('figs/cervical_' ,tprac, '_model2.pdf', sep='')
        pdf(pnam, width=6, height=6)
        par(mar=c(5,4,4,2), mfrow=c(1,1))
        dt <- subset(rtwdata, practice==tprac)
        tmp <- summary(survfit(Surv(day, rtw.3m) ~ 1, data=dt))
        tmp2 <- tmp
        tmp2$time <- tmp$time[!is.na(tmp$lower) & !is.na(tmp$upper)]
        tmp2$surv <- tmp$surv[!is.na(tmp$lower) & !is.na(tmp$upper)]
        tmp2$lower <- tmp$lower[!is.na(tmp$lower) & !is.na(tmp$upper)]
        tmp2$upper <- tmp$upper[!is.na(tmp$lower) & !is.na(tmp$upper)]
        plot(tmp$time, 1-tmp$surv, type='n', xlim=c(0,200), ylim=c(0,1), xlab='Days Post-Surgery', ylab='Proportion of Return to Work')
        polygon(c(tmp2$time, rev(tmp2$time)), c(1-tmp2$lower, rev(1-tmp2$upper)), col='grey80', border=NA)
        lines(tmp$time, 1-tmp$surv, type='s')
        lines(survs$time, 1-survs[[tprac]], type='s', col='blue')
        title(main=paste(tprac, ': Self Benchmark Return to Work \n (N=', nsample, ')', sep=''), line=4.3)
        legend('bottomright', bty='n', lty=1:1, col=c('black', 'blue'), legend=c(latexTranslate(tprac), 'QOD risk adjusted'))
        dev.off()
      } else {
        print(tprac)
      }
    }

    #####################################################################
    
    modrst2 <- vector('list', 0)
    for (i in 1:np) {
        tprac <- pracs1[i]
        if (tprac %nin% pracs2) {
            mat <- matrix('', nrow=8, ncol=8)
            tmp1 <- as.matrix(modrst[[tprac]])
            tmp2 <- as.matrix(format(round(tmp1,2), nsmall=2))
            tmp2[c(3,7),] <- as.matrix(format(round(tmp1[c(3,7),],1), nsmall=1))
            mat[1:8,c(2,4)] <- tmp2[1:8,c(2,5)]
            mat[1:8,3] <- paste('(', tmp2[1:8,3], ', ', tmp2[1:8,4], ')', sep='')
            mat[1:8,1] <- tmp1[1:8,1]
            mat2 <- matrix('', nrow=8, ncol=6)
            mat2[,c(1,3)] <- mat[,c(1,4)]
            mat2[,2] <- paste(mat[,2], mat[,3], sep='~~~~')
            modrst2[[tprac]] <- mat2
        } else {
            mat <- matrix('', nrow=8, ncol=8)
            tmp1 <- as.matrix(modrst[[tprac]])
            tmp2 <- as.matrix(format(round(tmp1,2), nsmall=2))
            tmp2[c(3,7),] <- as.matrix(format(round(tmp1[c(3,7),],1), nsmall=1))
            mat[1:8,c(2,4)] <- tmp2[1:8,c(2,5)]
            mat[1:8,3] <- paste('(', tmp2[1:8,3], ', ', tmp2[1:8,4], ')', sep='')
            mat[1:8,1] <- tmp1[1:8,1]
            mat[1:6,5] <- tmp1[1:6,6]
            mat[1:6,7] <- paste('(', tmp2[1:6,8], ', ', tmp2[1:6,9], ')', sep='')
            mat[1:6,c(6,8)] <- tmp2[1:6,c(7,10)]
            mat2 <- matrix('', nrow=8, ncol=6)
            mat2[,c(1,3, 4, 6)] <- mat[,c(1,4, 5, 8)]
            mat2[,2] <- paste(mat[,2], mat[,3], sep='~~~~')
            mat2[,5] <- paste(mat[,6], mat[,7], sep='~~~~')
            modrst2[[tprac]] <- mat2
        }
    }
    save(modrst2, file='cervical_modrst2.RData')


#################################
## generate descriptive tables ##
#################################

## get the start date of each center #
startdate <- aggregate(data$date_surgery, by=list(data$practice), FUN=min, na.rm=TRUE)
colnames(startdate) <- c('practice', 'sdate')

startdate$sdate <- as.character(startdate$sdate)
startdate$edate <- dldate


#### functions of generating descriptive statistics ####

# for categorical variables #
catfun1 <- function(var1, var2, ilev, dfs, var3="pt_study_id") {
  nlist <- length(dfs)
  dats <- vector("list", nlist)
  for (i in 1:nlist) {
    dats[[i]] <- dfs[[i]][!is.na(dfs[[i]][[var2]])&!is.na(dfs[[i]][[var3]]), ]
  }
  pcts <- character(nlist)
  fracs <- character(nlist)
  for (i in 1:nlist) {
    n <- nrow(dats[[i]])
    if (n==0) {
      pcts[i] <- "~~~~~~~~~~~~"
      fracs[i] <- "~{\\scriptsize $\\frac{0}{0}$}"
    } else {
      num <- sum(dats[[i]][[var1]]%in%ilev)
      pct <- paste(format(round(100*num/n,1), nsmall=1), "$\\%$", sep="")
      pcts[i] <- gsub(" ", "~", sprintf("%14s", pct))
      fracs[i] <- paste("{\\scriptsize $\\frac{", num, "}{", n, "}$}", sep="")
    }
  }
  ret <- paste(pcts, fracs, sep="~~")
  return(ret)
}


catfun2 <- function(ds, vars) {
  n <- length(vars)
  if (n==2) {
    M <- rep('',4)
    dt <- ds[!is.na(ds[[vars[1]]]) & !is.na(ds[[vars[2]]]), ]
    M[1] <- nrow(dt)
    if (nrow(dt) > 0) {
      tmp <- dt[[vars[1]]]
      num <- sum(tmp %in% c(1))
      den <- nrow(dt)
      pct <- format(round(100*num/den,1), nsmall=1) 
      M[3] <- paste(pct, "$\\%$ (", num, ")", sep="")
      tmp <- dt[[vars[2]]]
      num <- sum(tmp %in% c(1))
      den <- nrow(dt)
      pct <- format(round(100*num/den,1), nsmall=1) 
      M[4] <- paste(pct, "$\\%$ (", num, ")", sep="")
    }
  } else {
    M <- rep('',3)
    dt <- ds[!is.na(ds[[vars[1]]]), ]
    M[1] <- nrow(dt)
    if (nrow(dt) > 0) {
      tmp <- dt[[vars[1]]]
      num <- sum(tmp %in% c(1))
      den <- nrow(dt)
      pct <- format(round(100*num/den,1), nsmall=1) 
      M[3] <- paste(pct, "$\\%$ (", num, ")", sep="")
    }
  }
  return(M)
}

catfun2a <- function(ds, vars) {
  n <- length(vars)
  M <- rep('',4)
  dt3 <- ds[!is.na(ds[[vars[1]]]), ]
  dt12 <- ds[!is.na(ds[[vars[2]]]), ]
  M[1] <- nrow(dt3)
  M[3] <- nrow(dt12)
  if (nrow(dt3) > 0) {
    tmp <- dt3[[vars[1]]]
    num <- sum(tmp %in% c(1))
    den <- nrow(dt3)
    pct <- format(round(100*num/den,1), nsmall=1) 
    M[2] <- paste(pct, "$\\%$ (", num, ")", sep="")
    tmp <- dt12[[vars[2]]]
    num <- sum(tmp %in% c(1))
    den <- nrow(dt12)
    pct <- format(round(100*num/den,1), nsmall=1) 
    M[4] <- paste(pct, "$\\%$ (", num, ")", sep="")
  }
  
  return(M)
}


# continuous variable #
confun1 <- function(var1, dig=1, dfs, var2, var3="pt_study_id") {
  nlist <- length(dfs)
  vecs <- vector("list", nlist)
  for (i in 1:nlist) {
    tmpdat <- dfs[[i]][!is.na(dfs[[i]][[var2]])&!is.na(dfs[[i]][[var3]]), ]
    vecs[[i]] <- tmpdat[[var1]]
  }
  pcts <- character(nlist)
  fracs <- character(nlist)
  for (i in 1:nlist) {
    n <- sum(!is.na(vecs[[i]]))
    if (n==0) {
      pcts[i] <-  "~~~~~~~~~~~~"
      fracs[i] <- "{\\scriptsize N=0}"
    } else {
      avg <- format(round(mean(vecs[[i]], na.rm=TRUE),dig), nsmall=dig)
      std <- format(round(sd(vecs[[i]], na.rm=TRUE),dig), nsmall=dig)
      tmp <- paste(avg, "$\\pm$", std, sep="")
      pcts[i] <- gsub(" ", "~", sprintf("%14s", tmp))
      fracs[i] <- paste("{\\scriptsize N=", n, "}", sep="")
    }
  }
  ret <- paste(pcts, fracs, sep="~~")
  return(ret)
}

# for categorical variables for figures#
catfun1b <- function(var1, var2, ilev, dfs, var3="pt_study_id") {
  nlist <- length(dfs)
  dats <- vector("list", nlist)
  for (i in 1:nlist) {
    dats[[i]] <- dfs[[i]][!is.na(dfs[[i]][[var2]])&!is.na(dfs[[i]][[var3]]), ]
  }
  pcts <- integer(nlist)
  for (i in 1:nlist) {
    n <- nrow(dats[[i]])
    num <- sum(dats[[i]][[var1]]%in%ilev)
    pct <- round(100*num/n,1)
    pcts[i] <-  pct
  }
  return(pcts)
}

# continuous variable #
confun2 <- function(var1, dig=1, dfs, dig2=0) {
  nlist <- length(dfs)
  vecs <- vector("list", nlist)
  for (i in 1:nlist) {
    vecs[[i]] <- as.numeric(dfs[[i]][[var1]])
  }
  pcts <- character(nlist)
  fracs <- character(nlist)
  for (i in 1:nlist) {
    n <- sum(!is.na(vecs[[i]]))
    if (n==0) {
      pcts[i] <-  "~~~~~~~~~~~~~~~~~~"
      fracs[i] <- "{\\scriptsize N=0}"
    } else {
      avg <- format(round(mean(vecs[[i]], na.rm=TRUE),dig), nsmall=dig)
      std <- format(round(sd(vecs[[i]], na.rm=TRUE),dig), nsmall=dig)
      mdn <- format(round(median(vecs[[i]], na.rm=TRUE), dig2), nsmall=dig2)
      low <- format(round(quantile(vecs[[i]], probs=0.25, na.rm=TRUE), dig2), nsmall=dig2)
      up <- format(round(quantile(vecs[[i]], probs=0.75, na.rm=TRUE), dig2), nsmall=dig2)
      tmp <- paste("{\\scriptsize ", low, "}~", mdn, "{\\scriptsize ", up, "}~", "(",  avg, "$\\pm$", std, ")", sep="")
      
      pcts[i] <- gsub(" ", "~", sprintf("%20s", tmp))
      fracs[i] <- paste("{\\scriptsize N=", n, "}", sep="")
    }
  }
  ret <- paste(pcts, fracs, sep="~~")
  return(ret)
}


confun3 <- function(var1, dig=1, dfs, var2, var3="pt_study_id", dig2=0) {
  nlist <- length(dfs)
  vecs <- vector("list", nlist)
  for (i in 1:nlist) {
    tmpdat <- dfs[[i]][!is.na(dfs[[i]][[var2]])&!is.na(dfs[[i]][[var3]]), ]
    vecs[[i]] <- tmpdat[[var1]]
  }
  pcts <- character(nlist)
  fracs <- character(nlist)
  for (i in 1:nlist) {
    n <- sum(!is.na(vecs[[i]]))
    if (n==0) {
      pcts[i] <-  "~~~~~~~~~~~~"
      fracs[i] <- "{\\scriptsize N=0}"
    } else {
      avg <- format(round(mean(vecs[[i]], na.rm=TRUE),dig), nsmall=dig)
      std <- format(round(sd(vecs[[i]], na.rm=TRUE),dig), nsmall=dig)
      mdn <- format(round(median(vecs[[i]], na.rm=TRUE), dig2), nsmall=dig2)
      low <- format(round(quantile(vecs[[i]], probs=0.25, na.rm=TRUE), dig2), nsmall=dig2)
      up <- format(round(quantile(vecs[[i]], probs=0.75, na.rm=TRUE), dig2), nsmall=dig2)
      tmp <- paste("{\\scriptsize ", low, "}~", mdn, "{\\scriptsize ", up, "}~", "(",  avg, "$\\pm$", std, ")", sep="")
      
      pcts[i] <- gsub(" ", "~", sprintf("%20s", tmp))
      fracs[i] <- paste("{\\scriptsize N=", n, "}", sep="")
    }
  }
  ret <- paste(pcts, fracs, sep="~~")
  return(ret)
}

### calculate standardized difference ###

# for continuous variables #
sfun1 <- function(data, vr, ip) {
  y1 <- data[[vr]][data$practice %in% ip]
  y2 <- data[[vr]][data$practice %nin% ip]
  y1 <- y1[!is.na(y1)]
  y2 <- y2[!is.na(y2)]
  m1 <- mean(y1)
  m2 <- mean(y2)
  s1 <- sd(y1)
  s2 <- sd(y2)
  d <- (m1-m2)/sqrt(s1*s1/2 + s2*s2/2)
  return(d)
}

# for binary variable #
sfun2 <- function(data, vr, ip, lv=1) {
  y1 <- data[[vr]][data$practice %in% ip]
  y2 <- data[[vr]][data$practice %nin% ip]
  y1 <- y1[!is.na(y1)]
  y2 <- y2[!is.na(y2)]
  m1 <- mean(y1==lv)
  m2 <- mean(y2==lv)
  s1 <- m1*(1-m1)
  s2 <- m2*(1-m2)
  d <- (m1-m2)/sqrt(s1/2 + s2/2)
  return(d)
}

# for categorical variables #
sfun3 <- function(data, vr, ip) {
  y1 <- data[[vr]][data$practice %in% ip]
  y2 <- data[[vr]][data$practice %nin% ip]
  y1 <- y1[!is.na(y1)]
  y2 <- y2[!is.na(y2)]
  m1 <- as.numeric(prop.table(table(y1)))[-1]
  m2 <- as.numeric(prop.table(table(y2)))[-1]
  m1b <- 1-as.numeric(prop.table(table(y1)))[-1]
  m2b <- 1-as.numeric(prop.table(table(y2)))[-1]
  n <- length(m1)
  S <- matrix(NA, nrow=n, ncol=n)
  for (i in 1:n) {
    S[i,i] <- (m1[i]*m1b[i] + m2[i]*m2b[i])/2
  }
  for (i in 1:(n-1)) {
    
    for (j in (i+1):n) {
      S[i,j] <- (m1[i]*m1[j] + m2[i]*m2[j])/2
      S[j,i] <- (m1[i]*m1[j] + m2[i]*m2[j])/2
    }
  }
  diff <- m1-m2
  tmp <- t(diff) %*% solve(S) %*% diff
  d <- sqrt(tmp)
  return(d)
}

######New Employment Variable##########################################################

data$pt_education_level___1 <- ifelse(data$pt_education_level %in% 1, 1, 0)
data$pt_education_level___2 <- ifelse(data$pt_education_level %in% 2, 1, 0)
data$pt_education_level___3 <- ifelse(data$pt_education_level %in% 3, 1, 0)
data$pt_education_level___4 <- ifelse(data$pt_education_level %in% 4, 1, 0)
data$pt_education_level___5 <- ifelse(data$pt_education_level %in% 5, 1, 0)
data$pt_education_level___6 <- ifelse(data$pt_education_level %in% NA, 1, 0)

data$pt_education_level___1[is.na(data$pt_education_level)]<-NA
data$pt_education_level___2[is.na(data$pt_education_level)]<-NA
data$pt_education_level___3[is.na(data$pt_education_level)]<-NA
data$pt_education_level___4[is.na(data$pt_education_level)]<-NA
data$pt_education_level___5[is.na(data$pt_education_level)]<-NA
data$pt_education_level___6[is.na(data$pt_education_level)]<-NA

data$smoker___1 <- ifelse(data$smoker %in% 1, 1, 0)
data$smoker___2 <- ifelse(data$smoker %in% 2, 1, 0)
data$smoker___3 <- ifelse(data$smoker %in% 3, 1, 0)

data$smoker___1[is.na(data$smoker)]<-NA
data$smoker___2[is.na(data$smoker)]<-NA
data$smoker___3[is.na(data$smoker)]<-NA

data$asa_grade___1 <- ifelse(data$asa_grade %in% 1, 1, 0)
data$asa_grade___2 <- ifelse(data$asa_grade %in% 2, 1, 0)
data$asa_grade___3 <- ifelse(data$asa_grade %in% 3, 1, 0)
data$asa_grade___4 <- ifelse(data$asa_grade %in% 4, 1, 0)


data$asa_grade___1[is.na(data$asa_grade)]<-NA
data$asa_grade___2[is.na(data$asa_grade)]<-NA
data$asa_grade___3[is.na(data$asa_grade)]<-NA
data$asa_grade___4[is.na(data$asa_grade)]<-NA

data$symptom_duration2___1 <- ifelse(data$symptom_duration2 %in% 1, 1, 0)
data$symptom_duration2___2 <- ifelse(data$symptom_duration2 %in% 2, 1, 0)
data$symptom_duration2___3 <- ifelse(data$symptom_duration2 %in% 3, 1, 0)

data$symptom_duration2___1[is.na(data$symptom_duration2)]<-NA
data$symptom_duration2___2[is.na(data$symptom_duration2)]<-NA
data$symptom_duration2___3[is.na(data$symptom_duration2)]<-NA

data$surgical_approach___1 <- ifelse(data$surgical_approach %in% 1, 1, 0)
data$surgical_approach___2 <- ifelse(data$surgical_approach %in% 2, 1, 0)
data$surgical_approach___3 <- ifelse(data$surgical_approach %in% 3, 1, 0)
data$surgical_approach___4 <- ifelse(data$surgical_approach %in% 4, 1, 0)



data$surgical_approach___1[is.na(data$surgical_approach)]<-NA
data$surgical_approach___2[is.na(data$surgical_approach)]<-NA
data$surgical_approach___3[is.na(data$surgical_approach)]<-NA
data$surgical_approach___4[is.na(data$surgical_approach)]<-NA

data$pt_satisfaction_index.3m___1 <- ifelse(data$pt_satisfaction_index.3m %in% 1, 1, 0)
data$pt_satisfaction_index.3m___2 <- ifelse(data$pt_satisfaction_index.3m %in% 2, 1, 0)
data$pt_satisfaction_index.3m___3 <- ifelse(data$pt_satisfaction_index.3m %in% 3, 1, 0)
data$pt_satisfaction_index.3m___4 <- ifelse(data$pt_satisfaction_index.3m %in% 4, 1, 0)


data$pt_satisfaction_index.3m___1[is.na(data$pt_satisfaction_index.3m)]<-NA
data$pt_satisfaction_index.3m___2[is.na(data$pt_satisfaction_index.3m)]<-NA
data$pt_satisfaction_index.3m___3[is.na(data$pt_satisfaction_index.3m)]<-NA
data$pt_satisfaction_index.3m___4[is.na(data$pt_satisfaction_index.3m)]<-NA



data$place_discharged_to___6 <- ifelse(data$place_discharged_to %in% 6, 1, 0)
data$place_discharged_to___6[is.na(data$place_discharged_to)]<-NA
######New Employment Variable##########################################################

data$insurance1___1 <- ifelse(data$insurance1 %in% 1, 1, 0)
data$insurance1___2 <- ifelse(data$insurance1 %in% 2, 1, 0)
data$insurance1___3 <- ifelse(data$insurance1 %in% 3, 1, 0)
data$insurance1___4 <- ifelse(data$insurance1 %in% 4, 1, 0)
data$insurance1___5 <- ifelse(data$insurance1 %in% 5, 1, 0)


data$insurance1___1[is.na(data$insurance1)]<-NA
data$insurance1___2[is.na(data$insurance1)]<-NA
data$insurance1___3[is.na(data$insurance1)]<-NA
data$insurance1___4[is.na(data$insurance1)]<-NA
data$insurance1___5[is.na(data$insurance1)]<-NA


### generate table template ###

tab2fun <- function(datas) {
  M <- matrix("", nrow=45, ncol=2)
  M[2,] <- catfun1(var1="pgender", var2="pgender", ilev="2", dfs=datas)
  M[3,] <- catfun1(var1="pgender", var2="pgender", ilev="1", dfs=datas)
  M[4,] <- confun2(var1="ptage2", dig=1, dfs=datas)
  M[6,] <- catfun1(var1="prace___1", var2="prace___1", ilev="1", dfs=datas)
  M[7,] <- catfun1(var1="prace___2", var2="prace___2", ilev="1", dfs=datas)
  M[8,] <- catfun1(var1="prace___3", var2="prace___3", ilev="1", dfs=datas)
  M[9,] <- catfun1(var1="prace___4", var2="prace___4", ilev="1", dfs=datas)
  M[10,] <- catfun1(var1="prace___5", var2="prace___5", ilev="1", dfs=datas)
  M[11,] <- catfun1(var1="prace___6", var2="prace___6", ilev="1", dfs=datas)
  M[12,] <- catfun1(var1="ptethnicity", var2="ptethnicity", ilev="1", dfs=datas)
  M[14,] <- catfun1(var1="pt_education_level___1", var2="pt_education_level___1", ilev="1", dfs=datas)
  M[15,] <- catfun1(var1="pt_education_level___2", var2="pt_education_level___2", ilev="1", dfs=datas)
  M[16,] <- catfun1(var1="pt_education_level___3", var2="pt_education_level___3", ilev="1", dfs=datas)
  M[17,] <- catfun1(var1="pt_education_level___4", var2="pt_education_level___4", ilev="1", dfs=datas)
  M[18,] <- catfun1(var1="pt_education_level___5", var2="pt_education_level___5", ilev="1", dfs=datas)
  M[20,] <- catfun1(var1="insurance1___1", var2="insurance1___1", ilev="1", dfs=datas)
  M[21,] <- catfun1(var1="insurance1___2", var2="insurance1___2", ilev="1", dfs=datas)
  M[22,] <- catfun1(var1="insurance1___3", var2="insurance1___3", ilev="1", dfs=datas)
  M[23,] <- catfun1(var1="insurance1___4", var2="insurance1___4", ilev="1", dfs=datas)
  M[24,] <- catfun1(var1="insurance1___5", var2="insurance1___5", ilev="1", dfs=datas)
  M[26,] <- catfun1(var1="workers_comp", var2="workers_comp", ilev="1", dfs=datas)
  M[27,] <- catfun1(var1="liability_claim1", var2="liability_claim1", ilev="1", dfs=datas)
  M[28,] <- catfun1(var1="employment", var2="employment", ilev="1", dfs=datas)
  M[29,] <- catfun1(var1="full_part_time", var2="full_part_time", ilev="1", dfs=datas)
  M[30,] <- catfun1(var1="full_part_time", var2="full_part_time", ilev="2", dfs=datas)
  M[31,] <- catfun1(var1="employment", var2="employment", ilev="2", dfs=datas)
  M[32,] <- catfun1(var1="disability_reason1", var2="disability_reason1", ilev="1", dfs=datas)
  M[33,] <- catfun1(var1="disability_reason1", var2="disability_reason1", ilev="2", dfs=datas)
  M[34,] <- catfun1(var1="employment", var2="employment", ilev="3", dfs=datas)
  M[35,] <- catfun1(var1="unemployed", var2="unemployed", ilev="1", dfs=datas)
  M[36,] <- catfun1(var1="disability_reason2", var2="disability_reason2", ilev="1", dfs=datas)
  M[37,] <- catfun1(var1="disability_reason2", var2="disability_reason2", ilev="2", dfs=datas)
  M[38,] <- catfun1(var1="unemployed", var2="unemployed", ilev="2", dfs=datas)
  M[39,] <- catfun1(var1="unemployed", var2="unemployed", ilev="3", dfs=datas)
  M[40,] <- catfun1(var1="unemployed", var2="unemployed", ilev="4", dfs=datas)
  M[41,] <- catfun1(var1="employment", var2="employment", ilev="4", dfs=datas)
  M[42,] <- catfun1(var1="plan_return_work", var2="plan_return_work", ilev="1", dfs=datas)
  M[44,] <- catfun1(var1="activity_out_home", var2="activity_out_home", ilev="1", dfs=datas)
  M[45,] <- catfun1(var1="activity_inside_home", var2="activity_inside_home", ilev="1", dfs=datas)
  return(M)
}

hltab2 <- function(data, ipr) {
  hl <- c(1,4,6,7,8,9,10,11,12,14,15,16,17,18,20,21,22,23,24,26,27,28,29, 31, 32, 34, 35, 36, 38:42, 44,45)
  dv <- rep(0, times=35)
  dv[1] <- sfun2(data=data, vr='pgender', ip=ipr)
  dv[2] <- sfun1(data=data, vr='ptage2', ip=ipr)
  dv[3] <- sfun2(data=data, vr='prace___1', ip=ipr)
  dv[4] <- sfun2(data=data, vr='prace___2', ip=ipr)
  dv[5] <- sfun2(data=data, vr='prace___3', ip=ipr)
  dv[6] <- sfun2(data=data, vr='prace___4', ip=ipr)
  dv[7] <- sfun2(data=data, vr='prace___5', ip=ipr)
  dv[8] <- sfun2(data=data, vr='prace___6', ip=ipr)
  dv[9] <- sfun2(data=data, vr='ptethnicity', ip=ipr)
  dv[10] <- sfun2(data=data, vr='pt_education_level___1', ip=ipr)
  dv[11] <- sfun2(data=data, vr='pt_education_level___2', ip=ipr)
  dv[12] <- sfun2(data=data, vr='pt_education_level___3', ip=ipr)
  dv[13] <- sfun2(data=data, vr='pt_education_level___4', ip=ipr)
  dv[14] <- sfun2(data=data, vr='pt_education_level___5', ip=ipr)
  dv[15] <- sfun2(data=data, vr='insurance1___1', ip=ipr)
  dv[16] <- sfun2(data=data, vr='insurance1___2', ip=ipr)
  dv[17] <- sfun2(data=data, vr='insurance1___3', ip=ipr)
  dv[18] <- sfun2(data=data, vr='insurance1___4', ip=ipr)
  dv[19] <- sfun2(data=data, vr='insurance1___5', ip=ipr)
  dv[20] <- sfun2(data=data, vr='workers_comp', ip=ipr)
  dv[21] <- sfun2(data=data, vr='liability_claim1', ip=ipr)
  dv[22] <- sfun2(data=data, vr='employment', ip=ipr, lv=1)
  dv[23] <- sfun2(data=data, vr='full_part_time', ip=ipr)
  dv[24] <- sfun2(data=data, vr='employment', ip=ipr, lv=2)
  dv[25] <- sfun2(data=data, vr='disability_reason1', ip=ipr, lv=1)
  dv[26] <- sfun2(data=data, vr='employment', ip=ipr, lv=3)
  dv[27] <- sfun2(data=data, vr='unemployed', ip=ipr, lv=1)
  dv[28] <- sfun2(data=data, vr='disability_reason2', ip=ipr, lv=1)
  dv[29] <- sfun2(data=data, vr='unemployed', ip=ipr, lv=2)
  dv[30] <- sfun2(data=data, vr='unemployed', ip=ipr, lv=3)
  dv[31] <- sfun2(data=data, vr='unemployed', ip=ipr, lv=4)
  dv[32] <- sfun2(data=data, vr='employment', ip=ipr, lv=4)
  dv[33] <- sfun2(data=data, vr='plan_return_work', ip=ipr, lv=1)
  dv[34] <- sfun2(data=data, vr='activity_out_home', ip=ipr, lv=1)
  dv[35] <- sfun2(data=data, vr='activity_inside_home', ip=ipr, lv=1)
  dv2 <- ifelse(abs(dv)>=0.4, TRUE, FALSE)
  return(hl[dv2])
}



# Table of Medical History #
tab3fun <- function(datas) {
  M <- matrix("", nrow=38, ncol=2)
  M[2,] <- catfun1(var1="diabetes", var2="diabetes", ilev=c("1"), dfs=datas)
  M[3,] <- catfun1(var1="cad", var2="cad", ilev="1", dfs=datas)
  M[4,] <- catfun1(var1="osteoporosis", var2="osteoporosis", ilev="1", dfs=datas)
  M[5,] <- catfun1(var1="anxiety", var2="anxiety", ilev="1", dfs=datas)
  M[6,] <- catfun1(var1="depression", var2="depression", ilev="1", dfs=datas)
  M[8,] <- catfun1(var1="smoker___1", var2="smoker___1", ilev="1", dfs=datas)
  M[9,] <- catfun1(var1="smoker___2", var2="smoker___2", ilev="1", dfs=datas)
  M[10,] <- catfun1(var1="smoker___3", var2="smoker___3", ilev="1", dfs=datas)
  M[11,] <- confun2(var1="bmi", dig=1, dfs=datas, dig2=1)
  M[12,] <- catfun1(var1="revision", var2="revision", ilev="1", dfs=datas)
  M[14,] <- catfun1(var1="indication_surgery___1", var2="indication_surgery___1", ilev="1", dfs=datas)
  M[15,] <- catfun1(var1="indication_surgery___2", var2="indication_surgery___2", ilev="1", dfs=datas)
  M[16,] <- catfun1(var1="indication_surgery___3", var2="indication_surgery___3", ilev="1", dfs=datas)
  M[18,] <- catfun1(var1="underlying_pathology___1", var2="underlying_pathology___1", ilev="1", dfs=datas)
  M[19,] <- catfun1(var1="underlying_pathology___2", var2="underlying_pathology___2", ilev="1", dfs=datas)
  M[20,] <- catfun1(var1="underlying_pathology___3", var2="underlying_pathology___3", ilev="1", dfs=datas)
  M[21,] <- catfun1(var1="underlying_pathology___4", var2="underlying_pathology___4", ilev="1", dfs=datas)
  M[22,] <- catfun1(var1="underlying_pathology___5", var2="underlying_pathology___5", ilev="1", dfs=datas)
  M[23,] <- catfun1(var1="underlying_pathology___6", var2="underlying_pathology___6", ilev="1", dfs=datas)
  M[25,] <- catfun1(var1="symptom_duration2___1", var2="symptom_duration2___1", ilev="1", dfs=datas)
  M[26,] <- catfun1(var1="symptom_duration2___2", var2="symptom_duration2___2", ilev="1", dfs=datas)
  M[27,] <- catfun1(var1="symptom_duration2___3", var2="symptom_duration2___3", ilev="1", dfs=datas)
  M[29,] <- catfun1(var1="asa_grade___1", var2="asa_grade___1", ilev="1", dfs=datas)
  M[30,] <- catfun1(var1="asa_grade___2", var2="asa_grade___2", ilev="1", dfs=datas)
  M[31,] <- catfun1(var1="asa_grade___3", var2="asa_grade___3", ilev="1", dfs=datas)
  M[32,] <- catfun1(var1="asa_grade___4", var2="asa_grade___4", ilev="1", dfs=datas)
  M[33,] <- catfun1(var1="arthrodesis", var2="arthrodesis", ilev="1", dfs=datas)
  M[35,] <- catfun1(var1="surgical_approach___1", var2="surgical_approach___1", ilev="1", dfs=datas)
  M[36,] <- catfun1(var1="surgical_approach___2", var2="surgical_approach___2", ilev="1", dfs=datas)
  M[37,] <- catfun1(var1="surgical_approach___3", var2="surgical_approach___3", ilev="1", dfs=datas)
  M[38,] <- confun1(var1="levels", var2="levels", dfs=datas, var3="levels")
  return(M)
}

hltab3 <- function(data, ipr) {
  hl <- c(2,3,4,5,6,8,9,10,11,12,14,15,16,18,19,20,21,22,23,25,26,27,29:32,33,35:37,38)
  dv <- rep(0, times=31)
  dv[1] <- sfun2(data=data, vr='diabetes', ip=ipr)
  dv[2] <- sfun2(data=data, vr='cad', ip=ipr)
  dv[3] <- sfun2(data=data, vr='osteoporosis', ip=ipr)
  dv[4] <- sfun2(data=data, vr='anxiety', ip=ipr)
  dv[5] <- sfun2(data=data, vr='depression', ip=ipr)
  dv[6] <- sfun2(data=data, vr='smoker___1', ip=ipr)
  dv[7] <- sfun2(data=data, vr='smoker___2', ip=ipr)
  dv[8] <- sfun2(data=data, vr='smoker___3', ip=ipr)
  dv[9] <- sfun1(data=data, vr='bmi', ip=ipr)
  dv[10] <- sfun2(data=data, vr='revision', ip=ipr)
  dv[11] <- sfun2(data=data, vr='indication_surgery___1', ip=ipr)
  dv[12] <- sfun2(data=data, vr='indication_surgery___2', ip=ipr)
  dv[13] <- sfun2(data=data, vr='indication_surgery___3', ip=ipr)
  dv[14] <- sfun2(data=data, vr='underlying_pathology___1', ip=ipr)
  dv[15] <- sfun2(data=data, vr='underlying_pathology___2', ip=ipr)
  dv[16] <- sfun2(data=data, vr='underlying_pathology___3', ip=ipr)
  dv[17] <- sfun2(data=data, vr='underlying_pathology___4', ip=ipr)
  dv[18] <- sfun2(data=data, vr='underlying_pathology___5', ip=ipr)
  dv[19] <- sfun2(data=data, vr='underlying_pathology___6', ip=ipr)
  dv[20] <- sfun2(data=data, vr='symptom_duration2___1', ip=ipr)
  dv[21] <- sfun2(data=data, vr='symptom_duration2___2', ip=ipr)
  dv[22] <- sfun2(data=data, vr='symptom_duration2___3', ip=ipr)
  dv[23] <- sfun2(data=data, vr='asa_grade___1', ip=ipr)
  dv[24] <- sfun2(data=data, vr='asa_grade___2', ip=ipr)
  dv[25] <- sfun2(data=data, vr='asa_grade___3', ip=ipr)
  dv[26] <- sfun2(data=data, vr='asa_grade___4', ip=ipr)
  dv[27] <- sfun2(data=data, vr='arthrodesis_performed', ip=ipr)
  dv[28] <- sfun2(data=data, vr='surgical_approach___1', ip=ipr)
  dv[29] <- sfun2(data=data, vr='surgical_approach___2', ip=ipr)
  dv[30] <- sfun2(data=data, vr='surgical_approach___3', ip=ipr)
  dv[31] <- sfun1(data=data, vr='levels', ip=ipr)
  dv3 <- ifelse(abs(dv)>=0.4, TRUE, FALSE)
  return(hl[dv3])
}



# Table of surgical procedures #
tab6fun <- function(datas) {
  M <- matrix("", nrow=89, ncol=2)
  M[2,] <- catfun1(var1="asa_grade", var2="asa_grade", ilev="1", dfs=datas)
  M[3,] <- catfun1(var1="asa_grade", var2="asa_grade", ilev="2", dfs=datas)
  M[4,] <- catfun1(var1="asa_grade", var2="asa_grade", ilev="3", dfs=datas)
  M[5,] <- catfun1(var1="asa_grade", var2="asa_grade", ilev="4", dfs=datas)
  M[6,] <- catfun1(var1="arthrodesis", var2="arthrodesis", ilev="1", dfs=datas)
  
  M[8,] <- catfun1(var1="surgical_approach", var2="surgical_approach", ilev="2", dfs=datas)
  M[9,] <- catfun1(var1="surgical_approach", var2="surgical_approach", ilev="1", dfs=datas)
  M[10,] <- catfun1(var1="surgical_approach", var2="surgical_approach", ilev="3", dfs=datas)
  
  M[11,] <- confun1(var1="levels", var2="levels", dfs=datas, var3="levels")
  
  M[14,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind4", ilev="2", dfs=datas)
  M[15,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind4", ilev="1", dfs=datas)
  M[16,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind4", ilev="3", dfs=datas)
  M[17,] <- confun1(var1="levels", var2="ind1", var3="ind4", dfs=datas)
  
  M[19,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind8", ilev="2", dfs=datas)
  M[20,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind8", ilev="1", dfs=datas)
  M[21,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind8", ilev="3", dfs=datas)
  M[22,] <- confun1(var1="levels", var2="ind1", var3="ind8", dfs=datas)
  
  M[24,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind7", ilev="2", dfs=datas)
  M[25,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind7", ilev="1", dfs=datas)
  M[26,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind7", ilev="3", dfs=datas)
  M[27,] <- confun1(var1="levels", var2="ind1", var3="ind7", dfs=datas)
  
  M[29,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind9", ilev="2", dfs=datas)
  M[30,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind9", ilev="1", dfs=datas)
  M[31,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind9", ilev="3", dfs=datas)
  M[32,] <- confun1(var1="levels", var2="ind1", var3="ind9", dfs=datas)
  
  M[34,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind10", ilev="2", dfs=datas)
  M[35,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind10", ilev="1", dfs=datas)
  M[36,] <- catfun1(var1="surgical_approach", var2="ind1", var3="ind10", ilev="3", dfs=datas)
  M[37,] <- confun1(var1="levels", var2="ind1", var3="ind10", dfs=datas)
  
  M[40,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind4", ilev="2", dfs=datas)
  M[41,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind4", ilev="1", dfs=datas)
  M[42,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind4", ilev="3", dfs=datas)
  M[43,] <- confun1(var1="levels", var2="ind2", var3="ind4", dfs=datas)
  
  M[45,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind8", ilev="2", dfs=datas)
  M[46,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind8", ilev="1", dfs=datas)
  M[47,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind8", ilev="3", dfs=datas)
  M[48,] <- confun1(var1="levels", var2="ind2", var3="ind8", dfs=datas)
  
  M[50,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind7", ilev="2", dfs=datas)
  M[51,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind7", ilev="1", dfs=datas)
  M[52,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind7", ilev="3", dfs=datas)
  M[53,] <- confun1(var1="levels", var2="ind2", var3="ind7", dfs=datas)
  
  M[55,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind9", ilev="2", dfs=datas)
  M[56,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind9", ilev="1", dfs=datas)
  M[57,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind9", ilev="3", dfs=datas)
  M[58,] <- confun1(var1="levels", var2="ind2", var3="ind9", dfs=datas)
  
  M[60,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind10", ilev="2", dfs=datas)
  M[61,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind10", ilev="1", dfs=datas)
  M[62,] <- catfun1(var1="surgical_approach", var2="ind2", var3="ind10", ilev="3", dfs=datas)
  M[63,] <- confun1(var1="levels", var2="ind2", var3="ind10", dfs=datas)
  
  
  M[66,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind4", ilev="2", dfs=datas)
  M[67,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind4", ilev="1", dfs=datas)
  M[68,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind4", ilev="3", dfs=datas)
  M[69,] <- confun1(var1="levels", var2="ind3", var3="ind4", dfs=datas)
  
  M[71,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind8", ilev="2", dfs=datas)
  M[72,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind8", ilev="1", dfs=datas)
  M[73,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind8", ilev="3", dfs=datas)
  M[74,] <- confun1(var1="levels", var2="ind3", var3="ind8", dfs=datas)
  
  M[76,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind7", ilev="2", dfs=datas)
  M[77,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind7", ilev="1", dfs=datas)
  M[78,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind7", ilev="3", dfs=datas)
  M[79,] <- confun1(var1="levels", var2="ind3", var3="ind7", dfs=datas)
  
  M[81,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind9", ilev="2", dfs=datas)
  M[82,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind9", ilev="1", dfs=datas)
  M[83,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind9", ilev="3", dfs=datas)
  M[84,] <- confun1(var1="levels", var2="ind3", var3="ind9", dfs=datas)
  
  M[86,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind10", ilev="2", dfs=datas)
  M[87,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind10", ilev="1", dfs=datas)
  M[88,] <- catfun1(var1="surgical_approach", var2="ind3", var3="ind10", ilev="3", dfs=datas)
  M[89,] <- confun1(var1="levels", var2="ind3", var3="ind10", dfs=datas)
  return(M)
}
  
# Table of safety and quality #
tab7fun <- function(datas, datasb) {
  M <- matrix("", nrow=37, ncol=2)
  M[2,] <- catfun1(var1="place_discharged_to", var2="place_discharged_to", ilev="6", df=datasb)
  M[3,] <- catfun1(var1="mort30d", var2="mort30d", ilev="1", df=datasb)
  M[4,] <- catfun1(var1="mort3m", var2="mort3m", ilev="1", df=datasb)
  M[6,] <- catfun1(var1="pulmonary_embolism_30day", var2="pulmonary_embolism_30day", ilev="1", df=datas)
  M[7,] <- catfun1(var1="cva_30days", var2="cva_30days", ilev="1", df=datas)
  M[8,] <- catfun1(var1="mi_30day", var2="mi_30day", ilev="1", df=datas)
  M[9,] <- catfun1(var1="surgical_site_infect_30day", var2="surgical_site_infect_30day", ilev="1", df=datas)
  M[10,] <- catfun1(var1="hematoma", var2="hematoma", ilev="1", df=datas)
  M[11,] <- catfun1(var1="new_neuro_deficit_30day", var2="new_neuro_deficit_30day", ilev="1", df=datas)
  M[12,] <- catfun1(var1="majorade", var2="majorade", ilev="1", df=datas)
  
  M[14,] <- catfun1(var1="dvt_30day", var2="dvt_30day", ilev="1", df=datas)
  M[15,] <- catfun1(var1="uti_30days", var2="uti_30days", ilev="1", df=datas)
  M[16,] <- catfun1(var1="dysphagia_ng_tube", var2="dysphagia_ng_tube", ilev="1", df=datas)
  M[17,] <- catfun1(var1="dysphagia_no_ng_tube", var2="dysphagia_no_ng_tube", ilev="1", df=datas)
  M[18,] <- catfun1(var1="vocal_cord_paralysis", var2="vocal_cord_paralysis", ilev="1", df=datas)
  M[19,] <- catfun1(var1="csf_leak", var2="csf_leak", ilev="1", df=datas)
  M[20,] <- catfun1(var1="wound_dehiscence", var2="wound_dehiscence", ilev="1", df=datas)
  M[21,] <- catfun1(var1="pneumonia", var2="pneumonia", ilev="1", df=datas)
  M[22,] <- catfun1(var1="minorade", var2="minorade", ilev="1", df=datas)
  
  M[24,] <- catfun1(var1="pt_satisfaction_index.3m", var2="pt_satisfaction_index.3m", ilev="1", df=datas)
  M[19+6,] <- catfun1(var1="pt_satisfaction_index.3m", var2="pt_satisfaction_index.3m", ilev="2", df=datas)
  M[20+6,] <- catfun1(var1="pt_satisfaction_index.3m", var2="pt_satisfaction_index.3m", ilev="3", df=datas)
  M[21+6,] <- catfun1(var1="pt_satisfaction_index.3m", var2="pt_satisfaction_index.3m", ilev="4", df=datas)
  M[22+6,] <- catfun1(var1="pt_satisfaction_index2.3m", var2="pt_satisfaction_index2.3m", ilev="1", df=datas)
  
  M[24+6,] <- confun2(var1="estimated_blood_loss_cc3", dfs=datas, dig=0)
  M[25+6,] <- confun2(var1="length_of_surgery", dfs=datas, dig=0)
  M[26+6,] <- catfun1(var1="arthrodesis", var2="arthrodesis", ilev="1", df=datas)
  M[27+6,] <- confun2(var1="los3", dfs=datas, dig=1)
  M[28+6,] <- catfun1(var1="returned_to_or_with_30_day", var2="returned_to_or_with_30_day", ilev="1", df=datas)
  M[35,] <- catfun1(var1="readmit30day", var2="readmit30day", ilev="1", df=datas)
  M[36,] <- catfun1(var1="revision_surg_3mths.3m", var2="revision_surg_3mths.3m", ilev="1", df=datas)
  M[37,] <- catfun1(var1="readmit3m", var2="readmit3m", ilev="1", df=datas)
  return(M)
}


figure_construct <- function(datas) {
  M_f <- matrix(NA,nrow=9, ncol=2)
  M_f[1,] <- catfun1b(var1="pt_satisfaction_index.3m___1", var2="pt_satisfaction_index.3m___1", ilev="1", df=datas)
  M_f[2,] <- catfun1b(var1="pt_satisfaction_index.3m___2", var2="pt_satisfaction_index.3m___2", ilev="1", df=datas)
  M_f[3,] <- catfun1b(var1="pt_satisfaction_index.3m___3", var2="pt_satisfaction_index.3m___3", ilev="1", df=datas)
  M_f[4,] <- catfun1b(var1="pt_satisfaction_index.3m___4", var2="pt_satisfaction_index.3m___4", ilev="1", df=datas)
  M_f[5,] <- catfun1b(var1="pt_satisfaction_index2.3m", var2="pt_satisfaction_index2.3m", ilev="1", df=datas)
  M_f[6,] <- M_f[1,] + M_f[2,]
  M_f[7,] <- catfun1b(var1="readmit30day", var2="readmit30day", ilev="1", df=datas)
  M_f[8,] <- catfun1b(var1="revision_surg_3mths.3m", var2="revision_surg_3mths.3m", ilev="1", df=datas)
  M_f[9,] <- catfun1b(var1="readmit3m", var2="readmit3m", ilev="1", df=datas)
  return(M_f)
}




# table of utilization #
tab7bfun <- function(datas) { 
  M <- matrix("", nrow=45, ncol=2)
  M[2,] <- confun3(var1="estimated_blood_loss_cc3", var2="newcat1", dfs=datas, var3="estimated_blood_loss_cc3", dig=0)
  M[3,] <- confun3(var1="length_of_surgery", var2="newcat1", dfs=datas, var3="length_of_surgery", dig=0)
  M[4,] <- catfun1(var1="arthrodesis", var2="newcat1", ilev="1", df=datas)
  M[5,] <- confun3(var1="los3", var2="newcat1", dfs=datas, var3="los3", dig=1)
  M[6,] <- catfun1(var1="returned_to_or_with_30_day", var2="newcat1", ilev="1", df=datas)
  M[7,] <- catfun1(var1="readmit30day", var2="newcat1", ilev="1", df=datas)
  M[8,] <- catfun1(var1="revision_surg_3mths.3m", var2="newcat1", ilev="1", df=datas)
  M[9,] <- catfun1(var1="readmit3m", var2="newcat1", ilev="1", df=datas)
  
  M[11,] <- confun3(var1="estimated_blood_loss_cc3", var2="newcat2", dfs=datas, var3="estimated_blood_loss_cc3", dig=0)
  M[12,] <- confun3(var1="length_of_surgery", var2="newcat2", dfs=datas, var3="length_of_surgery", dig=0)
  M[13,] <- catfun1(var1="arthrodesis", var2="newcat2", ilev="1", df=datas)
  M[14,] <- confun3(var1="los3", var2="newcat2", dfs=datas, var3="los3", dig=1)
  M[15,] <- catfun1(var1="returned_to_or_with_30_day", var2="newcat2", ilev="1", df=datas)
  M[16,] <- catfun1(var1="readmit30day", var2="newcat2", ilev="1", df=datas)
  M[17,] <- catfun1(var1="revision_surg_3mths.3m", var2="newcat2", ilev="1", df=datas)
  M[18,] <- catfun1(var1="readmit3m", var2="newcat2", ilev="1", df=datas)
  
  M[20,] <- confun3(var1="estimated_blood_loss_cc3", var2="newcat3", dfs=datas, var3="estimated_blood_loss_cc3", dig=0)
  M[21,] <- confun3(var1="length_of_surgery", var2="newcat3", dfs=datas, var3="length_of_surgery", dig=0)
  M[22,] <- catfun1(var1="arthrodesis", var2="newcat3", ilev="1", df=datas)
  M[23,] <- confun3(var1="los3", var2="newcat3", dfs=datas, var3="los3", dig=1)
  M[24,] <- catfun1(var1="returned_to_or_with_30_day", var2="newcat3", ilev="1", df=datas)
  M[25,] <- catfun1(var1="readmit30day", var2="newcat3", ilev="1", df=datas)
  M[26,] <- catfun1(var1="revision_surg_3mths.3m", var2="newcat3", ilev="1", df=datas)
  M[27,] <- catfun1(var1="readmit3m", var2="newcat3", ilev="1", df=datas)
  
  M[29,] <- confun3(var1="estimated_blood_loss_cc3", var2="newcat4", dfs=datas, var3="estimated_blood_loss_cc3", dig=0)
  M[30,] <- confun3(var1="length_of_surgery", var2="newcat4", dfs=datas, var3="length_of_surgery", dig=0)
  M[31,] <- catfun1(var1="arthrodesis", var2="newcat4", ilev="1", df=datas)
  M[32,] <- confun3(var1="los3", var2="newcat4", dfs=datas, var3="los3", dig=1)
  M[33,] <- catfun1(var1="returned_to_or_with_30_day", var2="newcat4", ilev="1", df=datas)
  M[34,] <- catfun1(var1="readmit30day", var2="newcat4", ilev="1", df=datas)
  M[35,] <- catfun1(var1="revision_surg_3mths.3m", var2="newcat4", ilev="1", df=datas)
  M[36,] <- catfun1(var1="readmit3m", var2="newcat4", ilev="1", df=datas)
  
  M[38,] <- confun3(var1="estimated_blood_loss_cc3", var2="newcat5", dfs=datas, var3="estimated_blood_loss_cc3", dig=0)
  M[39,] <- confun3(var1="length_of_surgery", var2="newcat5", dfs=datas, var3="length_of_surgery", dig=0)
  M[40,] <- catfun1(var1="arthrodesis", var2="newcat4", ilev="1", df=datas)
  M[41,] <- confun3(var1="los3", var2="newcat5", dfs=datas, var3="los3", dig=1)
  M[42,] <- catfun1(var1="returned_to_or_with_30_day", var2="newcat5", ilev="1", df=datas)
  M[43,] <- catfun1(var1="readmit30day", var2="newcat5", ilev="1", df=datas)
  M[44,] <- catfun1(var1="revision_surg_3mths.3m", var2="newcat5", ilev="1", df=datas)
  M[45,] <- catfun1(var1="readmit3m", var2="newcat5", ilev="1", df=datas)
  return(M)
}


  

tab9pfun <- function(datas) {
  M <- matrix(NA, nrow=5, ncol=9)
  vars1 <- c("neck_pain_vas", "arm_pain_vas1", "ndiscore", "eq5dscore", "joa")
  vars2 <- paste(c("neck_pain_vas", "arm_pain_vas1", "ndiscore", "eq5dscore", "joa"), ".3m", sep="")
  vars3 <- paste(c("neck_pain_vas", "arm_pain_vas1", "ndiscore", "eq5dscore", "joa"), ".12m", sep="")
  for (i in 1:5) {
    tmp_12 <- subset(datas, analyzed_12month)
    tmp_3 <- subset(datas, analyzed_3month)
    tmp_base<- datas
    M[i,1] <- sum(!is.na(tmp_base[,vars1[i]]))
    M[i,4] <- sum(!is.na(tmp_3[,vars2[i]]))
    M[i,7] <- sum(!is.na(tmp_12[,vars3[i]]))
    M[i,2] <- mean(tmp_base[,vars1[i]], na.rm = TRUE)
    M[i,3] <- sd(tmp_base[,vars1[i]], na.rm = TRUE)
    M[i,5] <- mean(tmp_3[,vars2[i]], na.rm = TRUE)
    M[i,6] <- sd(tmp_3[,vars2[i]], na.rm = TRUE)
    if (nrow(tmp_12)>0) {
      M[i,8] <- mean(tmp_12[,vars3[i]], na.rm = TRUE)
      M[i,9] <- sd(tmp_12[,vars3[i]], na.rm = TRUE)
    }
  }
  return(M)
}


tab9fun <- function(ptab, datas) {
  M <- matrix("", nrow=8, ncol=12)
  M[c(2,3,4,5,6),c(1,3,5,7,9,11)] <- ptab[,c(1,4,7,10,13,16)]
  digs <- c(1,1,1,1,2)
  for (i in 1:5){
    if (ptab[i,1] > 0) {
      M[(i+1), 2] <- paste(format(round(ptab[i,2], digs[i]), nsmall=digs[i]), "$\\pm$", format(round(ptab[i,3], digs[i]), nsmall=digs[i]), sep="")
      M[(i+1), 4] <- paste(format(round(ptab[i,5], digs[i]), nsmall=digs[i]), "$\\pm$", format(round(ptab[i,6], digs[i]), nsmall=digs[i]), sep="")
      M[(i+1), 6] <- paste(format(round(ptab[i,8], digs[i]), nsmall=digs[i]), "$\\pm$", format(round(ptab[i,9], digs[i]), nsmall=digs[i]), sep="")
    }
    M[(i+1), 8] <- paste(format(round(ptab[i,11], digs[i]), nsmall=digs[i]), "$\\pm$", format(round(ptab[i,12], digs[i]), nsmall=digs[i]), sep="")
    M[(i+1), 10] <- paste(format(round(ptab[i,14], digs[i]), nsmall=digs[i]), "$\\pm$", format(round(ptab[i,15], digs[i]), nsmall=digs[i]), sep="")
    M[(i+1), 12] <- paste(format(round(ptab[i,17], digs[i]), nsmall=digs[i]), "$\\pm$", format(round(ptab[i,18], digs[i]), nsmall=digs[i]), sep="")
  }
  
  M[7,c(3,4,5,6)] <- catfun2a(ds=datas[[1]], vars=c('return_to_work.3m','return_to_work.12m'))
  M[7,c(9,10,11,12)] <- catfun2a(ds=datas[[2]], vars=c('return_to_work.3m','return_to_work.12m'))
  M[8,c(3,4,5,6)] <- catfun2a(ds=datas[[1]], vars=c('pt_satisfaction_index.3m','pt_satisfaction_index.12m'))
  M[8,c(9,10,11,12)] <- catfun2a(ds=datas[[2]], vars=c('pt_satisfaction_index.3m','pt_satisfaction_index.12m'))
  return(M)
}



### plot functions ###
plotfun1 <- function(x, yli, yma, yla, mai, yll) {
  plot(1:10, 1:10, xlim=c(1.5,8), ylim=yli, type="n", axes=FALSE, xlab="Time after Surgery", ylab=yla, main=mai)
  legend(x=9, y=0,legend=c('QOD',latexTranslate(pracs[k])), pch=c(20,8), col=c('red','blue'), bty='n', xpd=NA, ncol=1)
  axis(side=1, at=c(2,5,8), labels=paste(c("Baseline", "3-month", "12-month"), "\n N=", c(x[1], x[4], x[7]), sep=''), col='white')
  axis(side=2, at=yma, las=2, labels=yll)
  
  if (x[1]>4) {
    arrows(2, x[2]-x[3], 2, x[2]+x[3], angle=90, code=3, length=0.05)
    arrows(5, x[5]-x[6], 5, x[5]+x[6], angle=90, code=3, length=0.05)
    segments(2, x[2], 5, x[5])
    points(c(2,5), x[c(11,14)],  pch=c(20,20), col=c('red','red'), cex=3)
    points(c(2,5), x[c(2,5)],  pch=c(8,8), col=c('blue','blue'), cex=1)
  }
  
  if (x[7]>4) {
    arrows(8, x[8]-x[9], 8, x[8]+x[9], angle=90, code=3, length=0.05)
    segments(5, x[5], 8, x[8])
    points(c(8), x[c(17)], pch=c(20), col=c('red'), cex=3)
    points(c(8), x[c(8)], pch=c(8), col=c('blue'), cex=1)
  }
}

plotfun1b <- function(dat, iv, xyli, xyma, xla, yla, idline, mai) {
  vars1 <- c("neck_pain_vas", "arm_pain_vas1", "ndiscore", "eq5dscore", "joa")
  vars2 <- paste(c("neck_pain_vas", "arm_pain_vas1", "ndiscore", "eq5dscore", "joa"), ".3m", sep="")
  tmp <- dat[!is.na(dat[[vars1[iv]]]) & !is.na(dat[[vars2[iv]]]),]
  x <- tmp[[vars1[[iv]]]]
  y <- tmp[[vars2[[iv]]]]
  plot(jitter(x,1), jitter(y,1), xlim=xyli, ylim=xyli, main=mai, axes=FALSE, xlab=xla, ylab=yla, type="p", pch=1, cex=1, col='grey40')
  axis(side=1, at=xyma)
  axis(side=2, at=xyma, las=2)
  segments(idline[1], idline[2], idline[3], idline[4], lty=3) 
}


plotfun2 <- function(datas, pltnam, ptab) {
  pdf(pltnam, width=8, height=12)
  par(mfrow=c(3,2), mar=c(7,10,6,7))
  plotfun1(x=ptab[1,], mai="Neck Pain", yli=c(0,10), yma=c(0,2,4,6,8,10), yla="Pain Score", yll=c("  \n 0\n No Pain", 2,4,6,8, "  \n 10\n Worst Pain"))
  plotfun1(x=ptab[2,], mai="Arm Pain", yli=c(0,10), yma=c(0,2,4,6,8,10), yla="Pain Score", yll=c("  \n 0\n No Pain", 2,4,6,8, "  \n 10\n Worst Pain"))
  plotfun1(x=ptab[3,], mai="Neck Disability Index", yli=c(0,100), yma=c(0,20,40,60,80,100), yla="NDI", yll=c("  \n 0\n No Impairment", 20,40,60,80, "  \n 100\n Functional Impairment"))
  plotfun1(x=ptab[4,], mai="Euroqual Quality of Life", yli=c(-0.1, 1.0), yma=c(-0.1, 0, 0.2, 0.4, 0.6, 0.8, 1.0), yla="EQ-5D", yll=c("  \n -0.1\n Worst Health", 0,0.2,0.4,0.6,0.8, "  \n 1.0\n Best Health"))
  plotfun1(x=ptab[5,], mai="mJOA", yli=c(0,17), yma=c(0,3,6,9,12,14,17), yla="mJOA", yll=c("  \n 0\n Severe", 3,6,9,12,14, "  \n 17\n Mild"))
  dev.off()
}


plot12mfun1 <- function(x, yli, yma, yla, mai, yll) {
  plot(1:10, 1:10, xlim=c(1.5,8), ylim=yli, type="n", axes=FALSE, xlab="Time after Surgery", ylab=yla, main=mai)
  legend(x=9, y=0,legend=c('QOD',latexTranslate(pracs[k])), pch=c(20,8), col=c('red','blue'), bty='n', xpd=NA, ncol=1)
  axis(side=1, at=c(2,5,8), labels=paste(c("Baseline", "3-month", "12-month"), "\n N=", c(x[1], x[4], x[7]), sep=''), col='white')
  axis(side=2, at=yma, las=2, labels=yll)
  
  if (x[1]>4) {
    arrows(2, x[2]-x[3], 2, x[2]+x[3], angle=90, code=3, length=0.05)
    arrows(5, x[5]-x[6], 5, x[5]+x[6], angle=90, code=3, length=0.05)
    segments(2, x[2], 5, x[5])
    points(c(2,5), x[c(11,14)],  pch=c(20,20), col=c('red','red'), cex=3)
    points(c(2,5), x[c(2,5)],  pch=c(8,8), col=c('blue','blue'), cex=1)
  }
  
  if (x[7]>4) {
    arrows(8, x[8]-x[9], 8, x[8]+x[9], angle=90, code=3, length=0.05)
    segments(5, x[5], 8, x[8])
    points(c(8), x[c(17)], pch=c(20), col=c('red'), cex=3)
    points(c(8), x[c(8)], pch=c(8), col=c('blue'), cex=1)
  }
}

plot12mfun1b <- function(dat, iv, xyli, xyma, xla, yla, idline, mai) {
  vars1 <- c("neck_pain_vas", "arm_pain_vas1", "ndiscore", "eq5dscore", "joa")
  vars2 <- paste(c("neck_pain_vas", "arm_pain_vas1", "ndiscore", "eq5dscore", "joa"), ".3m", sep="")
  tmp <- dat[!is.na(dat[[vars1[iv]]]) & !is.na(dat[[vars2[iv]]]),]
  x <- tmp[[vars1[[iv]]]]
  y <- tmp[[vars2[[iv]]]]
  plot(jitter(x,1), jitter(y,1), xlim=xyli, ylim=xyli, main=mai, axes=FALSE, xlab=xla, ylab=yla, type="p", pch=1, cex=1, col='grey40')
  axis(side=1, at=xyma)
  axis(side=2, at=xyma, las=2)
  segments(idline[1], idline[2], idline[3], idline[4], lty=3) 
}

plot12mfun1c <- function(dat, iv, xyli, xyma, xla, yla, idline, mai) {
  vars1 <- paste(c("neck_pain_vas", "arm_pain_vas1", "ndiscore", "eq5dscore", "joa"), ".3m", sep="")
  vars2 <- paste(c("neck_pain_vas", "arm_pain_vas1", "ndiscore", "eq5dscore", "joa"), ".12m", sep="")
  tmp <- dat[!is.na(dat[[vars1[iv]]]) & !is.na(dat[[vars2[iv]]]),]
  x <- tmp[[vars1[[iv]]]]
  y <- tmp[[vars2[[iv]]]]
  plot(jitter(x,1), jitter(y,1), xlim=xyli, ylim=xyli, main=mai, axes=FALSE, xlab=xla, ylab=yla, type="p", pch=1, cex=1, col='grey40')
  axis(side=1, at=xyma)
  axis(side=2, at=xyma, las=2)
  segments(idline[1], idline[2], idline[3], idline[4], lty=3) 
}

plot12mfun2 <- function(datas, pltnam, ptab) {
  pdf(pltnam, width=6, height=17)
  par(mfrow=c(4,1), mar=c(4,9.3,4,11))
  plot12mfun1(x=ptab[1,], mai="Neck Pain", yli=c(0,10), yma=c(0,2,4,6,8,10), yla="Pain Score", yll=c("  \n 0\n No Pain", 2,4,6,8, "  \n 10\n Worst Pain"))
  plot12mfun1(x=ptab[2,], mai="Arm Pain", yli=c(0,10), yma=c(0,2,4,6,8,10), yla="Pain Score", yll=c("  \n 0\n No Pain", 2,4,6,8, "  \n 10\n Worst Pain"))
  plot12mfun1(x=ptab[3,], mai="Neck Disability Index", yli=c(0,100), yma=c(0,20,40,60,80,100), yla="NDI", yll=c("  \n 0\n No Impairment", 20,40,60,80, "  \n 100\n Functional Impairment"))
  plot12mfun1(x=ptab[4,], mai="Euroqual Quality of Life", yli=c(-0.1, 1.0), yma=c(-0.1, 0, 0.2, 0.4, 0.6, 0.8, 1.0), yla="EQ-5D", yll=c("  \n -0.1\n Worst Health", 0,0.2,0.4,0.6,0.8, "  \n 1.0\n Best Health"))
  plot12mfun1(x=ptab[5,], mai="mJOA", yli=c(0,17), yma=c(0,3,6,9,12,14,17), yla="mJOA", yll=c("  \n 0\n Severe", 3,6,9,12,14, "  \n 17\n Mild"))
  dev.off()
}


#### generate the follow up plot #############

## only for site with >20 12m followup ##



## only for site with >20 12m followup ##

if(length(pracs1) > 0L) {
  n <- nrow(data_follow_up)
  #d$practice <- sub("^([^*]+)(\\*(.+))?_LP[0-9]{4}$", "\\1", d$pt_study_id)
  #d$practice[d$practice=="Cornell"]<-"ABC"
  #d$practice[d$practice=="BSSNY"]<-"ABC"
  #d$practice[d$practice=="NSARVA"]<-"ABC"
  #d$practice[d$practice=="Semmes"]<-"ABC"
  #d$sub_practice <- sub("^([^*]+)(\\*(.+))?_LP[0-9]{4}$", "\\3", d$pt_study_id)
  data_follow_up_site <- subset(data_follow_up, practice %in% pracs1)
  dfp <- aggregate(cbind(usefull3month, usefull12month, analysis3month, analysis12month) ~ practice, FUN=sum, data=data_follow_up_site)
  colnames(dfp) <- c('practice', 'x3m', 'x12m', 'y3m', 'y12m')
  ##########Ben ekledim#################
  dfp$QOD_3base<-sum(dfp$x3m,na.rm = FALSE)
  dfp$QOD_12base<-sum(dfp$x12m,na.rm = FALSE)
  dfp$QOD_3m<-sum(dfp$y3m,na.rm = FALSE)
  dfp$QOD_12m<-sum(dfp$y12m,na.rm = FALSE)
  dfp$qod_fu3m <- round(dfp$QOD_3m/dfp$QOD_3base,2)
  dfp$qod_fu12m <-round(dfp$QOD_12m/dfp$QOD_12base,2)
  ################
  dfp$fu3m <-0
  dfp$fu12m <-0
  dfp$fu3m <- round(dfp$y3m/dfp$x3m,2)
  dfp$fu12m <- round(dfp$y12m/dfp$x12m,2)
  dfp$fu <- round(dfp$fu3m + dfp$fu12m,2)
  dfp <- dfp[order(dfp$fu),]
  nm <- nrow(dfp)
  dfp$center <- nm:1
  num.min <- min(dfp$x12m)
  num.max <- max(dfp$x3m)
}





##############################################
#### run .rnw file to generate pdf report ####
##############################################
data1 <- data
data2o <- subset(data, analyzed_3month | analyzed_12month)
data2 <- subset(data, analyzed_12month)
data3 <- subset(data, analyzed_3month)



load(file='cervical_modrst2.RData')
mlCap <- function(x) {
  sprintf("\\begin{tabular}{l}%s \\end{tabular}", x)
}
pracs <- pracs1
filepracs <- gsub('*', '+', pracs, fixed=TRUE)

for (k in seq_along(pracs)) {
    Sweave("cervical_report_generate.Rnw", output=paste0(filepracs[k], ".tex"))
}

for (k in seq_along(pracs)) {
    for (q in 1:3) {
        if (system(paste0("pdflatex -halt-on-error ", filepracs[k], ".tex")) != 0L)
            stop("Unable to compile latex document ", filepracs[k], ".tex")
    }
}

