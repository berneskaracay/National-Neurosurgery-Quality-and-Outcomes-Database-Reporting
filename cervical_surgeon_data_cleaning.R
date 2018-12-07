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
library("stringr", lib.loc="~/R/win-library/3.4")
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
data$surgeon_name <- data$surgeon
data$surgeon_name <- str_replace_all(data$surgeon_name, "[[:punct:]]", " ")
data$surgeon <- sub('^.*\\(([0-9]+)\\)$', '\\1', as.character(data$surgeon))
data$surg_location <- sub('^.*\\(([0-9]+)\\)$', '\\1', as.character(data$surg_location))
#liste=c("Semmes","Vanderbilt","Duke")
#data <- subset(data, practice %in% liste)
# merge the data with index #
data_follow_up <- merge(data, d_follow_up[,c('pt_study_id', 'analyzed_3month', 'analyzed_12month', 'analysis3month', 'analysis12month',"usefull3month","usefull12month")], by='pt_study_id', all.y=TRUE)



data <- merge(data, d[,c('pt_study_id', 'analyzed_3month', 'analyzed_12month', 'analysis3month', 'analysis12month')], by='pt_study_id', all.y=TRUE)


data <- data[order(data$surgeon),]
varnames<-c('pt_study_id', 'surgeon','surgeon_name')
data_surgeon <-data[varnames]
data_surgeon$num <- ave(data_surgeon$pt_study_id, data_surgeon$surgeon , FUN = seq_along)
data_name <- subset(data_surgeon, num %in% 1)


varnames<-c('surgeon','surgeon_name')
data_name <-data_name[varnames]


colnames(data_name)[colnames(data_name)=="surgeon_name"] <- "surgeon_new_id"
data <- merge(data,data_name,by="surgeon")



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
  text(50, 7.6, '3-Month Post-Surgery', pos=3)
  text(150+sp, 7.6, '12-Month Post-Surgery', pos=3)
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



data$smoker___1 <- ifelse(data$smoker %in% 1, 1, 0)
data$smoker___2 <- ifelse(data$smoker %in% 2, 1, 0)
data$smoker___3 <- ifelse(data$smoker %in% 3, 1, 0)



data$asa_grade___1 <- ifelse(data$asa_grade %in% 1, 1, 0)
data$asa_grade___2 <- ifelse(data$asa_grade %in% 2, 1, 0)
data$asa_grade___3 <- ifelse(data$asa_grade %in% 3, 1, 0)
data$asa_grade___4 <- ifelse(data$asa_grade %in% 4, 1, 0)

data$symptom_duration2___1 <- ifelse(data$symptom_duration2 %in% 1, 1, 0)
data$symptom_duration2___2 <- ifelse(data$symptom_duration2 %in% 2, 1, 0)
data$symptom_duration2___3 <- ifelse(data$symptom_duration2 %in% 3, 1, 0)



data$surgical_approach___1 <- ifelse(data$surgical_approach %in% 1, 1, 0)
data$surgical_approach___2 <- ifelse(data$surgical_approach %in% 2, 1, 0)
data$surgical_approach___3 <- ifelse(data$surgical_approach %in% 3, 1, 0)
data$surgical_approach___4 <- ifelse(data$surgical_approach %in% 4, 1, 0)

data$pt_satisfaction_index.3m___1 <- ifelse(data$pt_satisfaction_index.3m %in% 1, 1, 0)
data$pt_satisfaction_index.3m___2 <- ifelse(data$pt_satisfaction_index.3m %in% 2, 1, 0)
data$pt_satisfaction_index.3m___3 <- ifelse(data$pt_satisfaction_index.3m %in% 3, 1, 0)
data$pt_satisfaction_index.3m___4 <- ifelse(data$pt_satisfaction_index.3m %in% 4, 1, 0)


data$place_discharged_to___6 <- ifelse(data$place_discharged_to %in% 6, 1, 0)

######New Employment Variable##########################################################

data$insurance1___1 <- ifelse(data$insurance1 %in% 1, 1, 0)
data$insurance1___2 <- ifelse(data$insurance1 %in% 2, 1, 0)
data$insurance1___3 <- ifelse(data$insurance1 %in% 3, 1, 0)
data$insurance1___4 <- ifelse(data$insurance1 %in% 4, 1, 0)
data$insurance1___5 <- ifelse(data$insurance1 %in% 5, 1, 0)










