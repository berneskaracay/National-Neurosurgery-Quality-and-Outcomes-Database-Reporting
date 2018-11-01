####################################################################
#### This is R code to generate lumbar surgeon quarterly report ####
####################################################################

## put the following files in the working directory ##
# patient_lumbar_index.rda #
# patient_lumbar.csv #
# eq5dscore.csv #
# lumbar_surgeon_report.rnw #


## create the following two folders in the working directory ##
# figs #

## generated pdf reports will be in the working directory ##

rm(list=ls())

### set working directory###
setwd("C:\\Users\\karacb1\\Desktop\\qod-project-codes")

library(Hmisc)
library(rms)
library(MASS)

#################################################
#### organize data set and prepare variables ####
#################################################

# download date #
dldate <- '2018-01-08'

### read in Thomas' index data ###
load('patient_lumbar_index.rda')
d$death3m <- ifelse(d$deadhospital %in% TRUE | d$dead30day %in% TRUE | d$dead3month %in% TRUE, TRUE, FALSE)
d$analyzed_3month <- ifelse(d$analysis3month %in% TRUE | d$death3m %in% TRUE, TRUE, FALSE)
d$analyzed_12month <- ifelse(d$analysis12month %in% TRUE | d$dead12month %in% TRUE, TRUE, FALSE)
# only include followed up at 3 month or 12 month #
d <- subset(d, analyzed_3month | analyzed_12month)

### read in patient data set ###
data=read.csv('patient_lumbar.csv', stringsAsFactors=FALSE)
# combine baseline data and 3/12-month data #
dat1 <- subset(data, redcap_event_name=="baseline_arm_1")
dat1 <- dat1[!duplicated(dat1$pt_study_id),]
dat2 <- subset(data, redcap_event_name=="3month_arm_1")
dat2 <- dat2[!duplicated(dat2$pt_study_id),]
dat3 <- subset(data, redcap_event_name=="12month_arm_1")
dat3 <- dat3[!duplicated(dat3$pt_study_id),]
# change 3month ODI and EQ5D colnames #
vid <- which(names(data) %in% c('patient_baseline_interview_complete', 'patient_interview_complete', 'questionnaires_complete', 'qcdr_followup_to_surgery_complete', 'date_surgery_performed'))
names(dat2)[(vid[2]+1):vid[4]] <- paste(names(dat1)[(vid[2]+1):vid[4]], ".3m", sep="")
names(dat3)[(vid[2]+1):vid[4]] <- paste(names(dat1)[(vid[2]+1):vid[4]], ".12m", sep="")
# merge the data sets #
dat1b <- dat1[c(1:vid[1], (vid[3]+1):vid[5])]
dat2b <- dat2[c(1,(vid[1]+1):vid[4])]
dat3b <- dat3[c(1,(vid[2]+1):vid[4])]
data <- merge(dat1b, dat2b, by=c("pt_study_id"), all=TRUE)
data <- merge(data, dat3b, by=c('pt_study_id'), all=TRUE)

# extract practice/center name, patient id, surgeon id, hospital/surg_location #
n <- nrow(data)
tmp <- gsub(pattern='_LP([0-9*]+)', '', data$pt_study_id)
data$practice <- gsub('_', '-', tmp)
data$surgeon <- sub('^.*\\(([0-9]+)\\)$', '\\1', as.character(data$surgeon))
data$surg_location <- sub('^.*\\(([0-9]+)\\)$', '\\1', as.character(data$surg_location))
liste=c("Cornell")
data <- subset(data, practice %in% liste)
# merge the data with index #
data <- merge(data, d[,c('pt_study_id', 'analyzed_3month', 'analyzed_12month', 'analysis3month', 'analysis12month')], by='pt_study_id', all.y=TRUE)
# only include sites with at least 20 patients followed up at 3 month #
tab <- with(subset(data, analysis3month), table(practice))
pracs1 <- names(tab)[tab>=20]
data <- subset(data, practice %in% pracs1)
# create 12m analysis data indicator #
tab <- with(subset(data, analysis12month), table(practice))
pracs2 <- names(tab)[tab>=20]
data$select12m <- data$practice %in% pracs2 & data$analysis12month



### prepare variables ###
data$pgender = factor(data$pgender,levels=c("2","1"))
data$age_entered <- as.numeric(data$age_entered)
data$pt_age <- as.numeric(as.Date(data$surgerydte1, format='%Y-%m-%d') - as.Date(data$pt_dob, format='%Y-%m-%d'))/365.25
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
data$activity_out_home = factor(data$activity_out_home,levels=c("1","0"))
data$activity_inside_home = factor(data$activity_inside_home,levels=c("1","0"))
data$any_major_surgery_in_the_p = factor(data$any_major_surgery_in_the_p,levels=c("1","0"))
data$diabetes = factor(data$diabetes,levels=c("0","1"))
data$cad = factor(data$cad,levels=c("1","0"))
data$osteoporosis = factor(data$osteoporosis,levels=c("1","0"))
data$anxiety = factor(data$anxiety,levels=c("1","0"))
data$depression = factor(data$depression,levels=c("1","0"))
data$smoker = factor(data$smoking_status,levels=c("1","2","3","4"))
data$pt_height_in2 <- 2.54*data$pt_height_in
data$pt_height_cm2 <- as.numeric(data$pt_height_cm)
data$pt_height <- rowMeans(data[c("pt_height_in2", "pt_height_cm2")], na.rm=TRUE)
data$pt_wt_lb2 <- 0.453592*data$pt_wt_lb
data$pt_wt <- rowMeans(data[c("pt_wt_lb2", "pt_wt_kg")], na.rm=TRUE)
data$bmi <- 10000*data$pt_wt/(data$pt_height)^2

# diagnosis #
data$principal_spine_diagnosis1 <-  data$principal_spine_diagnosis2 <-  data$principal_spine_diagnosis3 <-  data$principal_spine_diagnosis4 <-  data$principal_spine_diagnosis5 <-  data$principal_spine_diagnosis6 <- 0
data$principal_spine_diagnosis1[data$principal_spine_diagnosis___3 %in% 1] <- 1
data$principal_spine_diagnosis2[data$principal_spine_diagnosis___1 %in% 1] <- 1
data$principal_spine_diagnosis3[data$principal_spine_diagnosis___4 %in% 1] <- 1
data$principal_spine_diagnosis4[data$principal_spine_diagnosis___5 %in% 1] <- 1
data$principal_spine_diagnosis5[data$principal_spine_diagnosis___6 %in% 1] <- 1
data$principal_spine_diagnosis6[data$principal_spine_diagnosis___9 %in% 1] <- 1
data$diagnosis1 <- ifelse(data$principal_spine_diagnosis1%in% 1, 1, NA)
data$diagnosis2 <- ifelse(data$principal_spine_diagnosis2%in% 1, 1, NA)
data$diagnosis3 <- ifelse(data$principal_spine_diagnosis3%in% 1, 1, NA)
data$diagnosis4 <- ifelse(data$principal_spine_diagnosis4%in% 1, 1, NA)
data$diagnosis5 <- ifelse(data$principal_spine_diagnosis5%in% 1, 1, NA)
data$diagnosis6 <- ifelse(data$principal_spine_diagnosis6%in% 1, 1, NA)

# surgical subgroups #
data$newcat1 <- data$newcat2 <- data$newcat3 <- data$newcat4 <- data$newcat5 <- data$newcat6 <- NA
data$newcat1[(data$principal_spine_diagnosis___1 %in% 1 & data$laminectomy_performed %in% 1 & data$arthrodesis_performed %nin% 1 & data$osteotomy_performed %nin% 1 & data$interbody1_instrum %nin% 1)] <- 1
data$newcat2[(data$principal_spine_diagnosis___1 %nin% 1 & data$laminectomy_performed %in% 1 & data$arthrodesis_performed %nin% 1 & data$osteotomy_performed %nin% 1 & data$interbody1_instrum %nin% 1)] <- 1
data$newcat3[(data$surgical_approach %in% 1 & data$arthrodesis_performed %in% 1 & data$interbody_graft_1 %in% 0 )] <- 1
data$newcat4[(data$surgical_approach %in% 1 & data$arthrodesis_performed %in% 1 & data$interbody_graft_1 %in% 1)] <- 1
data$newcat5[(data$surgical_approach %in% 2 & data$arthrodesis_performed %in% 1 & data$interbody_graft_1 %in% 1 )] <- 1
data$newcat6[(data$surgical_approach %in% 3 & data$arthrodesis_performed %in% 1 & data$interbody_graft_1 %in% 1)] <- 1

data$surgical_approach <- factor(data$surgical_approach, levels=1:4)
data$dominant_symptom1 = factor(data$dominant_symptom1,levels=c("1","2","3"))
data$motor_def2 = factor(data$motor_def2,levels=c("1","0"))
data$symptom_duration2 = factor(data$symptom_duration2,levels=c("1","2"))
data$asa_grade = factor(data$asa_grade,levels=c("1","2","3","4"))
data$arthrodesis_performed = factor(data$arthrodesis_performed,levels=c("1","0"))
data$laminectomy_performed = factor(data$laminectomy_performed,levels=c("1","0"))
data$laminectomy_level = factor(data$laminectomy_level,levels=1:7)
data$interbody_graft <- factor(data$interbody_graft_1, levels=1:0)
data$place_discharged_to = factor(data$place_discharged_to,levels=c("1","2","3","4","5","6","7"))
data$patient_died_within_30_day = factor(data$patient_died_within_30_day,levels=c("1","0"))

data$pulmonary_embolism_30day <- as.numeric(as.character(data$pulmonary_embolism_30day))
data$cva <- as.numeric(as.character(data$cva))
data$mi_30day <- as.numeric(as.character(data$mi_30day))
data$surgical_site_infect_30day <- as.numeric(as.character(data$surgical_site_infect_30day))
data$hematoma <- as.numeric(as.character(data$hematoma))
data$new_neuro_deficit <- as.numeric(as.character(data$new_neuro_deficit))
data$majoradep <- rowSums(data[c("pulmonary_embolism_30day", "cva", "mi_30day", "surgical_site_infect_30day", "hematoma", "new_neuro_deficit")], na.rm=TRUE)
data$majorade <- ifelse(data$majoradep%in%c(1:6),1,0)
data$majorade[is.na(data$majoradep)] <- NA
data$majorade <- factor(data$majorade, levels=c("1", "0"))
data$pulmonary_embolism_30day = factor(data$pulmonary_embolism_30day,levels=c("1","0"))
data$cva_30days = factor(data$cva,levels=c("1","0"))
data$mi_30day = factor(data$mi_30day,levels=c("1","0"))
data$surgical_site_infect_30day = factor(data$surgical_site_infect_30day,levels=c("1","0"))
data$hematoma = factor(data$hematoma,levels=c("1","0"))
data$new_neuro_deficit = factor(data$new_neuro_deficit,levels=c("1","0"))

data$uti_30days <- as.numeric(as.character(data$uti_30days))
data$dvt_30day <- as.numeric(as.character(data$dvt_30day))
data$incidental_durotomy <- as.numeric(as.character(data$incidental_durotomy))
data$pneumonia <- as.numeric(as.character(data$pneumonia))
data$minoradep <- rowSums(data[c("uti_30days", "dvt_30day", "pneumonia", "incidental_durotomy")], na.rm=TRUE)
data$minorade <- ifelse(data$minoradep%in%c(1:4),1,0)
data$minorade[is.na(data$minoradep)] <- NA
data$minorade <- factor(data$minorade, levels=c("1", "0"))
data$uti_30days = factor(data$uti_30days,levels=c("1","0"))
data$dvt_30day = factor(data$dvt_30day,levels=c("1","0"))
data$incidental_durotomy = factor(data$incidental_durotomy,levels=c("1","0"))
data$pneumonia = factor(data$pneumonia,levels=c("1","0"))

data$pt_satisfaction_index2.3m <- ifelse(data$pt_satisfaction_index.3m%in%c(1,2), 1,2)
data$pt_satisfaction_index2.3m[is.na(data$pt_satisfaction_index.3m)] <- NA
data$pt_satisfaction_index2.3m <- factor(data$pt_satisfaction_index2.3m, levels=c("1", "2"))
data$pt_satisfaction_index.3m = factor(data$pt_satisfaction_index.3m,levels=c("1","2","3","4"))
data$pt_satisfaction_index2.12m <- ifelse(data$pt_satisfaction_index.12m%in%c(1,2), 1,2)
data$pt_satisfaction_index2.12m[is.na(data$pt_satisfaction_index.12m)] <- NA
data$pt_satisfaction_index2.12m <- factor(data$pt_satisfaction_index2.12m, levels=c("1", "2"))
data$pt_satisfaction_index.12m = factor(data$pt_satisfaction_index.12m,levels=c("1","2","3","4"))
data$los3 <- ifelse(!is.na(data$los), data$los, data$los_calculated)
data$los3[data$los3<0 | data$los3>90] <- NA
data$estimated_blood_loss_cc3 <- as.numeric(sub(".*[-]+", "", gsub("[^0-9-]*", "", data$estimated_blood_loss_cc)))
data$estimated_blood_loss_cc3[data$estimated_blood_loss_cc3>10000] <- NA
data$returned_to_or_with_30_day = factor(data$returned_to_or_with_30_day,levels=c("1","0"))

data$date_surgery <- as.Date(data$date_surgery_performed, format='%Y-%m-%d')
data$date_readmission_3mths <- as.Date(data$date_readmission_3mths, format='%Y-%m-%d')
data$date_readmission_3mo.3m <- as.Date(data$date_readmission_3mo.3m, format='%Y-%m-%d')
data$days1 <- as.numeric(data$date_readmission_3mths - data$date_surgery)
data$days2 <- as.numeric(data$date_readmission_3mo.3m - data$date_surgery)
data$days.readmit <- ifelse(is.na(data$days1), data$days2, data$days1)
data$readmit30day <- data$re_admitted_within_30_days
data$readmit30day[!is.na(data$days.readmit)] <- 0
data$readmit30day[data$days.readmit<31] <- 1
data$readmit30day <- factor(data$readmit30day, levels=0:1)
data$readmit3m <- ifelse(data$readmit_3months %in% 1 | data$readmit_3mth_surg.3m%in%c(1) | data$readmit_3mth_surg.12m%in%c(1), 1, 0)
data$readmit3m <- factor(data$readmit3m, levels=c("1", "0"))

data$return_to_activities.3m = factor(data$return_to_activities.3m,levels=c("1","0"))
data$return_to_work.3m[!data$plan_return_work.3m %in% 1] <- NA
data$return_to_work.3m <- factor(data$return_to_work.3m, levels=c('1','0','2'))
data$return_to_activities.12m = factor(data$return_to_activities.12m,levels=c("1","0"))
data$return_to_work.12m[!data$plan_return_work.12m %in% 1] <- NA
data$return_to_work.12m <- factor(data$return_to_work.12m, levels=c('1','0','2'))

data$mort30d <- ifelse(data$place_discharged_to %in% c('6') | data$patient_died_within_30_day %in% c('1'), 1, 0)
data$mort30d[is.na(data$place_discharged_to) & is.na(data$patient_died_within_30_day)] <- NA
data$mort30d <- factor(data$mort30d, level=0:1)
data$mort3m <- ifelse(data$mort30d %in% c('1') | data$patient_interview_status.3m %in% 9, 1, 0)
data$mort3m[is.na(data$mort30d) & is.na(data$patient_interview_status.3m)] <- NA
data$mort3m <- factor(data$mort3m, level=0:1)
data$mort12m <- ifelse(data$patient_interview_status.12m %in% 9 | data$mort3m %in% '1', 1, 0)
data$mort12m[is.na(data$patient_interview_status.12m) & is.na(data$mort3m)] <- NA
data$mort12m <- factor(data$mort12m, level=0:1)

data$revision_surg_3mths2 <- ifelse(data$revision_surg_3mths.3m %in% 1 | data$revision_surg_3mths.12m %in% 1, 1, 0)
data$revision_surg_3mths2[is.na(data$revision_surg_3mths.3m) & is.na(data$revision_surg_3mths.12m)] <- NA
data$revision_surg_3mths2 <- factor(data$revision_surg_3mths2, levels=c("1", "0"))

# ODI score #
data$odi_score_pain_intensity_int <- ifelse(is.na(data$odi_score_pain_intensity_int), data$odi_pain_intensity_self, data$odi_score_pain_intensity_int)
data$odi_score_personal_care_int <- ifelse(is.na(data$odi_score_personal_care_int), data$odi_personal_care_self, data$odi_score_personal_care_int)
data$odi_score_lifting_int <- ifelse(is.na(data$odi_score_lifting_int), data$odi_lifting_self, data$odi_score_lifting_int)
data$odi_score_walking_int <- ifelse(is.na(data$odi_score_walking_int), data$odi_walking_self, data$odi_score_walking_int)
data$odi_score_sitting_int <- ifelse(is.na(data$odi_score_sitting_int), data$odi_sitting_self, data$odi_score_sitting_int)
data$odi_score_standing_int <- ifelse(is.na(data$odi_score_standing_int), data$odi_standing_self, data$odi_score_standing_int)
data$odi_score_sleeping_int <- ifelse(is.na(data$odi_score_sleeping_int), data$odi_sleeping_self, data$odi_score_sleeping_int)
data$odi_score_sex_life_int <- ifelse(is.na(data$odi_score_sex_life_int), data$odi_sex_life_self, data$odi_score_sex_life_int)
data$odi_score_social_life_int <- ifelse(is.na(data$odi_score_social_life_int), data$odi_social_life_self, data$odi_score_social_life_int)
data$odi_score_travelling_int <- ifelse(is.na(data$odi_score_travelling_int), data$odi_travelling_self, data$odi_score_travelling_int)

data$odi_score_pain_intensity_int.3m <- ifelse(is.na(data$odi_score_pain_intensity_int.3m), data$odi_pain_intensity_self.3m, data$odi_score_pain_intensity_int.3m)
data$odi_score_personal_care_int.3m <-  ifelse(is.na(data$odi_score_personal_care_int.3m), data$odi_personal_care_self.3m, data$odi_score_personal_care_int.3m)
data$odi_score_lifting_int.3m <-  ifelse(is.na(data$odi_score_lifting_int.3m), data$odi_lifting_self.3m, data$odi_score_lifting_int.3m)
data$odi_score_walking_int.3m <-  ifelse(is.na(data$odi_score_walking_int.3m), data$odi_walking_self.3m, data$odi_score_walking_int.3m)
data$odi_score_sitting_int.3m <-  ifelse(is.na(data$odi_score_sitting_int.3m), data$odi_sitting_self.3m, data$odi_score_sitting_int.3m)
data$odi_score_standing_int.3m <-  ifelse(is.na(data$odi_score_standing_int.3m), data$odi_standing_self.3m, data$odi_score_standing_int.3m)
data$odi_score_sleeping_int.3m <-  ifelse(is.na(data$odi_score_sleeping_int.3m), data$odi_sleeping_self.3m, data$odi_score_sleeping_int.3m)
data$odi_score_sex_life_int.3m <-  ifelse(is.na(data$odi_score_sex_life_int.3m), data$odi_sex_life_self.3m, data$odi_score_sex_life_int.3m)
data$odi_score_social_life_int.3m <-  ifelse(is.na(data$odi_score_social_life_int.3m), data$odi_social_life_self.3m, data$odi_score_social_life_int.3m)
data$odi_score_travelling_int.3m <- ifelse(is.na(data$odi_score_travelling_int.3m), data$odi_travelling_self.3m, data$odi_score_travelling_int.3m)

data$odi_score_pain_intensity_int.12m <- ifelse(is.na(data$odi_score_pain_intensity_int.12m), data$odi_pain_intensity_self.12m, data$odi_score_pain_intensity_int.12m)
data$odi_score_personal_care_int.12m <-  ifelse(is.na(data$odi_score_personal_care_int.12m), data$odi_personal_care_self.12m, data$odi_score_personal_care_int.12m)
data$odi_score_lifting_int.12m <-  ifelse(is.na(data$odi_score_lifting_int.12m), data$odi_lifting_self.12m, data$odi_score_lifting_int.12m)
data$odi_score_walking_int.12m <-  ifelse(is.na(data$odi_score_walking_int.12m), data$odi_walking_self.12m, data$odi_score_walking_int.12m)
data$odi_score_sitting_int.12m <-  ifelse(is.na(data$odi_score_sitting_int.12m), data$odi_sitting_self.12m, data$odi_score_sitting_int.12m)
data$odi_score_standing_int.12m <-  ifelse(is.na(data$odi_score_standing_int.12m), data$odi_standing_self.12m, data$odi_score_standing_int.12m)
data$odi_score_sleeping_int.12m <-  ifelse(is.na(data$odi_score_sleeping_int.12m), data$odi_sleeping_self.12m, data$odi_score_sleeping_int.12m)
data$odi_score_sex_life_int.12m <-  ifelse(is.na(data$odi_score_sex_life_int.12m), data$odi_sex_life_self.12m, data$odi_score_sex_life_int.12m)
data$odi_score_social_life_int.12m <-  ifelse(is.na(data$odi_score_social_life_int.12m), data$odi_social_life_self.12m, data$odi_score_social_life_int.12m)
data$odi_score_travelling_int.12m <- ifelse(is.na(data$odi_score_travelling_int.12m), data$odi_travelling_self.12m, data$odi_score_travelling_int.12m)

data$odiscore <- apply(data[,c('odi_score_pain_intensity_int', 'odi_score_personal_care_int', 'odi_score_lifting_int', 'odi_score_walking_int', 'odi_score_sitting_int', 'odi_score_standing_int', 'odi_score_sleeping_int', 'odi_score_sex_life_int', 'odi_score_social_life_int', 'odi_score_travelling_int')], MARGIN=1, FUN=function(x) {
  mean(x/c(rep(5,7),5,5,5), na.rm=TRUE)
})*100
data$odi_ind <- apply(data[,c('odi_score_pain_intensity_int', 'odi_score_personal_care_int', 'odi_score_lifting_int', 'odi_score_walking_int', 'odi_score_sitting_int', 'odi_score_standing_int', 'odi_score_sleeping_int', 'odi_score_sex_life_int', 'odi_score_social_life_int', 'odi_score_travelling_int')], MARGIN=1, FUN=function(x) {
  sum(is.na(x))
})
data$odiscore[data$odi_ind > 5] <- NA

data$odiscore.3m <- apply(data[,c('odi_score_pain_intensity_int.3m', 'odi_score_personal_care_int.3m', 'odi_score_lifting_int.3m', 'odi_score_walking_int.3m', 'odi_score_sitting_int.3m', 'odi_score_standing_int.3m', 'odi_score_sleeping_int.3m', 'odi_score_sex_life_int.3m', 'odi_score_social_life_int.3m', 'odi_score_travelling_int.3m')], MARGIN=1, FUN=function(x) {
  mean(x/c(rep(5,7),5,5,5), na.rm=TRUE)
})*100
data$odi_ind.3m <- apply(data[,c('odi_score_pain_intensity_int.3m', 'odi_score_personal_care_int.3m', 'odi_score_lifting_int.3m', 'odi_score_walking_int.3m', 'odi_score_sitting_int.3m', 'odi_score_standing_int.3m', 'odi_score_sleeping_int.3m', 'odi_score_sex_life_int.3m', 'odi_score_social_life_int.3m', 'odi_score_travelling_int.3m')], MARGIN=1, FUN=function(x) {
  sum(is.na(x))
})
data$odiscore.3m[data$odi_ind.3m > 5] <- NA


data$odiscore.12m <- apply(data[,c('odi_score_pain_intensity_int.12m', 'odi_score_personal_care_int.12m', 'odi_score_lifting_int.12m', 'odi_score_walking_int.12m', 'odi_score_sitting_int.12m', 'odi_score_standing_int.12m', 'odi_score_sleeping_int.12m', 'odi_score_sex_life_int.12m', 'odi_score_social_life_int.12m', 'odi_score_travelling_int.12m')], MARGIN=1, FUN=function(x) {
  mean(x/c(rep(5,7),5,5,5), na.rm=TRUE)
})*100
data$odi_ind.12m <- apply(data[,c('odi_score_pain_intensity_int.12m', 'odi_score_personal_care_int.12m', 'odi_score_lifting_int.12m', 'odi_score_walking_int.12m', 'odi_score_sitting_int.12m', 'odi_score_standing_int.12m', 'odi_score_sleeping_int.12m', 'odi_score_sex_life_int.12m', 'odi_score_social_life_int.12m', 'odi_score_travelling_int.12m')], MARGIN=1, FUN=function(x) {
  sum(is.na(x))
})
data$odiscore.12m[data$odi_ind.12m > 5] <- NA

# EQ5D score #
data$mobility_int <- ifelse(is.na(data$mobility_int), data$mobility_self_admin, data$mobility_int)
data$self_care_int <- ifelse(is.na(data$self_care_int), data$self_care_self_admin, data$self_care_int)
data$usual_activities_int <- ifelse(is.na(data$usual_activities_int), data$usual_activities_self_admin, data$usual_activities_int)
data$pain_discomfort_int <- ifelse(is.na(data$pain_discomfort_int), data$pain_self_admin_discomfort, data$pain_discomfort_int)
data$anxiety_depression_int <- ifelse(is.na(data$anxiety_depression_int), data$anxiety_self_admin_depression, data$anxiety_depression_int)

data$mobility_int.3m <- ifelse(is.na(data$mobility_int.3m), data$mobility_self_admin.3m, data$mobility_int.3m)
data$self_care_int.3m <- ifelse(is.na(data$self_care_int.3m), data$self_care_self_admin.3m, data$self_care_int.3m)
data$usual_activities_int.3m <- ifelse(is.na(data$usual_activities_int.3m), data$usual_activities_self_admin.3m, data$usual_activities_int.3m)
data$pain_discomfort_int.3m <- ifelse(is.na(data$pain_discomfort_int.3m), data$pain_self_admin_discomfort.3m, data$pain_discomfort_int.3m)
data$anxiety_depression_int.3m <- ifelse(is.na(data$anxiety_depression_int.3m), data$anxiety_self_admin_depression.3m, data$anxiety_depression_int.3m)


data$mobility_int.12m <- ifelse(is.na(data$mobility_int.12m), data$mobility_self_admin.12m, data$mobility_int.12m)
data$self_care_int.12m <- ifelse(is.na(data$self_care_int.12m), data$self_care_self_admin.12m, data$self_care_int.12m)
data$usual_activities_int.12m <- ifelse(is.na(data$usual_activities_int.12m), data$usual_activities_self_admin.12m, data$usual_activities_int.12m)
data$pain_discomfort_int.12m <- ifelse(is.na(data$pain_discomfort_int.12m), data$pain_self_admin_discomfort.12m, data$pain_discomfort_int.12m)
data$anxiety_depression_int.12m <- ifelse(is.na(data$anxiety_depression_int.12m), data$anxiety_self_admin_depression.12m, data$anxiety_depression_int.12m)

eq <- read.csv('eq5dscore.csv')
eq$com <- paste(eq$MO, eq$SC, eq$UA, eq$PD, eq$AD, sep='')

data$com <- with(data, paste(mobility_int, self_care_int, usual_activities_int, pain_discomfort_int, anxiety_depression_int, sep=''))
data$com.3m <- with(data, paste(mobility_int.3m, self_care_int.3m, usual_activities_int.3m, pain_discomfort_int.3m, anxiety_depression_int.3m, sep=''))
data$com.12m <- with(data, paste(mobility_int.12m, self_care_int.12m, usual_activities_int.12m, pain_discomfort_int.12m, anxiety_depression_int.12m, sep=''))

data$eq5dscore <- eq$score[match(data$com, eq$com)]
data$eq5dscore.3m <- eq$score[match(data$com.3m, eq$com)]
data$eq5dscore.12m <- eq$score[match(data$com.12m, eq$com)]

data$revision <- ifelse(data$primary_revision %in% 2, 1, 0)

data$disability_reason1 <- data$disability_reason2 <- data$disability_reason
data$disability_reason1[data$employment %in% 3] <- NA
data$disability_reason2[data$employment %in% 2] <- NA

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

data$dominant_symptom1___1 <- ifelse(data$dominant_symptom1 %in% 1, 1, 0)
data$dominant_symptom1___2 <- ifelse(data$dominant_symptom1 %in% 2, 1, 0)
data$dominant_symptom1___3 <- ifelse(data$dominant_symptom1 %in% 3, 1, 0)

data$asa_grade___1 <- ifelse(data$asa_grade %in% 1, 1, 0)
data$asa_grade___2 <- ifelse(data$asa_grade %in% 2, 1, 0)
data$asa_grade___3 <- ifelse(data$asa_grade %in% 3, 1, 0)
data$asa_grade___4 <- ifelse(data$asa_grade %in% 4, 1, 0)

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


#######################################################################################

## get the start date of each center #
startdate <- aggregate(data$date_surgery, by=list(data$practice), FUN=min, na.rm=TRUE)
colnames(startdate) <- c('practice', 'sdate')
startdate$sdate <- as.character(startdate$sdate)


#### functions of generating descriptive statistics ####

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

### generate table template ###

## table of baseline characteristics ##
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
  hl <- c(1:45)
  dv <- rep(0, times=45)
  dv[1] <- sfun2(data=data, vr='pgender', ip=ipr)
  dv[4] <- sfun1(data=data, vr='ptage2', ip=ipr)
  dv[6] <- sfun2(data=data, vr='prace___1', ip=ipr)
  dv[7] <- sfun2(data=data, vr='prace___2', ip=ipr)
  dv[8] <- sfun2(data=data, vr='prace___3', ip=ipr)
  dv[9] <- sfun2(data=data, vr='prace___4', ip=ipr)
  dv[10] <- sfun2(data=data, vr='prace___5', ip=ipr)
  dv[11] <- sfun2(data=data, vr='prace___6', ip=ipr)
  dv[12] <- sfun2(data=data, vr='ptethnicity', ip=ipr)
  dv[14] <- sfun2(data=data, vr='pt_education_level___1', ip=ipr)
  dv[15] <- sfun2(data=data, vr='pt_education_level___2', ip=ipr)
  dv[16] <- sfun2(data=data, vr='pt_education_level___3', ip=ipr)
  dv[17] <- sfun2(data=data, vr='pt_education_level___4', ip=ipr)
  dv[18] <- sfun2(data=data, vr='pt_education_level___5', ip=ipr)
  dv[20] <- sfun2(data=data, vr='insurance1___1', ip=ipr)
  dv[21] <- sfun2(data=data, vr='insurance1___2', ip=ipr)
  dv[22] <- sfun2(data=data, vr='insurance1___3', ip=ipr)
  dv[23] <- sfun2(data=data, vr='insurance1___4', ip=ipr)
  dv[24] <- sfun2(data=data, vr='insurance1___5', ip=ipr)
  dv[26] <- sfun2(data=data, vr='workers_comp', ip=ipr)
  dv[27] <- sfun2(data=data, vr='liability_claim1', ip=ipr)
  dv[28] <- sfun2(data=data, vr='employment', ip=ipr, lv=1)
  dv[29] <- sfun2(data=data, vr='full_part_time', ip=ipr, lv=1)
  dv[30] <- sfun2(data=data, vr='full_part_time', ip=ipr, lv=2)
  dv[31] <- sfun2(data=data, vr='employment', ip=ipr, lv=2)
  dv[32] <- sfun2(data=data, vr='disability_reason1', ip=ipr, lv=1)
  dv[33] <- sfun2(data=data, vr='disability_reason1', ip=ipr, lv=2)
  dv[34] <- sfun2(data=data, vr='employment', ip=ipr, lv=3)
  dv[35] <- sfun2(data=data, vr='unemployed', ip=ipr, lv=1)
  dv[36] <- sfun2(data=data, vr='disability_reason2', ip=ipr, lv=1)
  dv[37] <- sfun2(data=data, vr='disability_reason2', ip=ipr, lv=2)
  dv[38] <- sfun2(data=data, vr='unemployed', ip=ipr, lv=2)
  dv[39] <- sfun2(data=data, vr='unemployed', ip=ipr, lv=3)
  dv[40] <- sfun2(data=data, vr='unemployed', ip=ipr, lv=4)
  dv[41] <- sfun2(data=data, vr='employment', ip=ipr, lv=4)
  dv[42] <- sfun2(data=data, vr='plan_return_work', ip=ipr, lv=1)
  dv[44] <- sfun2(data=data, vr='activity_out_home', ip=ipr, lv=1)
  dv[45] <- sfun2(data=data, vr='activity_inside_home', ip=ipr, lv=1)
  dv2 <- ifelse(abs(dv)>=0.4, TRUE, FALSE)
  return(hl[dv2])
}

plotfun1 <- function(x, yli, yma, yla, mai, yll) {
  plot(1:10, 1:10, xlim=c(1.5,7), ylim=yli, type="n", axes=FALSE, xlab="Time after Surgery", ylab=yla, main=mai)
  axis(side=1, at=c(2,4,7), labels=c("Baseline", "3-month", "12-month"), col='white')
  axis(side=2, at=yma, las=2, labels=yll)
  
  if (x[1]>4) {
    points(c(2,4), x[c(2,4)], pch=19)
    arrows(2, x[2]-x[3], 2, x[2]+x[3], angle=90, code=3, length=0.05)
    arrows(4, x[4]-x[5], 4, x[4]+x[5], angle=90, code=3, length=0.05)
    segments(2, x[2], 4, x[4])
  }
}

plotfun1b <- function(dat, iv, xyli, xyma, xla, yla, idline, mai) {
  vars1 <- c("back_pain_vas", "leg_pain_vas1", "odiscore", "eq5dscore")
  vars2 <- paste(c("back_pain_vas", "leg_pain_vas1", "odiscore", "eq5dscore"), ".3m", sep="")
  tmp <- dat[!is.na(dat[[vars1[iv]]]) & !is.na(dat[[vars2[iv]]]),]
  x <- tmp[[vars1[[iv]]]]
  y <- tmp[[vars2[[iv]]]]
  if (nrow(tmp)>0) {
    plot(jitter(x,1), jitter(y,1), xlim=xyli, ylim=xyli, main=mai, axes=FALSE, xlab=xla, ylab=yla, type="p", pch=1, cex=1, col='grey30')
  } else {
    plot(jitter(x,1), jitter(y,1), xlim=xyli, ylim=xyli, main=mai, axes=FALSE, xlab=xla, ylab=yla, type="n", pch=1, cex=1, col='grey30')
  }
  axis(side=1, at=xyma)
  axis(side=2, at=xyma, las=2)
  segments(idline[1], idline[2], idline[3], idline[4], lty=3) 
}

plotfun1c <- function(dat, iv, xyli, xyma, xla, yla, idline, mai) {
  vars1 <- paste(c("back_pain_vas", "leg_pain_vas1", "odiscore", "eq5dscore"), ".3m", sep="")
  vars2 <- paste(c("back_pain_vas", "leg_pain_vas1", "odiscore", "eq5dscore"), ".12m", sep="")
  tmp <- dat[!is.na(dat[[vars1[iv]]]) & !is.na(dat[[vars2[iv]]]),]
  x <- tmp[[vars1[[iv]]]]
  y <- tmp[[vars2[[iv]]]]
  if (nrow(tmp)>0) {
    plot(jitter(x,1), jitter(y,1), xlim=xyli, ylim=xyli, main=mai, axes=FALSE, xlab=xla, ylab=yla, type="p", pch=1, cex=1, col='grey30')
  } else {
    plot(jitter(x,1), jitter(y,1), xlim=xyli, ylim=xyli, main=mai, axes=FALSE, xlab=xla, ylab=yla, type="n", pch=1, cex=1, col='grey30')
  }
  
  axis(side=1, at=xyma)
  axis(side=2, at=xyma, las=2)
  segments(idline[1], idline[2], idline[3], idline[4], lty=3) 
}

plotfun2 <- function(datas, pltnam, ptab) {
  pdf(pltnam, width=7, height=14.0625)
  par(mfrow=c(4,2), mar=c(7,10,6,1))
  plotfun1(x=ptab[1,], mai="NRS-Back Pain", yli=c(0,10), yma=c(0,2,4,6,8,10), yla="Pain Score", yll=c("  \n 0\n No Pain", 2,4,6,8, "  \n 10\n Worst Pain"))
  plotfun1b(dat=datas, iv=1, xyli=c(0,10), xyma=c(0,2,4,6,8,10), xla="Pre-surgery Pain Score", yla="3-month Pain Score", idline=c(0,0,10,10), mai="NRS-Back Pain")
  
  plotfun1(x=ptab[2,], mai="NRS-Leg Pain", yli=c(0,10), yma=c(0,2,4,6,8,10), yla="Pain Score", yll=c("  \n 0\n No Pain", 2,4,6,8, "  \n 10\n Worst Pain"))
  plotfun1b(dat=datas, iv=2, xyli=c(0,10), xyma=c(0,2,4,6,8,10), xla="Pre-surgery Pain Score", yla="3-month Pain Score", idline=c(0,0,10,10), mai="NRS-Leg Pain")
  
  
  plotfun1(x=ptab[3,], mai="Oswestry Disability Index", yli=c(0,100), yma=c(0,20,40,60,80,100), yla="ODI", yll=c("  \n 0\n No Impairment", 20,40,60,80, "  \n 100\n Functional Impairment"))
  plotfun1b(dat=datas, iv=3, xyli=c(0,100), xyma=c(0,20,40,60,80,100), xla="Pre-surgery ODI", yla="3-month ODI", idline=c(0,0,100,100), mai="Oswestry Disability Index")
  
  plotfun1(x=ptab[4,], mai="Euroqual Quality of Life", yli=c(-0.1, 1.0), yma=c(-0.1, 0, 0.2, 0.4, 0.6, 0.8, 1.0), yla="EQ-5D", yll=c("  \n -0.1\n Worst Health", 0,0.2,0.4,0.6,0.8, "  \n 1.0\n Best Health"))
  plotfun1b(dat=datas, iv=4, xyli=c(-0.1,1.0), xyma=c(-0.1, 0, 0.2, 0.4, 0.6, 0.8, 1.0), xla="Pre-surgery EQ-5D", yla="3-month EQ-5D", idline=c(-0.1,-0.1,1,1), mai="Euroqual Quality of Life")
  dev.off()
}

pracs <- pracs1

for (w in seq(length(pracs))) {
  data1 <- subset(data, practice==pracs[w])
  data2 <- subset(data1, analysis12month)
  tab <- table(data1$surgeon)
  if (sum(tab>19) > 0) {
    snam <- names(tab)[tab>19]
    nsg <- length(snam)
    for (k in seq(nsg)) {
      Sweave("lumbar_surgeon_report_091218", output=paste(pracs[w], '_', snam[k], ".tex", sep="")) 
    }
  }
}

for (w in seq(length(pracs))) {
  data1 <- subset(data, practice==pracs[w])
  data2 <- subset(data1, analysis12month)
  tab <- table(data1$surgeon)
  if (sum(tab>19) > 0) {
    snam <- names(tab)[tab>19]
    nsg <- length(snam)
    for (k in seq(nsg)) {
      for (q in 1:3) {system(paste("pdflatex ", paste(pracs[w], '_', snam[k], ".tex", sep="")))}
    }
  }
}


