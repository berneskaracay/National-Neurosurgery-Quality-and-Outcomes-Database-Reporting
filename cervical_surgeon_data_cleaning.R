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