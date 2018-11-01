dataname <- commandArgs(TRUE)

if(length(dataname) == 0 || length(dataname) > 1) {
  stop("no/too many args specified")
}

load(paste0(dataname, ".rda"))
library(Hmisc)

baseline <- dataEvents$baseline_arm_1
baseline$surgeon <- as.character(baseline$surgeon)
baseline$surg_location <- as.character(baseline$surg_location)

data_3month <- dataEvents$`3month_arm_1`
data_12month <- dataEvents$`12month_arm_1`

source(paste0(dataname, '.index.R'))

baseline_excludeA <- Reduce('|', Map(f=eval, excludeA,
                                     MoreArgs=list(envir=dataEvents)))
baseline_excludeA <- !is.na(baseline_excludeA) & baseline_excludeA


baseline_excludeA1 <- Reduce('|', Map(f=eval, excludeA1,
                                      MoreArgs=list(envir=dataEvents)))
baseline_excludeA1 <- !is.na(baseline_excludeA1) & baseline_excludeA1

dataEvents$redcap_event_name <- "3month_arm_1"
baseline_excludeB <- Reduce('|', Map(f=eval, excludeB,
                                      MoreArgs=list(envir=dataEvents)))
baseline_excludeB <- !is.na(baseline_excludeB) & baseline_excludeB

NotNA <- function(x) !is.na(x) & x
data_excludeL <- data.frame(L7 = NotNA(eval(excludeL7, envir=dataEvents)),
                            L8 = NotNA(eval(excludeL8, envir=dataEvents)),
                            L9 = NotNA(eval(excludeL9, envir=dataEvents)),
                            L10 = NotNA(eval(excludeL10, envir=dataEvents)))

dataEvents$redcap_event_name <- "3month_arm_1"
baseline_excludeC <- Reduce('|', Map(f=eval, excludeC,
                                      MoreArgs=list(envir=dataEvents)))
baseline_excludeC <- !is.na(baseline_excludeC) & baseline_excludeC

dataEvents$redcap_event_name <- "12month_arm_1"
baseline_excludeD <- NotNA(Reduce('|', Map(f=eval, excludeD,
                                      MoreArgs=list(envir=dataEvents))))

data_excludeL$L14 <- NotNA(eval(excludeL14, envir=dataEvents))

data_excludeL$L15 <- NotNA(eval(excludeL15, envir=dataEvents))

touched_baseline <- !is.na(baseline$date_review_baseint) | !is.na(baseline$date_verify_baseint)

interviewfail_baseline <- baseline_excludeA1

ndi_scorefail_baseline <- apply(baseline[,c("ndi_pain_intensity_int", "ndi_personal_care_int","ndi_lifting_int","ndi_reading_int","ndi_headaches_int","ndi_concentration_int","ndi_work_int","ndi_driving_int","ndi_sleeping_int","ndi_recreation_int")], 1, function(x) sum(is.na(x)) > 5) & apply(baseline[,c("ndi_pain_intensity_self", "ndi_personal_care_self","ndi_lifting_self","ndi_reading_self","ndi_headaches_self","ndi_concentration_self","ndi_work_self","ndi_driving_self","ndi_sleeping_self","ndi_recreation_self")], 1, function(x) sum(is.na(x)) > 5)

eq5d_scorefail_baseline <- apply(baseline[,c("mobility_int","self_care_int","usual_activities_int","pain_discomfort_int","anxiety_depression_int")], 1, FUN=function(x) any(is.na(x))) & apply(baseline[,c("mobility_self","self_care_self","usual_activities_self","pain_discomfort_self","anxiety_depression_self")], 1, FUN=function(x) any(is.na(x)))

scorefail_baseline <- ndi_scorefail_baseline | eq5d_scorefail_baseline


touched_3month <- !is.na(dataEvents$`3month_arm_1`$date_interview_review) | !is.na(dataEvents$`3month_arm_1`$date_interview_verify)

touched_12month <- !is.na(dataEvents$`12month_arm_1`$date_interview_review) | !is.na(dataEvents$`12month_arm_1`$date_interview_verify)

eligible_3month <- (!is.na(baseline$surgery_date) & as.Date(DataTimestamp) - baseline$surgery_date >= as.difftime(365/12*3 + 6*7, units='days')) | touched_3month

eligible_12month <- (!is.na(baseline$surgery_date) & as.Date(DataTimestamp) - baseline$surgery_date >= as.difftime(365 + 8*7, units='days')) | touched_12month
                          
ndi_scorefail_3month <- apply(data_3month[,c("ndi_pain_intensity_int", "ndi_personal_care_int","ndi_lifting_int","ndi_reading_int","ndi_headaches_int","ndi_concentration_int","ndi_work_int","ndi_driving_int","ndi_sleeping_int","ndi_recreation_int")], 1, function(x) sum(is.na(x)) > 5) & apply(data_3month[,c("ndi_pain_intensity_self", "ndi_personal_care_self","ndi_lifting_self","ndi_reading_self","ndi_headaches_self","ndi_concentration_self","ndi_work_self","ndi_driving_self","ndi_sleeping_self","ndi_recreation_self")], 1, function(x) sum(is.na(x)) > 5)

eq5d_scorefail_3month <- apply(data_3month[,c("mobility_int","self_care_int","usual_activities_int","pain_discomfort_int","anxiety_depression_int")], 1, FUN=function(x) any(is.na(x))) & apply(data_3month[,c("mobility_self","self_care_self","usual_activities_self","pain_discomfort_self","anxiety_depression_self")], 1, FUN=function(x) any(is.na(x)))

scorefail_3month <- ndi_scorefail_3month | eq5d_scorefail_3month
satisfactionfail_3month <- is.na(data_3month$pt_satisfaction_index)

interviewfail_3month <- as.integer(data_3month$patient_interview_status) %in% c(3,4,5,6,8,9,10,11,12,13,NA)
interviewfail_3month <- interviewfail_3month & !is.na(interviewfail_3month)

ndi_scorefail_12month <- apply(data_12month[,c("ndi_pain_intensity_int", "ndi_personal_care_int","ndi_lifting_int","ndi_reading_int","ndi_headaches_int","ndi_concentration_int","ndi_work_int","ndi_driving_int","ndi_sleeping_int","ndi_recreation_int")], 1, function(x) sum(is.na(x)) > 5) & apply(data_12month[,c("ndi_pain_intensity_self", "ndi_personal_care_self","ndi_lifting_self","ndi_reading_self","ndi_headaches_self","ndi_concentration_self","ndi_work_self","ndi_driving_self","ndi_sleeping_self","ndi_recreation_self")], 1, function(x) sum(is.na(x)) > 5)

eq5d_scorefail_12month <- apply(data_12month[,c("mobility_int","self_care_int","usual_activities_int","pain_discomfort_int","anxiety_depression_int")], 1, FUN=function(x) any(is.na(x))) & apply(data_12month[,c("mobility_self","self_care_self","usual_activities_self","pain_discomfort_self","anxiety_depression_self")], 1, FUN=function(x) any(is.na(x)))

scorefail_12month <- ndi_scorefail_12month | eq5d_scorefail_12month
satisfactionfail_12month <- is.na(data_12month$pt_satisfaction_index)

interviewfail_12month <- as.integer(data_12month$patient_interview_status) %in% c(3,4,5,6,8,9,10,11,12,13,NA)
interviewfail_12month <- interviewfail_12month & !is.na(interviewfail_12month)

data_fail <- data.frame(interviewfail_baseline,
                        scorefail_baseline,
                        ndi_scorefail_baseline,
                        eq5d_scorefail_baseline,
                        interviewfail_3month,
                        scorefail_3month,
                        ndi_scorefail_3month,
                        eq5d_scorefail_3month,
                        satisfactionfail_3month,
                        interviewfail_12month,
                        scorefail_12month,
                        satisfactionfail_12month,
                        ndi_scorefail_12month,
                        eq5d_scorefail_12month)


dat_select <- !is.na(baseline$surgery_date) & touched_baseline
dat_baseline <- baseline[dat_select,]
dat_3month <- data_3month[dat_select,]
dat_12month <- data_12month[dat_select,]
dat_excludeA <- baseline_excludeA[dat_select]
dat_excludeA1 <- baseline_excludeA1[dat_select]
dat_excludeB <- baseline_excludeB[dat_select]
dat_excludeC <- baseline_excludeC[dat_select]
dat_excludeD <- baseline_excludeD[dat_select]
dat_excludeL <- data_excludeL[dat_select,]
dat_fail <- data_fail[dat_select,]
dat_touched_baseline <- touched_baseline[dat_select]
dat_touched_3month <- touched_3month[dat_select]
dat_touched_12month <- touched_12month[dat_select]
dat_eligible_3month <- eligible_3month[dat_select]
dat_eligible_12month <- eligible_12month[dat_select]

    
excludeEnrollment <- dat_excludeA | dat_excludeA1
excludeBaseline <- excludeEnrollment | dat_excludeB
exclude3month <- excludeBaseline | dat_excludeC
exclude12month <- excludeBaseline | dat_excludeD
    
followedEnrollment <- !excludeEnrollment
followedBaseline <- !excludeBaseline & dat_touched_baseline
followed3month <- !exclude3month & dat_touched_3month
followed12month <- !exclude12month & dat_touched_12month


## subjects for which enough time has passed so that followup can be collected
available3month <- dat_eligible_3month | dat_touched_3month
available12month <- dat_eligible_12month | dat_touched_12month

excludeAnalysisBaseline <- dat_fail$interviewfail_baseline | dat_fail$scorefail_baseline
excludeAnalysis3month <- excludeAnalysisBaseline | dat_fail$interviewfail_3month | dat_fail$scorefail_3month
excludeAnalysis12month <- excludeAnalysisBaseline | dat_fail$interviewfail_12month | dat_fail$scorefail_12month

usefull3month <- available3month & !exclude3month
usefull12month <- available12month & !exclude12month

analysis3month <- followed3month & !excludeAnalysis3month
analysis12month <- followed12month & !excludeAnalysis12month


## Various Death points
deadhospital <- dat_3month$place_discharged_to == "Died in hospital"
dead30day <- dat_3month$patient_died_within_30_day == "Yes"
dead3month <- dat_3month$patient_interview_status == "Deceased"
dead12month <- dat_12month$patient_interview_status == "Deceased"

d <- data.frame(pt_study_id=dat_baseline$pt_study_id, usefull3month, usefull12month,
                deadhospital, dead30day, dead3month, dead12month, analysis3month,
                analysis12month, dat_fail)

save(d, DataTimestamp, file=paste0(dataname, "_index.rda"))
