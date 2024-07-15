# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT data cleaning ---------------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# .This script combines datasets on 11 diseases from first three cohorts of the Rotterdam Study. The final dataset also includes vital status, sex and date of birth of all participants from cohort 1 through 3. The final dataset is called "shift_data.Rdata" and is saved in the folder "cleaned" which is a subfolder of the working directory.

# For all questions about it, contact Premysl Velek at p.velek@erasmusmc.nl 

# Last update: 22 April 2024!! Stroke and TIA data updated. Edited by P Velek

# .-------------------------------------------------------------------------------
# ------------------------ Preliminaries ----------------------------------------
# .--------------------------------------------------------------------------------

# libraries needed in this script
packages_needed <- c("foreign", "tidyverse", "lubridate", "here")

# install missing libraries
install.packages(setdiff(packages_needed, rownames(installed.packages())))  

# load libraries
require(foreign)
require(tidyverse)
require(lubridate)

# start here
setwd(here::here())


# .----------------------------------------------------------------------
# ------------- Format basics ------------------------------------------
# .----------------------------------------------------------------------
basic_p <- here::here("Datasets", "ERGO-basic", "RoterdamStudy_Basics2014.sav")
basic <- read.spss(basic_p, to.data.frame = TRUE)

# convert dates
basic[, c(4, 8)] <- lapply(basic[, c(4, 8)] / 86400, 
                           as.Date, origin = "1582-10-14")

basic <- basic %>%
  rename(birthd = date_of_birth)

# .----------------------------------------------------------------------
# ------------- Vital status -------------------------------------------
# .----------------------------------------------------------------------

vs_p <- here::here("Datasets", "ERGO-basic", "fp_VitalStatus_(24-MAY-2018).sav")
vs <- read.spss(vs_p,  to.data.frame = TRUE)

vs[, c(5, 6, 9, 10, 11, 12)] <- lapply(vs[, c(5, 6, 9, 10, 11, 12)] / 86400, 
                                       as.Date, origin = "1582-10-14")

names(vs)[10] <- "mortd"
# .----------------------------------------------------------------------
# ------------- ERGO centre visit dates --------------------------------
# .----------------------------------------------------------------------

# ERGO I 1
ergo_i_p <- here::here("Datasets", "ERGO-basic", "Ergo1ResponseDetail_(22-jan-2015)_excerpt.sav")
ergo_i <- read.spss(ergo_i_p, to.data.frame = TRUE)

# convert dates
ergo_i[, c(7, 9, 11)] <- lapply(ergo_i[, c(7, 9, 11)] / 86400, 
                           as.Date, origin = "1582-10-14")

ergo_i <- ergo_i[, -c(3:5)]


# ERGO I 2
ergo_i_2_p <- here::here("Datasets", "ERGO-basic", "Ergo2ResponseDetail_(22-jan-2015)_excerpt.sav")
ergo_i_2 <- read.spss(ergo_i_2_p, to.data.frame = TRUE)

# convert dates
ergo_i_2[, c(8, 10)] <- lapply(ergo_i_2[, c(8, 10)] / 86400, 
                                as.Date, origin = "1582-10-14")

ergo_i_2 <- ergo_i_2[, c(1, 2, 8, 10)]
names(ergo_i_2) <- c("ergoid", "rs_cohort", "visit21_d", "int2_d")

# Since for ERGO-I-2, participants filled in a questionnaire at home, rather than doing
# home interview. 
# The forms were handed in at the research center, so there is not an exact date of the 
# interview / questionnaire. As an approximate date, we take the date of the centre visit

ergo_i_2$int2_d <- ergo_i_2$visit21_d


# ERGO I 3
ergo_i_3_p <- here::here("Datasets", "ERGO-basic", "e3_(3)_RESPONS_(22-feb-2016)_excerpt.sav")
ergo_i_3 <- read.spss(ergo_i_3_p, to.data.frame = TRUE)

# convert dates
ergo_i_3[, c(7, 9, 12)] <- lapply(ergo_i_3[, c(7, 9, 12)] / 86400, 
                                as.Date, origin = "1582-10-14")

ergo_i_3 <- ergo_i_3[, c(1, 2, 7, 9, 12)]
names(ergo_i_3) <- c("ergoid", "rs_cohort", "int3_d", "visit31_d", "visit32_d")

# ERGO I 4 (including ERGO-II-2)
ergo_i_4_p <- here::here("Datasets", "ERGO-basic", "e4_(4)_RESPONS_(12-mar-2018)_excerpt.sav")
ergo_i_4 <- read.spss(ergo_i_4_p, to.data.frame = TRUE)

# convert dates
ergo_i_4[, c(7, 9, 11)] <- lapply(ergo_i_4[, c(7, 9, 11)] / 86400, 
                                  as.Date, origin = "1582-10-14")

ergo_i_4 <- ergo_i_4[, c(1, 2, 7, 9, 11)]
names(ergo_i_4) <- c("ergoid", "rs_cohort", "int4_d", "visit41_d", "visit42_d")

# Since this comprises two cohorts, it will be treated separately from the rest of ERGO-I visit dates.
# It will be left-joined directly to the smoking dataset

# ERGO II (ERGOPLUS)
ergo_ii_p <- here::here("Datasets", "ERGO-basic", "ep_(1)_RESPONS_(15-jan-2019)_excerpt.sav")
ergo_ii <- read.spss(ergo_ii_p, to.data.frame = TRUE)

# convert dates
ergo_ii[, c(7, 9, 11)] <- lapply(ergo_ii[, c(7, 9, 11)] / 86400, 
                                as.Date, origin = "1582-10-14")

ergo_ii <- ergo_ii[, -c(3:6, 8, 10)]

# ERGO III (ERGOJONG)
ergo_iii_p <- here::here("Datasets", "ERGO-basic", "ej_(1)_RESPONS_(04-apr-2016)_excerpt.sav")
ergo_iii <- read.spss(ergo_iii_p, to.data.frame = TRUE)

# convert dates
ergo_iii[, c(7, 9, 11)] <- lapply(ergo_iii[, c(7, 9, 11)] / 86400, 
                                 as.Date, origin = "1582-10-14")


ergo_iii <- ergo_iii[, -c(3:6, 8, 10)]

# rename variables so it's the same for all three cohorts
# suffix "a": whether or not an interview or visit was done
# suffix "d": date of interview or a visit
names <- c("ergoid", "rs_cohort", "int_d", 
           "visit1_d", "visit2_d")


names(ergo_ii) <- names(ergo_iii) <- names

ergo_i_visits <- list(ergo_i, 
                      ergo_i_2 %>% select(-rs_cohort),
                      ergo_i_3 %>% select(-rs_cohort)) %>%
  reduce(left_join, by = "ergoid") %>%
  rename(int1_d = e1_aintdat,
         visit11_d = e1_acen1dat,
         visit12_d = e1_acen2dat) %>%
  mutate(int1_d = if_else(e1_aint != "interview done", as.Date(NA), int1_d)) %>%
  select(-c(e1_aint, e1_acen1, e1_acen2))


start <- bind_rows(ergo_i_visits, ergo_ii, ergo_iii)


# add censor date for all cohorts
start <- start %>%
  left_join(select(vs, ergoid, fp_startdate, fp_censordate), by = "ergoid")

# get the latest start date 
# max(start$fp_startdate)

# 21 November 2008


# .----------------------------------------------------------------------
# ------------- Coronary Heart disease ---------------------------------
# -.---------------------------------------------------------------------

# load the chd data
chd_p <- here::here("Datasets", "CVD", "Coronary-heart-disease-updated-2018-11-06.sav")
chd <- read.spss(chd_p, to.data.frame = TRUE)

# convert dates
chd[[4]] <- as.Date(chd[[4]] / 86400, origin = "1582-10-14")

# rename informed consent column to keep track of consent per conditions
names(chd)[6] <- "ic_ok_chd"
names(chd)[3] <- "prev_chd"
names(chd)[4] <- "endd_chd"
names(chd)[5] <- "inc_chd"

# all data for participants without informed consent will be set to NA
chd$prev_chd[chd$prev_chd == "no informed consent for follow-up"] <- NA
chd$inc_chd[chd$inc_chd == "no informed consent for follow-up"] <- NA

# ids of no-consenting participants
no_ic <- chd$ergoid[chd$inc_chd == "no informed consent for follow-up"]


# all data for participants with no follow up will be set to NA
chd$prev_chd[chd$prev_chd == "no follow-up"] <- NA

# prevalence data for participants with incomplete data at baseline will be set to NA
chd$prev_chd[chd$prev_chd == "incomplete data at baseline RS-I"] <- NA


# rename levels
chd$prev_chd <- recode_factor(chd$prev_chd, 
                              "no CHD" = "no", 
                              "history of CHD (MI, PCI, CABG)" = "yes")

chd$inc_chd <- recode_factor(chd$inc_chd, 
                             "no CHD" = "no", 
                             "incident CHD (MI, PCI, CABG, or any coronary mortality)" = "yes",
                             "prevalent myocardial infarction, PCI, or CABG" = "prev")


# remove rs cohort columns
chd <- chd[, -2]

# histogram with distribution of end dates.
hist(chd$endd_chd[chd$inc_chd == "no"], "month")


# The natural end date is clearly 1 January 2015

# add start date - assuming it started on fp_startdate from the vital status data
chd <- chd %>%
  left_join(., vs %>% select(ergoid, fp_startdate), by = "ergoid") %>%
  rename(startd_chd = fp_startdate)

# .----------------------------------------------------------------------
# ------------- Heart failure -------------------------------------------
# .----------------------------------------------------------------------

# load data
hf_p <- here::here("Datasets", "CVD", "Heart-failure-updated-2018-06-22.sav")
hf <- read.spss(hf_p, to.data.frame = TRUE)

# convert dates
hf[, c(3, 6)] <- lapply(hf[, c(3, 6)] / 86400, 
                        as.Date, origin = "1582-10-14")


# rename informed consent column to keep track of consent per conditions
names(hf)[3] <- "startd_hf"
names(hf)[4] <- "prev_hf"
names(hf)[5] <- "inc_hf"
names(hf)[6] <- "endd_hf"

# add ic_ok_hf column
hf$ic_ok_hf <- "yes"
hf$ic_ok_hf[hf$inc_hf == "no informed consent for follow-up"] <- "no"


# all data for participants without informed consent will be set to NA
hf$prev_hf[hf$prev_hf == "no informed consent for follow-up"] <- NA
hf$inc_hf[hf$inc_hf == "no informed consent for follow-up"] <- NA

no_icc <- hf$ergoid[hf$inc_hf == "no informed consent for follow-up"]



# all data for participants with no follow up will be set to NA
hf$prev_hf[hf$prev_hf == "no follow-up"] <- NA
hf$inc_hf[hf$inc_hf == "no follow-up"] <- NA


# rename levels
hf$prev_hf <- recode_factor(hf$prev_hf, 
                            "no heart failure" = "no", 
                            "history of heart failure" = "yes")

hf$inc_hf <- recode_factor(hf$inc_hf, 
                           "no heart failure" = "no", 
                           "incident heart failure" = "yes",
                           "prevalent heart failure" = "prev")


# remove rs cohort column
hf <- hf[, -2]

# histogram with distribution of end dates.
hist(hf$endd_hf[hf$inc_hf == "no"], "month")


# .----------------------------------------------------------------------
# ------------- Cancer ---------------------------------
# .----------------------------------------------------------------------

can_p <- here::here("Datasets", "Cancer", "ONCOLOGY_prevalenceANDincidence_morbidityANDmortality_29.02.2020.sav")
can <- read.spss(can_p, to.data.frame = TRUE)

# convert dates to Dates
can[, c(4, 13, 15, 16)] <- lapply(can[, c(4, 13, 15, 16)] / 86400, 
                                  as.Date, origin = "1582-10-14")

# rename informed consent column to keep track of consent per conditions
names(can)[5] <- "ic_ok_can"

# keep only relevant columns
can <- can[, c(1, 5, 6, 9, 10, 13)]

# remove non melanoma skin cancers
can <-  can[!(can$fupicd10 %in% c("C44.B", "C44.P")), ]

# remove all possible cases. Only probable and certain cases are considered
can <- can[can$fupsure != "possible", ]

# drop unused levels
can$fupsure <- droplevels(can$fupsure)

# remove mortality data 
can <- can %>%
  filter(fupstat != "n.a. - dead") %>%
  droplevels()
  
# create variables for incidence and prevalence
can$prev_can[can$fupstat == "prevalent"] <- "yes" 
can$prev_can[can$fupstat == "incident"] <- "no" 

can$inc_can[can$fupstat == "incident"] <- "yes"
can$inc_can[can$fupstat == "prevalent"] <- "prev"

# data with only primary cancer (prevalent or incident). 
# There should be no duplicated ids
can_primary <- can %>%
  group_by(ergoid) %>%
  arrange(eventdat, .by_group = TRUE) %>%
  mutate(id = row_number()) %>%
  rename(eventd_can1 = eventdat,
         prev_can1 = prev_can,
         inc_can1 = inc_can) %>%
  filter(id == 1) %>%
  droplevels() %>%
  select(-c(id, fupstat, fupicd10, fupsure)) %>%
  left_join(select(start, ergoid, fp_censordate, fp_startdate), ., by = "ergoid") %>%
  rename(endd_can1 = fp_censordate,
         startd_can1 = fp_startdate)

# data with only secondary and further cancers (prevalent or incident). 
# the prevalent cases are those with SECOND OR MORE cancer case, eg. to be prevalent
# in this dataset, participants must have at least two prevalent cases of cancer. The 
# event date then record the date of the second prevalent case. The same applies for 
# incident cases - to be incident in this dataset, participants have to have SECOND or 
# MORE incident case. The event date than records the date of the second case.
# There should be no duplicated ids
can_secondary <- can %>%
  group_by(ergoid) %>%
  arrange(eventdat, .by_group = TRUE) %>%
  mutate(id = row_number()) %>%
  filter(id == 2) %>%
  rename(eventd_can2 = eventdat,
         prev_can2 = prev_can,
         inc_can2 = inc_can) %>%
  droplevels() %>%
  select(-c(id, fupstat, fupicd10, fupsure, ic_ok_can)) %>%
  left_join(select(start, ergoid, fp_censordate), ., by = "ergoid") %>%
  rename(endd_can2 = fp_censordate)
   

# we assume that 1) all cancer free participants give informed consent for cancer
# and 2) that the end date for cancer is the same as the end date in the vital status
# dataset (fp_censordate). 

with_cancer <- !(is.na(can_primary$eventd_can1))
can_primary$endd_can1[with_cancer] <- can_primary$eventd_can1[with_cancer]

with_cancer <- !(is.na(can_secondary$eventd_can2))
can_secondary$endd_can2[with_cancer] <- can_secondary$eventd_can2[with_cancer]

# add end date to secondary cancer dataset for those patients with primary cancer 
# (they were removed as a by product of the data manipulation that produced the 
# secondary cancer data)

with_one_cancer <- is.na(can_secondary$endd_can2)

can_secondary$endd_can2[with_one_cancer] <- start$fp_censordate[start$ergoid %in% can_secondary$ergoid[with_one_cancer]]


# now we can remove the event date, the end date in the dataset is either an event date for
# incidence or prevalend cases or censor date otherwise

can_primary <- can_primary %>% select(-eventd_can1)
can_secondary <- can_secondary %>% select(-eventd_can2)

# !!! Importantly, we assume that participants who are not included in the cancer
# dataset are cancer free, so we disregard the possibility that someone has cancer and 
# did not give informed consent for cancer data. !!!

can_primary$prev_can1[is.na(can_primary$prev_can1)] <- "no"
can_primary$inc_can1[is.na(can_primary$inc_can1)] <- "no"


can_secondary$prev_can2[is.na(can_secondary$prev_can2)] <- "no"
can_secondary$inc_can2[is.na(can_secondary$inc_can2)] <- "no"

can_primary$ic_ok_can <- "yes"


# .----------------------------------------------------------------------
# ------------- Dementia -----------------------------------------------
# .----------------------------------------------------------------------

dem_p <- here::here("Datasets", "Dementia", "Dementia_RSI-II-III_2021_01_21.sav")
dem <- read.spss(dem_p, to.data.frame = TRUE)


# convert dates to Dates
dem[, c(4, 6, 12, 14, 16)] <- lapply(dem[, c(4, 6, 12, 14, 16)] / 86400, 
                                     as.Date, origin = "1582-10-14")

# keep only relevant columns
dem <- dem[, c(1, 6, 7, 9, 10, 12, 14)]

# rename columns to make them consistent
dem <- dem %>%
  rename(prev_dem = dementia_prevalent,
         inc_dem = dementia_incident)

dem$ic_ok_dem <- "yes"
dem$ic_ok_dem[dem$prev_dem == "no dementia" &
              dem$dementia_at_risk == "no, not to be used for incident analysis (prevalent dementia, insufficient baseline screening, or no informed consent)"] <- "no"

dem$prev_dem[dem$prev_dem == "insufficient data to determine cognitive status"] <- NA

# rename factors to make them consistent
dem$prev_dem <- recode_factor(dem$prev_dem, 
                              "no dementia" = "no", 
                              "dementia (prevalent)" = "yes")


dem$inc_dem <- recode_factor(dem$inc_dem, 
                             "no dementia" = "no", 
                             "dementia (incident)" = "yes")

# add an extra factor level to incidence to indicate prevalent cases
levels(dem$inc_dem) <- c(levels(dem$inc_dem), "prev")
dem$inc_dem[dem$prev_dem == "yes"] <- "prev"


# rename columns for consistency
names(dem)[6] <- "eventd_dem"
names(dem)[7] <- "endd_dem"
names(dem)[2] <- "startd_dem"


# remove redundant columns
dem <- dem %>% select(-eventd_dem) %>%
               select(-dementia_at_risk)
  
# the end date for prevalent cases will be the same as the start date
test <- (dem$prev_dem == "yes") & !is.na(dem$prev_dem )

dem$endd_dem[test] <- dem$startd_dem[test]



# .----------------------------------------------------------------------
# ------------- Parkinsonism -------------------------------------------
# .----------------------------------------------------------------------

park_p <-  here::here("Datasets", "Parkinsonism", "fp_PARKINSON_(enddate 2018-01-01).sav")
park <- read.spss(park_p, to.data.frame = TRUE)

# convert dates to Dates
dates <- c(4, 14, 15, 17, 19, 20, 23, 25)
park[, dates] <- lapply(park[, dates] / 86400, as.Date, origin = "1582-10-14")

# rename columns
names(park)[5] <- "ic_ok_park"
names(park)[9] <- "prev_ps"
names(park)[10] <- "prev_pd"
names(park)[16] <- "inc_ps"
names(park)[18] <- "inc_pd"

names(park)[17] <- "endd_ps"
names(park)[19] <- "endd_pd"


# compare dementia data from the parkinsonism data with those from dementia dataset
park_dem <- park[, c(1, 23)]
dem_dem <- dem[, c(1, 5)]

dem_both <- left_join(park_dem, dem_dem, by = "ergoid")

dem_both %>%
  filter((!is.na(dementia_onset) & !is.na(endd_dem))) %>%
  filter(ymd(dementia_onset) != ymd(endd_dem))

# three participants have different event dates for dementia when compared the 
# parkinsonism and dementia datasets! ergoids: 553, 2770, 6224001

# Additionally, the dementia dataset seems to be more complete, as for some participants
# it record incident cases which are missing in the parkinsonism dataset (eg. ergoid 19).
# There is 229 such cases, there is only one case for which an incident case is recorded
# in the parkinsonism dataset, but not in the the dementia dataset (ergoid 2795001, it is # recorded as prevalent dementia in the dementia dataset)

# bottom line: we'll use the dementia dataset (rather than the parkinsonism dataset) for
# dementia


# keep only relevant columns
park <- park[, c(1, 5, 9, 10, 14:20, 24:26)]

# all incident cases of PD are also incident cases of Parkinsonism, but not all
# incident cases of Parkinsonism are also incident cases of PD. >> 
# >> PD is subset of Parkinsonism. We will therefore work with Parkinsonism as the
# condition to follow up on.

# To determine event date for each participant:

# if inc_PS & inc_PD > take the earlier date of the two (they are all equal, so this is 
# not strictly necessary)
# if inc_PS & NOT inc_PD > take the event date for PS
# if NOT inc_PS & inc_PD > such case doesn't exist

park <- park %>%
  mutate(eventd_park = case_when(
    (inc_ps == "incident parkinsonism" & inc_pd == "incident PD") 
    ~ pmin(onset_parkinsonism, endd_pd, na.rm = TRUE),
    (inc_ps == "incident parkinsonism" & inc_pd == "no incident PD") ~ endd_ps)
      )
  

park$prev_ps <- recode_factor(park$prev_ps, 
                              "no prevalent parkinsonism" = "no", 
                              "prevalent parkinsonism" = "yes")

park$prev_pd <- recode_factor(park$prev_pd, 
                              "no prevalent PD" = "no", 
                              "prevalent PD" = "yes")


park$inc_ps <- recode_factor(park$inc_ps, 
                             "no incident parkinsonism" = "no", 
                             "incident parkinsonism" = "yes")

park$inc_pd <- recode_factor(park$inc_pd, 
                             "no incident PD" = "no", 
                             "incident PD" = "yes")


park <- park %>%
  mutate(prev_park = case_when(
    (prev_ps == "yes" | prev_pd == "yes") ~ "yes",
    (prev_ps == "no" & prev_pd == "no") ~ "no"),
    inc_park = case_when(
      (inc_ps == "yes" | inc_pd == "yes") ~ "yes",
      (inc_ps == "no" & inc_pd == "no") ~ "no",
      prev_park == "yes" ~ "prev"))

# rename columns
names(park)[5] <- "startd_park"
names(park)[13] <- "endd_park"


# remove redundant columns
park <- park[, -c(3:4, 6:12, 14)]

# edit the endd column so that it records the event date for incident or prevalent cases
park$endd_park[!is.na(park$eventd_park)] <- park$eventd_park[!is.na(park$eventd_park)]

# remove the event date column
park <- park %>% select(-eventd_park)


# The start date is missing for prevalent cases even though they gave informed consent. 
# To make our data complete, I imputed the startd date, using the fp_startdate. Even though 
# this may not be the correct start date, it doesn't matter here as these are all prevalent cases
# and won't impact the incidence analysis

# park <- left_join(park, vs %>% select(fp_startdate, ergoid), by = "ergoid")



# .------------------------------------------------------------------------------------------------
# ------------- Depression -------------------------------------------------------------------------
# . ------------------------------------------------------------------------------------------------

# We consider also depressive symptoms as depression !

# . RS III -------------------------------------------------- 

dep_rs3_p <- here::here("Datasets", "Depression", "Incident_depression_RSIII_(05.08.2021).sav")
dep_rs3 <- read.spss(dep_rs3_p, to.data.frame = TRUE)

# convert dates to Dates
dep_rs3[, c(3, 4, 6, 8, 10, 12, 14, 16)] <- lapply(dep_rs3[, c(3, 4, 6, 8, 10, 12, 14, 16)] / 86400, 
                                                  as.Date, origin = "1582-10-14")

names(dep_rs3)[3] <- "startd_dep"
names(dep_rs3)[4] <- "endd_dep"


# depression first case
dep_rs3_primary <- dep_rs3[, c(1:6)]

# count the number of bipolar cases and extract the ids of those participants
bp_iii <- dep_rs3_primary$ergoid[dep_rs3_primary$CLEANtotalevent2yr1 == "bipolar"]


dep_rs3_primary <- dep_rs3_primary %>%
  mutate(inc_dep1 = case_when(
    CLEANtotalevent2yr1 != "no" ~ "yes",
    CLEANtotalevent2yr1 == "no" ~ "no")
  )

names(dep_rs3_primary)[6] <- "eventd_dep1"

# remove redundant columns
dep_rs3_primary <- dep_rs3_primary[, -c(2, 5)]

names(dep_rs3_primary)[2:3] <- c("startd_dep1", "endd_dep1")


# if eventd_dep1 is the same as the start date, then this case is considered
# prevalent
dep_rs3_primary <- dep_rs3_primary %>%
  mutate(prev_dep1 = case_when(eventd_dep1 == startd_dep1 ~ "yes",
                               TRUE ~ "no"),
         inc_dep1 = ifelse(prev_dep1 == "yes", "prev", inc_dep1))


# repeated depression (2 and more incidences)
dep_rs3_secondary <- dep_rs3[, c(1:4, 7:8)]


dep_rs3_secondary <- dep_rs3_secondary %>%
  mutate(inc_dep2 = case_when(
    CLEANtotalevent2yr2 != "no" ~ "yes",
    is.na(CLEANtotalevent2yr2) ~ "no")
  )

# rename column
names(dep_rs3_secondary)[6] <- "eventd_dep2"

# remove redundant columns
dep_rs3_secondary <- dep_rs3_secondary[, -c(2, 5)]

names(dep_rs3_secondary)[2:3] <- c("startd_dep2", "endd_dep2")

# . RS II -------------------------------------------------- 

dep_rs2_p <-  here::here("Datasets", "Depression", "ShortenedCohort_IncidenceDepressionRSII(03-2015).sav")
dep_rs2 <- read.spss(dep_rs2_p, to.data.frame = TRUE)

# convert dates to Dates
dep_rs2[, c(2, 3, 5, 7, 9, 11, 13, 15)] <- lapply(dep_rs2[, c(2, 3, 5, 7, 9, 11, 13, 15)] / 86400, 
                                                  as.Date, origin = "1582-10-14")

names(dep_rs2)[2] <- "startd_dep"
names(dep_rs2)[3] <- "endd_dep"


# depression first case
dep_rs2_primary <- dep_rs2[, c(1:5)]

# count the number of bipolar cases and extract the ids of those participants
bp_ii <- dep_rs2_primary$ergoid[dep_rs2_primary$CLEANtotalevent2yr1 == "bipolar"]

dep_rs2_primary <- dep_rs2_primary %>%
  mutate(inc_dep1 = case_when(
    CLEANtotalevent2yr1 != "no" ~ "yes",
    CLEANtotalevent2yr1 == "no" ~ "no")
  )

names(dep_rs2_primary)[5] <- "eventd_dep1"

# remove redundant columns
dep_rs2_primary <- dep_rs2_primary[, -4]

names(dep_rs2_primary)[2:3] <- c("startd_dep1", "endd_dep1")


# if eventd_dep1 is the same as the start date, then this case is considered
# prevalent
dep_rs2_primary <- dep_rs2_primary %>%
  mutate(prev_dep1 = case_when(eventd_dep1 == startd_dep1 ~ "yes",
                               TRUE ~ "no"),
         inc_dep1 = ifelse(prev_dep1 == "yes", "prev", inc_dep1))


# repeated depression (2 and more incidences)
dep_rs2_secondary <- dep_rs2[, c(1:3, 6:7)]


dep_rs2_secondary <- dep_rs2_secondary %>%
  mutate(inc_dep2 = case_when(
    CLEANtotalevent2yr2 != "no" ~ "yes",
    is.na(CLEANtotalevent2yr2) ~ "no")
  )

# rename column
names(dep_rs2_secondary)[5] <- "eventd_dep2"

# remove redundant columns
dep_rs2_secondary <- dep_rs2_secondary[, -4]

names(dep_rs2_secondary)[2:3] <- c("startd_dep2", "endd_dep2")

# . RS I ----------------------

dep_rs1_p <-  here::here("Datasets", "Depression", "Shortened_Cohort_IncidenceDepressionRSI(11-2013).sav")
dep_rs1 <- read.spss(dep_rs1_p, to.data.frame = TRUE)


# convert dates to Dates
dep_rs1[, c(3, 4, 6, 8, 10, 12, 14)] <- 
  lapply(dep_rs1[, c(3, 4, 6, 8, 10, 12, 14)] / 86400, 
                                               as.Date, origin = "1582-10-14")


names(dep_rs1)[3] <- "startd_dep"
names(dep_rs1)[4] <- "endd_dep"


# depression first case
dep_rs1_primary <- dep_rs1[, c(1, 3:6)]

# count the number of bipolar cases and extract the ids of those participants
bp_i <- dep_rs1_primary$ergoid[dep_rs1_primary$CLEANtotalevent2yr1 == "bipolar"]


dep_rs1_primary <- dep_rs1_primary %>%
  mutate(inc_dep1 = case_when(
    CLEANtotalevent2yr1 != "no" ~ "yes",
    CLEANtotalevent2yr1 == "no" ~ "no")
  )

names(dep_rs1_primary)[5] <- "eventd_dep1"

# remove redundant columns
dep_rs1_primary <- dep_rs1_primary[, -4]

names(dep_rs1_primary)[2:3] <- c("startd_dep1", "endd_dep1")

# if eventd_dep1 is the same as the start date, then this case is considered
# prevalent
dep_rs1_primary <- dep_rs1_primary %>%
  mutate(prev_dep1 = case_when(eventd_dep1 == startd_dep1 ~ "yes",
                               TRUE ~ "no"),
         inc_dep1 = ifelse(prev_dep1 == "yes", "prev", inc_dep1))


# repeated depression (2 and more incidences)
dep_rs1_secondary <- dep_rs1[, c(1, 3, 4, 7:8)]


dep_rs1_secondary <- dep_rs1_secondary %>%
  mutate(inc_dep2 = case_when(
    CLEANtotalevent2yr2 != "no" ~ "yes",
    is.na(CLEANtotalevent2yr2) ~ "no")
  )

# rename column
names(dep_rs1_secondary)[5] <- "eventd_dep2"


# remove redundant columns
dep_rs1_secondary <- dep_rs1_secondary[, -4]

names(dep_rs1_secondary)[2:3] <- c("startd_dep2", "endd_dep2")

# ------------- merge depression dataframes ---------------

dep_primary <- rbind(dep_rs1_primary, dep_rs2_primary, dep_rs3_primary)
dep_secondary <- rbind(dep_rs1_secondary, dep_rs2_secondary, dep_rs3_secondary)

# edit the endd column so that it records the event date for incident or prevalent cases or censor 
# date for censored participants
dep_primary$endd_dep1[!is.na(dep_primary$eventd_dep1)] <- 
  dep_primary$eventd_dep1[!is.na(dep_primary$eventd_dep1)]

dep_secondary$endd_dep2[!is.na(dep_secondary$eventd_dep2)] <- 
  dep_secondary$eventd_dep2[!is.na(dep_secondary$eventd_dep2)]

# remove the event date column
dep_primary <- dep_primary %>% select(-eventd_dep1)
dep_secondary <- dep_secondary %>% select(-eventd_dep2)

# remove the startd column for dep_secondary (since it's the same as in dep1)
dep_secondary <- dep_secondary %>% select(-startd_dep2)

# merge the ids of people with bipolar disorder
bp <- c(bp_i, bp_ii, bp_iii) 


# .----------------------------------------------------------------------
# ------------- Diabetes -----------------------------------------------
# -.---------------------------------------------------------------------
dia_p <-  here::here("Datasets", "Diabetes", "DM_data followup study_Silvan_Shiftstudy.sav")
dia <- read.spss(dia_p, to.data.frame = TRUE)


# convert dates to Dates
dia[, 4] <- as.Date(dia[, 4] / 86400, origin = "1582-10-14")


# rename columns to make them consistent
dia <- dia %>%
  rename(prev_dia = Prevalent_DM,
         inc_dia = Inci_DM_2015,
         eventd_dia = Incidentdate_DM)

# convert to factors
dia$prev_dia <- factor(dia$prev_dia)
dia$prev_dia <- recode_factor(dia$prev_dia, 
                              "0" = "no", 
                              "1" = "yes")

dia$inc_dia <- factor(dia$inc_dia)

dia$inc_dia <- recode_factor(dia$inc_dia, 
                             "0" = "no", 
                             "Prevalent_DM" = "prev",
                             "Incident_DM" = "yes")


# Since start and end dates are missing, we'll use the data from the basic
# dataset
dia <- dia %>%
  left_join(., vs %>% select(ergoid, fp_startdate, fp_censordate), by = "ergoid") %>%
  rename(startd_dia = fp_startdate,
         endd_dia = fp_censordate)


# edit the endd column so that it records the event date for incident or prevalent cases
dia$endd_dia[!is.na(dia$eventd_dia)] <- 
  dia$eventd_dia[!is.na(dia$eventd_dia)]

dia <- dia %>% select(-eventd_dia)

# .----------------------------------------------------------------------
# ------------- Lung diseases -------------------------------------------
# .----------------------------------------------------------------------

lung_p <-  here::here("Datasets", "Lung disease", "casfL16.sav")
lung <- read.spss(lung_p, to.data.frame = TRUE)


# convert dates to Dates
lung[, c(4, 7, 8, 9, 16, 18, 21)] <- lapply(lung[, c(4, 7, 8, 9, 16, 18, 21)] / 86400, 
                                            as.Date, origin = "1582-10-14")
# keep only relevant columns
lung <- lung[, c(1, 2, 6, 7, 8, 9, 16, 18, 21)]

# rename columns
names(lung)[3] <- "ic_ok_lung"
names(lung)[4] <- "startd_lung"
names(lung)[5] <- "eventd_COPD"
names(lung)[6] <- "eventd_asthma"

# add incidence and prevalence columns
lung$prev_COPD <- NA
lung$prev_asthma <- NA

lung$inc_COPD <- NA
lung$inc_asthma <- NA

#  prevalence 

# as a general rule, we classify prevalent cases as those who have their event date
# before the lung start date
lung$prev_COPD[lung$eventd_COPD <= lung$startd_lung] <- "yes"
lung$prev_asthma[lung$eventd_asthma <= lung$startd_lung] <- "yes"


# BUT !!

# There are cases with event date prior to entering RS that were classified as  
# normal in casfl16 (eg. participant with id 141 had asthma incidence date in 1947, 
# but was classified as not with asthma ('zeker normaal')). Those will be classified as not prevalent
# those without disease
clean <- c("Zeker normaal", 
           "Normaal", 
           "Restrictief/LF",
           "Subnormaal")

lung$prev_asthma[lung$casfL16 %in% clean & lung$eventd_asthma < lung$startd_lung] <- "no"

# Those who are classified as with COPD but not with asthma, but who still have an 
# event date for asthma before the lung start date, will also be classified as not prevalent
COPD_no_asthma <- c("Probable COPD/dossier",
                    "Definite COPD/dossier", 
                    "Mild COPD/LF",
                    "Mod/Sev COPD/LF")

lung$prev_asthma[lung$casfL16 %in% COPD_no_asthma & lung$eventd_asthma < lung$startd_lung] <- "no"


# all others will be non-prevalent cases
lung$prev_COPD[lung$eventd_COPD > lung$startd_lung] <- "no"
lung$prev_asthma[lung$eventd_asthma > lung$startd_lung] <- "no"
lung$prev_COPD[is.na(lung$eventd_COPD)] <- "no"
lung$prev_asthma[is.na(lung$eventd_asthma)] <- "no"

# incidence 

# incident cases are all those whose are not prevalent and whose event 
# date is after the lung start date (startd_lung)
lung$inc_COPD[lung$eventd_COPD > lung$startd_lung] <- "yes" 
lung$inc_asthma[lung$eventd_asthma > lung$startd_lung] <- "yes" 

lung$inc_COPD[lung$prev_COPD == "yes"] <- "prev" 
lung$inc_asthma[lung$prev_asthma == "yes"] <- "prev" 

lung$inc_COPD[is.na(lung$eventd_COPD)] <- "no" 
lung$inc_asthma[is.na(lung$eventd_asthma)] <- "no"

# Those who are classified as with COPD but not with asthma, but who still have an 
# event date for asthma before the lung start date, will be classified as not incident 
# (those are the same cases dealt with on line 709)
lung$inc_asthma[lung$casfL16 %in% COPD_no_asthma & lung$eventd_asthma < lung$startd_lung] <- "no"


# There who are classified as normal but still have event date for asthma will be classified as
# not incident (those are the same cases dealt with on line 600)
lung$inc_asthma[lung$casfL16 %in% clean & lung$eventd_asthma < lung$startd_lung] <- "no"

# finally remove those who didn't give informed consent
lung$inc_asthma[lung$ic_ok_lung == "no"] <- NA
lung$prev_asthma[lung$ic_ok_lung == "no"] <- NA

# remove cases of possible asthma (only indicated by)
lung$inc_asthma[lung$casfL16 == "Possible Astma en/of COPD" & lung$inc_asthma == "yes"] <- "no"
lung$eventd_asthma[lung$casfL16 == "Possible Astma en/of COPD" & lung$inc_asthma == "yes"] <- NA

# remove redundant columns
lung <- lung[, -c(2, 7:9)]

# remove the event dates for asthma for those who are classified as prevalent
lung$eventd_asthma[lung$prev_asthma == "yes"] <- NA
lung$eventd_COPD[lung$prev_COPD == "yes"] <- NA

# add censordate as the end date for COPD and asthma
lung <- lung %>%
  left_join(., vs %>% select(ergoid, fp_censordate), by = "ergoid") %>%
  mutate(endd_COPD = fp_censordate,
             endd_asthma = fp_censordate)
  

# edit the endd column so that it records the event date for incident or prevalent cases
lung$endd_COPD[!is.na(lung$eventd_COPD)] <- 
  lung$eventd_COPD[!is.na(lung$eventd_COPD)]

lung$endd_asthma[!is.na(lung$eventd_asthma)] <- 
  lung$eventd_asthma[!is.na(lung$eventd_asthma)]

lung <- lung %>% select(-c(eventd_COPD, eventd_asthma, fp_censordate))


# .----------------------------------------------------------------------
# ------------- Stroke -------------------------------------------
# .----------------------------------------------------------------------

# Update: 22 April 2024. New stroke data has been provided to us, hence we updated this script to 
# load it within the multimorbidity dataset. The new, updated data are called here stoke_new, the 
# original script has been commented off.

# stroke_p <-  here::here("Datasets", "Stroke", "Archive", "Strokes RSI-III 01-01-2016 (28-08-2017).sav")
stroke_p_new <-  here::here("Datasets", "Stroke", "Stroke_2020_Velek_14-02-2023.sav")

# stroke <- read.spss(stroke_p, to.data.frame = TRUE)
stroke_new <- read.spss(stroke_p_new, to.data.frame = TRUE)

# convert dates to Dates
# stroke[, c(3, 8, 13:15)] <- lapply(stroke[, c(3, 8, 13:15)] / 86400, 
#                                    as.Date, origin = "1582-10-14")

stroke_new[, c(3, 7, 11:12)] <- lapply(stroke_new[, c(3, 7, 11:12)] / 86400, 
                                   as.Date, origin = "1582-10-14")

# rename columns
# names(stroke)[6] <- "ic_ok_stroke"
# names(stroke)[5] <- "prev_stroke"
# names(stroke)[9] <- "inc_stroke"

names(stroke_new)[4] <- "ic_ok_stroke"
names(stroke_new)[5] <- "prev_stroke"
names(stroke_new)[6] <- "inc_stroke"

# keep only relevant columns
# stroke <- stroke[, c(1, 3, 6, 5, 8, 9, 13)]
stroke_new <- stroke_new[, c(1, 3, 4, 5, 6, 7, 11)]

# censor date is either date of incident event, date of last contact
# for the living, or date of death for the deceased
# names(stroke)[7] <- "endd_stroke"
# names(stroke)[5] <- "eventd_stroke"
# names(stroke)[2] <- "startd_stroke"


names(stroke_new)[7] <- "endd_stroke"
names(stroke_new)[6] <- "eventd_stroke"
names(stroke_new)[2] <- "startd_stroke"

# levels(stroke$inc_stroke) <- c(levels(stroke$inc_stroke), "prev")
levels(stroke_new$inc_stroke) <- c(levels(stroke_new$inc_stroke), "prev")

# stroke$inc_stroke <- recode_factor(stroke$inc_stroke, 
#                                    "No" = "no", 
#                                    "Yes" = "yes")

stroke_new$inc_stroke <- recode_factor(stroke_new$inc_stroke, 
                                   "No stroke" = "no", 
                                   "Incident stroke" = "yes")

# stroke$prev_stroke <- recode_factor(stroke$prev_stroke, 
#                                     "No prevalent stroke" = "no", 
#                                     "Prevalent stroke" = "yes")

stroke_new$prev_stroke <- recode_factor(stroke_new$prev_stroke,
                                    "no prevalent stroke" = "no",
                                    "prevalent stroke" = "yes")


stroke_new$ic_ok_stroke <- recode_factor(stroke_new$ic_ok_stroke, 
                                    "No" = "no", 
                                    "Yes" = "yes")

# stroke$inc_stroke[stroke$prev_stroke == "yes"] <- "prev"
stroke_new$inc_stroke[stroke_new$prev_stroke == "yes"] <- "prev"

# edit the endd column so that it records the event date for incident or prevalent cases
# stroke$endd_stroke[!is.na(stroke$eventd_stroke)] <- 
  # stroke$eventd_stroke[!is.na(stroke$eventd_stroke)]

# stroke <- stroke %>% select(-c(eventd_stroke))
stroke_new <- stroke_new %>% select(-c(eventd_stroke))

# .----------------------------------------------------------------------
# ------------- TIA ----------------------------------------------------
# .----------------------------------------------------------------------

# Update: 22 April 2024. New TIA data has been provided to us, hence we updated this script to 
# load it within the multimorbidity dataset. The new, updated data are called here tia_new, the 
# original script has been commented off.

# tia_p <-  here::here("Datasets", "Stroke", "Archive", 
#                      "TIA (focal and mixed) RSI-III 01-01-2016 (28-08-2017).sav")
tia_p_new <-  here::here("Datasets", "Stroke", 
                     "TIA(focal&mixed)_2020_Velek_12-02-2023.sav")

# tia <- read.spss(tia_p, to.data.frame = TRUE)
tia_new <- read.spss(tia_p_new, to.data.frame = TRUE)

# convert dates to Dates
# tia[, c(5, 16:19)] <- lapply(tia[, c(5, 16:19)] / 86400, 
#                              as.Date, origin = "1582-10-14")
tia_new[, c(2, 7:9)] <- lapply(tia_new[, c(2, 7:9)] / 86400, 
                             as.Date, origin = "1582-10-14")

# keep only relevant columns
# tia <- tia[, c(1, 5:7, 9, 16:17)]
tia_new <- tia_new[, c(1:2, 4:8)]

# rename columns
# censor date is either date of incident event, date of last contact
# for the living, or date of death for the deceased

# names(tia)[4] <- "ic_ok_tia"
# names(tia)[5] <- "inc_tia"
# names(tia)[6] <- "eventd_tia"
# names(tia)[7] <- "endd_tia"
# names(tia)[2] <- "startd_tia"

names(tia_new)[3] <- "ic_ok_tia"
names(tia_new)[5] <- "inc_tia"
names(tia_new)[4] <- "prev_tia"
names(tia_new)[6] <- "eventd_tia"
names(tia_new)[7] <- "endd_tia"
names(tia_new)[2] <- "startd_tia"


# levels(tia$inc_tia) <- c(levels(tia$inc_tia), "prev")
levels(tia_new$inc_tia) <- c(levels(tia_new$inc_tia), "prev")

# tia$inc_tia <- recode_factor(tia$inc_tia, 
#                              "No" = "no", 
#                              "Yes" = "yes")

# tia$prev_tia <- recode_factor(tia$prev_tia, 
#                               "No prevalent TIA" = "no", 
#                               "Prevalent TIA" = "yes")

# tia$inc_tia[tia$prev_tia == "yes"] <- "prev"
tia_new$inc_tia[tia_new$prev_tia == "yes"] <- "prev"


# edit the endd column so that it records the event date for incident or prevalent cases
# tia$endd_tia[!is.na(tia$eventd_tia)] <- 
#   tia$eventd_tia[!is.na(tia$eventd_tia)]

# tia <- tia %>% select(-c(eventd_tia))
tia_new <- tia_new %>% select(-c(eventd_tia))
  


# .----------------------------------------------------------------------
# ------------- add it all together ------------------------------------
# .----------------------------------------------------------------------

shift_data <- list(start %>% select(ergoid, rs_cohort, fp_startdate, fp_censordate), 
                   vs %>% select(ergoid, mortd), 
                   basic %>% select(ergoid, birthd, sex),
                   chd,
                   hf,
                   can_primary,
                   can_secondary,
                   dep_primary,
                   dep_secondary,
                   dia, 
                   park,
                   dem,
                   lung,
                   stroke_new,
                   tia_new) %>% 
  reduce(left_join, by = "ergoid") 

# drop all unused levels from factors
shift_data[] <- lapply(shift_data, function(x) if(is.factor(x)) factor(x) else x)

# convert all character columns to factor
shift_data[] <- lapply(shift_data, function(x) if(is.character(x)) factor(x) else x)

# convert ergoid to factor
shift_data$ergoid <- factor(shift_data$ergoid)


# save(shift_data, file = here::here("Data", "shift_data.Rdata")) 
save(shift_data, file = here::here("Data", "shift_data_2024-04-22.Rdata")) 
