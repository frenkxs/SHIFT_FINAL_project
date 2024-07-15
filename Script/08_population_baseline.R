# .-------------------------------------------------------------------------------------------------
# .-------------------------------------------------------------------------------------------------
# .-------------------------------------------------------------------------------------------------
# ------------- Tab. 1: Baseline characteristics of the study participants -------------------------
# .-------------------------------------------------------------------------------------------------
# .-------------------------------------------------------------------------------------------------
# .-------------------------------------------------------------------------------------------------


# This script takes produces Table 1: population characteristic of the sample
# For all questions about it, contact Premysl Velek at p.velek@erasmusmc.nl 


# .-------------------------------------------------------------------------------
# ------------------------ Preliminaries ----------------------------------------
# .--------------------------------------------------------------------------------

# libraries needed in this script
packages_needed <- c("foreign", "tidyverse", "lubridate", "here", "grid", "gridExtra")

# install missing libraries
install.packages(setdiff(packages_needed, rownames(installed.packages())))  

# load libraries
require(foreign)
require(tidyverse)
require(lubridate)



# excluded participants
load(here::here("Data", "shift_data.RData"))

# study cohort
load(here::here("Data", "incidence_cohort.RData"))


# get the latest start date for each participant
shift_data <- shift_data %>%  
  rowwise() %>% 
  mutate(latest_startd = max(c_across(contains("startd_")), na.rm = TRUE))

# get the earliest end date for each participant (across conditions)
shift_data <- shift_data %>% 
  rowwise() %>% 
  mutate(earliest_endd = min(c_across(contains("endd_")), na.rm = TRUE)) 

# get the earliest end date for each participant, excluding incident cases
shift_data <- shift_data %>% 
  rowwise() %>% 
  mutate(earliest_endd = min(c_across(contains("endd_")), na.rm = TRUE)) 

# let's also get the latest endd as a proxy for censordate
shift_data <- shift_data %>% 
  rowwise() %>% 
  mutate(latest_endd = max(c_across(contains("endd_")), na.rm = TRUE)) 


# ergoids of excluded participants
excluded <- shift_data %>%
  dplyr::filter(!(ergoid %in% incidence_cohort$ergoid))

# ergoids of incompletely screened participants (subset of excluded participants)
incomplete <- shift_data %>%
  filter_at(vars(prev_chd:last_col()), any_vars(is.na(.))) %>%
  dplyr::filter(!(ergoid %in% no_ic))

# ergoids of excluded participants due to prevalent cases
excluded <- shift_data %>%
  dplyr::filter(!(ergoid %in% incidence_cohort$ergoid),
                !(ergoid %in% incomplete$ergoid),
                !(ergoid %in% no_ic))


# ergoids of excluded participants due to informed consent withdrawal
excluded <- shift_data %>%
  dplyr::filter(ergoid %in% no_ic)


# .-------------------
# ------------------- AGE -------------------------
# .-------------------
 

# median age at baseline
age_baseline <- incidence_cohort %>%
    mutate(age_at_base = interval(birthd, latest_startd) / years(1)) %>%
    pull(age_at_base) %>% 
    quantile(., probs = c(0.5, 0.25, 0.75)) %>% round(., 1)

age_baseline_male <- incidence_cohort %>%
    filter(sex == "male") %>%
    mutate(age_at_base = interval(birthd, latest_startd) / years(1)) %>%
    pull(age_at_base) %>% 
    quantile(., probs = c(0.5, 0.25, 0.75)) %>% round(., 1)

age_baseline_female <- incidence_cohort %>%
    filter(sex == "female") %>%
    mutate(age_at_base = interval(birthd, latest_startd) / years(1)) %>%
    pull(age_at_base) %>% 
    quantile(., probs = c(0.5, 0.25, 0.75)) %>% round(., 1)


# Wilcoxon-Mann-Whitney test for equal medians
incidence_cohort %>%
  mutate(age_at_base = interval(birthd, latest_startd) / years(1)) %>%
  select(age_at_base, sex) %>% 
  wilcox.test(age_at_base ~ sex, data = .)

table(excluded$sex)
table(excluded$sex) / sum(table(excluded$sex))

table(incomplete$sex)
table(incomplete$sex) / sum(table(incomplete$sex))



# median age at baseline: EXCLUDED: PREVALENT CASES
age_baseline <-  excluded %>%
  mutate(age_at_base = interval(birthd, latest_startd) / years(1)) %>%
  pull(age_at_base) %>% 
  quantile(., probs = c(0.5, 0.25, 0.75)) %>% round(., 1)

age_baseline_male <- excluded %>%
  filter(sex == "male") %>%
  mutate(age_at_base = interval(birthd, latest_startd) / years(1)) %>%
  pull(age_at_base) %>% 
  quantile(., probs = c(0.5, 0.25, 0.75)) %>% round(., 1)

age_baseline_female <- excluded %>%
  filter(sex == "female") %>%
  mutate(age_at_base = interval(birthd, latest_startd) / years(1)) %>%
  pull(age_at_base) %>% 
  quantile(., probs = c(0.5, 0.25, 0.75)) %>% round(., 1)

excluded %>%
  mutate(age_at_base = interval(birthd, latest_startd) / years(1)) %>%
  select(age_at_base, sex) %>% 
  wilcox.test(age_at_base ~ sex, data = .)



# median age at baseline: EXCLUDED: INCOMPLETE
age_baseline <-  incomplete %>%
  mutate(age_at_base = interval(birthd, latest_startd) / years(1)) %>%
  pull(age_at_base) %>% 
  quantile(., probs = c(0.5, 0.25, 0.75)) %>% round(., 1)

age_baseline_male <- incomplete %>%
  filter(sex == "male") %>%
  mutate(age_at_base = interval(birthd, latest_startd) / years(1)) %>%
  pull(age_at_base) %>% 
  quantile(., probs = c(0.5, 0.25, 0.75)) %>% round(., 1)

age_baseline_female <- incomplete %>%
  filter(sex == "female") %>%
  mutate(age_at_base = interval(birthd, latest_startd) / years(1)) %>%
  pull(age_at_base) %>% 
  quantile(., probs = c(0.5, 0.25, 0.75)) %>% round(., 1)

incomplete %>%
  mutate(age_at_base = interval(birthd, latest_startd) / years(1)) %>%
  select(age_at_base, sex) %>% 
  wilcox.test(age_at_base ~ sex, data = .)


# ------------------- SEX -------------------------
# .-------------------
# 
# sex ratio
sex_n <- table(incidence_cohort$sex)
pct_male <- sex_n[1] / sum(sex_n)
pct_female <- sex_n[2] / sum(sex_n)
    
# --------------------- Smoking status --------------------------
# .-------------------
# 
# We have to decide what smoking status we consider. We take the one that's nearest to the 
# follow up start for the SHIFT study, ie. the one closest to the latest_startd in the SHIFT dataset

# this data 
load(here::here("Data", "smoking.RData"))

smoking$ergoid <- factor(smoking$ergoid)

smoking <- left_join(incidence_cohort %>% select(ergoid, latest_startd), smoking, by = "ergoid")
smoking <- left_join(excluded %>% select(ergoid, latest_startd), smoking, by = "ergoid")
smoking <- left_join(incomplete %>% select(ergoid, latest_startd), smoking, by = "ergoid")


smoking <- smoking %>%
    mutate(time_int1 = abs(interval(latest_startd, int1_d) / years(1)),
           time_int2 = abs(interval(latest_startd, int2_d) / years(1)),
           time_int3 = abs(interval(latest_startd, int3_d) / years(1)),
           time_int4 = abs(interval(latest_startd, int4_d) / years(1)))


times <- smoking %>%
    select(contains("time")) 

# helper for which.min so that it returns NA when the vector only has NAs
# only needed when calculating the smoking status of excluded participants
which.minNA <- function(x){
  if(FALSE %in% is.na(x)){
    return(which.min(x))
  } else {
    return(NA)
  }
}

closest <- times %>%
  rowwise() %>%
  mutate(closest = which.minNA(c_across(cols = everything()))) %>%
  pull(closest)


smoking <- smoking %>%
    select(contains("smoker"))

smoker <- rep(NA, nrow(smoking))

for (i in 1:nrow(smoking)) {
    smoker[i] <- ifelse(is.na(closest[i]), "remove", smoking[[i, closest[i]]])
}


smoker <- data.frame(sex = incidence_cohort$sex, smoker = smoker, ergoid = incidence_cohort$ergoid)
save(smoker, file = here::here("script", "data", "smoker.RData"))

smoker <- data.frame(sex = excluded$sex, smoker = smoker)
smoker <- data.frame(sex = incomplete$sex, smoker = smoker)

table(smoker, useNA = "ifany")
table(smoker$smoker, useNA = "ifany")
table(smoker$smoker, useNA = "ifany") / all

sum(table(smoker, useNA = "ifany"))

# values with non-missing data
all <- sum(table(smoker)[, c(1:3)])
all_men <- sum(table(smoker)[1, c(1:3)])
all_women <- sum(table(smoker)[2, c(1:3)])


# overall
table(smoker$smoker, useNA = "ifany")
round((table(smoker$smoker, useNA = "ifany") / sum(table(smoker$smoker)[1:3])) * 100, 1)

# men
men <- table(smoker$smoker[smoker$sex == "male"])
round((table(smoker$smoker[smoker$sex == "male"], useNA = "ifany") / sum(table(smoker$smoker[smoker$sex == "male"])[1:3])) * 100, 1)

# women
women <- table(smoker$smoker[smoker$sex == "female"])
round((table(smoker$smoker[smoker$sex == "female"], useNA = "ifany") / sum(table(smoker$smoker[smoker$sex == "female"])[1:3])) * 100, 1)


chisq.test(table(smoker)[, c(1:3)])

# values with missing data
sum(table(smoker, useNA = "ifany")[, c(4:5)]) / all


sum(table(smoker)[, 1])
sum(table(smoker)[, 1]) / all

sum(table(smoker)[, 2])
sum(table(smoker)[, 2]) / all

sum(table(smoker)[, 3])
sum(table(smoker)[, 3]) / all

table(smoker)[, 1]
sum(table(smoker)[1, 1]) / all_men
sum(table(smoker)[2, 1]) / all_women

table(smoker)[, 2]
sum(table(smoker)[1, 2]) / all_men
sum(table(smoker)[2, 2]) / all_women

table(smoker)[, 3]
sum(table(smoker)[1, 3]) / all_men
sum(table(smoker)[2, 3]) / all_women

719 / sum(table(smoker)[1, ]) * 100
1396 / sum(table(smoker)[1, ]) * 100
461 / sum(table(smoker)[1, ]) * 100

682 / sum(table(smoker)[2, ]) * 100
1294 / sum(table(smoker)[2, ]) * 100
1484 / sum(table(smoker)[2, ]) * 100

sum(table(smoker)[, 1]) / sum(table(smoker))
sum(table(smoker)[, 2]) / sum(table(smoker))
sum(table(smoker)[, 3]) / sum(table(smoker))



# --------------------- Marital status --------------------------
# .-------------------
# 
# We have to decide what marital status we consider. We take the one that's nearest to the 
# follow up start for the SHIFT study, ie. the one closest to the latest_startd in the SHIFT dataset

load(here("script", "data", "marital.RData"))

marital$ergoid <- factor(marital$ergoid)

marital <- left_join(incidence_cohort %>% select(ergoid, latest_startd), marital, by = "ergoid")
marital <- left_join(excluded %>% select(ergoid, latest_startd), marital, by = "ergoid")
marital <- left_join(incomplete %>% select(ergoid, latest_startd), marital, by = "ergoid")

marital <- marital %>%
    mutate(time_int1 = abs(interval(latest_startd, int1_d) / years(1)),
           time_int2 = abs(interval(latest_startd, int2_d) / years(1)),
           time_int3 = abs(interval(latest_startd, int3_d) / years(1)))


times <- marital %>%
    select(contains("time")) 

closest <- apply(times, 1, which.min)
closest <- as.numeric(closest)


marital <- marital %>%
    select(contains("partner"))

with_partner <- rep(NA, nrow(marital))

for (i in 1:nrow(marital)) {
    if(is.na(closest[i])){
        temp <- marital[[i, 1]]
    } else {temp <- marital[[i, closest[i]]]}
    with_partner[i] <- temp
}


living_with_partner <- data.frame(sex = incidence_cohort$sex, with_partner = with_partner, 
                                  ergoid = incidence_cohort$ergoid)

living_with_partner <- data.frame(sex = excluded$sex, with_partner = with_partner, 
                                  ergoid = excluded$ergoid)

living_with_partner <- data.frame(sex = incomplete$sex, with_partner = with_partner, 
                                  ergoid = incomplete$ergoid)

partner <- table(living_with_partner[, 1:2], useNA = "ifany")

chisq.test(partner)

colSums(partner)
all <- sum(partner[, c(1:2)])
colSums(partner) / all


pct_male_wo <- partner[1, 1] / (partner[1, 2] + partner[1, 1])
pct_female_wo <- partner[2, 1] / (partner[2, 2] + partner[2, 1])

pct_male_w <- partner[1, 2] / (partner[1, 2] + partner[1, 1])
pct_female_w <- partner[2, 2] / (partner[2, 2] + partner[2, 1])


(2244 + 2375) / ((2244 + 2375) + (239 + 888))
(239 + 888) / ((2244 + 2375) + (239 + 888))

# --------------------- Education --------------------------

load(here("script", "data", "education.RData"))


education$ergoid <- factor(education$ergoid)
education <- left_join(incidence_cohort %>% select(ergoid, sex), education, by = "ergoid") 
education <- left_join(excluded %>% select(ergoid, sex), education, by = "ergoid") 
education <- left_join(incomplete %>% select(ergoid, sex), education, by = "ergoid") 


edu <- table(education[, 2:3], useNA = "ifany")
colSums(edu)
all <- sum(colSums(edu[, 1:4]))

chisq.test(edu)

colSums(edu) / all

# men
edu[1, ]
round(edu[1, ] / sum(edu[1, c(1:4)]), 3)

# women
edu[2, ]
round(edu[2, ] / sum(edu[2, c(1:4)]), 3)


round(colSums(edu) / all, 3)

sum_m <- sum(table(education[, 2:3])[1, ])
sum_f <- sum(table(education[, 2:3])[2, ])

sum(table(education[, 2:3])[, 1]) + 
  sum(table(education[, 2:3])[, 2]) + 
  sum(table(education[, 2:3])[, 3]) + 
  sum(table(education[, 2:3])[, 4])


# --------------------- Hypertension --------------------------

# We have to decide what smoking status we consider. We take the one that's nearest to the 
# follow up start for the SHIFT study, ie. the one closest to the latest_startd in the SHIFT dataset

load(here("script", "data", "hypertension.RData"))

ht$ergoid <- factor(ht$ergoid)

ht <- left_join(incidence_cohort %>% select(ergoid, latest_startd), ht, by = "ergoid")
ht <- left_join(excluded %>% select(ergoid, latest_startd), ht, by = "ergoid")
ht <- left_join(incomplete %>% select(ergoid, latest_startd), ht, by = "ergoid")

ht <- ht %>%
    mutate(time_visit1 = abs(interval(latest_startd, visit11_d) / years(1)),
           time_visit2 = abs(interval(latest_startd, visit12_d) / years(1)),
           time_visit3 = abs(interval(latest_startd, visit21_d) / years(1)),
           time_visit4 = abs(interval(latest_startd, visit31_d) / years(1)),
           time_visit4 = abs(interval(latest_startd, visit32_d) / years(1)),
           time_visit5 = abs(interval(latest_startd, visit1_d) / years(1)),
           time_visit6 = abs(interval(latest_startd, visit2_d) / years(1)))

ht_f <- ht[c("ergoid", "e2_HT2018")]
summary(ht_f)

missing <- ht_f$ergoid[is.na(ht_f$e2_HT2018)] 
ht_f$e2_HT2018[ht_f$ergoid %in% missing] <- ht$e3_HT2018[ht$ergoid %in% missing]


missing <- ht_f$ergoid[is.na(ht_f$e2_HT2018)] 
ht_f$e2_HT2018[ht_f$ergoid %in% missing] <- ht$ep_HT2018[ht$ergoid %in% missing]

missing <- ht_f$ergoid[is.na(ht_f$e2_HT2018)] 
ht_f$e2_HT2018[ht_f$ergoid %in% missing] <- ht$ej_HT2018[ht$ergoid %in% missing]

missing <- ht_f$ergoid[is.na(ht_f$e2_HT2018)] 
ht_f$e2_HT2018[ht_f$ergoid %in% missing] <- ht$e4_HT2018[ht$ergoid %in% missing]

missing <- ht_f$ergoid[is.na(ht_f$e2_HT2018)] 
ht_f$e2_HT2018[ht_f$ergoid %in% missing] <- ht$e5_HT2018[ht$ergoid %in% missing]

summary(ht_f)

ht_f <- ht_f %>%
    mutate(has_ht = case_when(e2_HT2018 == "yes" ~ TRUE,
                              e2_HT2018 == "no" ~ FALSE))


table(ht_f$has_ht) / sum(table(ht_f$has_ht))

ht_f <- left_join(incidence_cohort %>% select(ergoid, sex), ht_f, by = "ergoid")
ht_f <- left_join(excluded %>% select(ergoid, sex), ht_f, by = "ergoid")
ht_f <- left_join(incomplete %>% select(ergoid, sex), ht_f, by = "ergoid")


table(ht_f$has_ht)

table(ht_f[, c(2, 4)]) / sum(table(ht_f[, c(2, 4)]))
table(ht_f[, c(2, 4)]) / sum(table(ht_f[, c(2, 4)]))

# Blood pressure ---------

load(here("script", "data", "blood_pressure.RData"))
bp$ergoid <- factor(bp$ergoid)

bp <- left_join(incidence_cohort %>% select(ergoid, latest_startd), bp, by = "ergoid")
bp <- left_join(excluded %>% select(ergoid, latest_startd), bp, by = "ergoid")
bp <- left_join(incomplete %>% select(ergoid, latest_startd), bp, by = "ergoid")

# remove missing data: participants with no bp measurements
bp <- bp %>%
  rowwise() %>%
  mutate(missing = sum(is.na(c_across(contains("stolic")))),
         missing = ifelse(missing == 12, TRUE, FALSE)) %>%
  dplyr::filter(missing == FALSE)

# add one fake measurement column (e2_visit2_d) to keep it symmetric with the dates 
# (two dates for each cohort)

bp <- bp %>%
  mutate(e2_visit2_d = NA) %>%
  relocate(e2_visit2_d, .after = e2_visit1_d)
  

# find out what measurements are in (irrespective of interview dates)
bp <- bp %>%
  rowwise() %>%
  mutate(missing_dia = list(is.na(c_across(contains("diastolic")))),
         missing_sys = list(is.na(c_across(contains("systolic")))))

# matrices with bp velues across the measurements: FALSE: missing, TRUE: value exist
dia <- !(matrix(unlist(bp$missing_dia), byrow = TRUE, nrow = length(bp$missing_dia)))
sys <- !(matrix(unlist(bp$missing_sys), byrow = TRUE, nrow = length(bp$missing_sys)))

bp <- bp %>%
  mutate(values_dia = list(which(missing_dia %in% FALSE)),
         values_sys = list(which(missing_sys %in% FALSE)))
  
bp <- bp %>%
  mutate(time_1 =  abs(interval(latest_startd, e1_visit1_d) / years(1)),
         time_2 =  abs(interval(latest_startd, e1_visit2_d) / years(1)),
         time_3 =  abs(interval(latest_startd, e2_visit1_d) / years(1)),
         time_4 =  abs(interval(latest_startd, e2_visit2_d) / years(1)),
         time_5 =  abs(interval(latest_startd, e3_visit1_d) / years(1)),
         time_6 =  abs(interval(latest_startd, e3_visit2_d) / years(1)),
         time_7 =  abs(interval(latest_startd, ep_visit1_d) / years(1)),
         time_8 =  abs(interval(latest_startd, ep_visit2_d) / years(1)),
         time_9 =  abs(interval(latest_startd, e4_visit1_d) / years(1)),
         time_10 = abs(interval(latest_startd, e4_visit2_d) / years(1)),
         time_11 = abs(interval(latest_startd, ej_visit1_d) / years(1)),
         time_12 = abs(interval(latest_startd, ej_visit2_d) / years(1)))

# remove from times those that don't have a matching bp measurement
times <- bp %>%
  select(contains("time"), ergoid) 

times1 <- times[, c(1, 3, 5, 7, 9, 11, 13)]
times2 <- times[, c(2, 4, 6, 8, 10, 12, 13)]

times1[, -7][!dia] <- NA
times2[, -7][!dia] <- NA

times <- cbind(times1[, -7], times2[, -7])
times <- times[, c(1, 7, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12)]

closest <- apply(times, 1, which.min)
closest <- as.numeric(closest)

closest[closest %in% c(1, 2)] <- 1
closest[closest %in% c(3, 4)] <- 2
closest[closest %in% c(5, 6)] <- 3
closest[closest %in% c(7, 8)] <- 4
closest[closest %in% c(9, 10)] <- 5
closest[closest %in% c(11, 12)] <- 6

bp$closest <- closest

systolic <- bp %>%
  select(contains("systolic")) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_all(as.numeric)

diastolic <- bp %>%
  select(contains("diastolic")) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_all(as.numeric)

systolic_bp <- rep(NA, nrow(systolic))
diastolic_bp <- rep(NA, nrow(diastolic))

for (i in 1:nrow(systolic)) {
  systolic_bp[i] <- ifelse(is.na(closest[[i]]), systolic[[i, 1]], systolic[[i, closest[i]]])
}


for (i in 1:nrow(diastolic)) {
  diastolic_bp[i] <- ifelse(is.na(closest[[i]]), diastolic[[i, 1]], diastolic[[i, closest[i]]])
}


bp_final <- data.frame(ergoid = bp$ergoid, 
                       sex = incidence_cohort$sex[incidence_cohort$ergoid %in% bp$ergoid], 
                       systolic = systolic_bp,
                       diastolic = diastolic_bp)


bp_final <- data.frame(ergoid = bp$ergoid, sex = excluded$sex[excluded$ergoid %in% bp$ergoid], 
                       systolic = systolic_bp,
                       diastolic = diastolic_bp)

bp_final <- data.frame(ergoid = bp$ergoid, sex = incomplete$sex[incomplete$ergoid %in% bp$ergoid], 
                       systolic = systolic_bp,
                       diastolic = diastolic_bp)




# overall
apply(bp_final[, 3:4], 2, quantile, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)


# men
apply(bp_final[bp_final$sex == "male", 3:4], 2, quantile, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# women
apply(bp_final[bp_final$sex == "female", 3:4], 2, quantile, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)


wilcox.test(systolic ~ sex, data = bp_final)
wilcox.test(diastolic ~ sex, data = bp_final)


# --------------------- Ancestry --------------------------

load(here::here("script", "data", "ancestry.RData"))

anc <- left_join(incidence_cohort %>% select(ergoid, sex), anc, by = "ergoid")
anc <- left_join(excluded %>% select(ergoid, sex), anc, by = "ergoid")
anc <- left_join(incomplete %>% select(ergoid, sex), anc, by = "ergoid")

# overall
table(anc$ancestry, useNA = "ifany")
round((table(anc$ancestry, useNA = "ifany") / sum(table(anc$ancestry)[1:4])) * 100, 1)

# men
table(anc$ancestry[anc$sex == "male"], useNA = "ifany")
round((table(anc$ancestry[anc$sex == "male"], useNA = "ifany") / sum(table(anc$ancestry[anc$sex == "male"])[1:4])) * 100, 1)

# women
table(anc$ancestry[anc$sex == "female"], useNA = "ifany")
round((table(anc$ancestry[anc$sex == "female"], useNA = "ifany") / sum(table(anc$ancestry[anc$sex == "female"])[1:4])) * 100, 1)


chisq.test(table(anc[, c(2, 3)])[, c(1:3)])
# ----------- Age at first and second diagnosis -------------

load("ages_first_second_event.RData")

# event_type codes:
# 0: censored
# 1: event
# 2: death

load(here::here("script", "data", "conditions_seq.RData"))

# age at first event
conditions_seq %>%
  filter(!(a %in% c(11, 12))) %>%
  pull(age_first) %>%
  quantile(., probs = c(0.25, 0.5, 0.75)) %>% 
  round(., 1)

conditions_seq %>%
  filter(!(a %in% c(11, 12))) %>%
  select(age_first, sex) %>%
  wilcox.test(age_first ~ sex, data = .)


# age at second event
conditions_seq %>%
  filter(!(b %in% c(11, 12))) %>%
  pull(age_second) %>%
  quantile(., probs = c(0.25, 0.5, 0.75)) %>% 
  round(., 1)


conditions_seq %>%
  filter(!(b %in% c(11, 12))) %>%
  select(age_second, sex) %>%
  wilcox.test(age_second ~ sex, data = .)

# by sex
# age at first event: men
conditions_seq %>%
  filter(!(a %in% c(11, 12)),
         sex == "male") %>%
  pull(age_first) %>%
  quantile(., probs = c(0.25, 0.5, 0.75)) %>% 
  round(., 1)


# age at second event: men
conditions_seq %>%
  filter(!(b %in% c(11, 12)),
         sex == "male") %>%
  pull(age_second) %>%
  quantile(., probs = c(0.25, 0.5, 0.75)) %>% 
  round(., 1)


# age at first event: women
conditions_seq %>%
  filter(!(a %in% c(11, 12)),
         sex == "female") %>%
  pull(age_first) %>%
  quantile(., probs = c(0.25, 0.5, 0.75)) %>% 
  round(., 1)


# age at second event: women
conditions_seq %>%
  filter(!(b %in% c(11, 12)),
         sex == "female") %>%
  pull(age_second) %>%
  quantile(., probs = c(0.25, 0.5, 0.75)) %>% 
  round(., 1)

# --------- Years of follow up ---------

((incidence_cohort$latest_startd %--% incidence_cohort$exit_date) / years(1)) %>%
  quantile(., probs = c(0.25, 0.5, 0.75)) %>% round(., 1)

# men
incidence_cohort %>%
  dplyr::filter(sex == "male") %>%
  mutate(follow_up = latest_startd %--% exit_date / years(1)) %>%
  pull(follow_up) %>%
  quantile(., probs = c(0.25, 0.5, 0.75)) %>% round(., 1)


# women
incidence_cohort %>%
  dplyr::filter(sex == "female") %>%
  mutate(follow_up = latest_startd %--% exit_date / years(1)) %>%
  pull(follow_up) %>%
  quantile(., probs = c(0.25, 0.5, 0.75)) %>% round(., 1)


incidence_cohort %>%
  mutate(follow_up = latest_startd %--% exit_date / years(1)) %>%
  select(follow_up, sex) %>%
  wilcox.test(follow_up ~ sex, data = .)


# --------- Number of participants by cohort ---------

load(here::here("cleaned", "shift_data.Rdata"))
load(here::here("script", "data", "incidence_cohort.RData"))

table(incidence_cohort$rs_cohort, useNA = "ifany")

# age at baseline by cohort -------------
load(here::here("script","data", "conditions_seq.RData"))

by_cohort <- incidence_cohort %>%
  select(ergoid, rs_cohort) %>%
  left_join(conditions_seq, ., by = "ergoid")

tapply(by_cohort$age_start, by_cohort$rs_cohort, median)
