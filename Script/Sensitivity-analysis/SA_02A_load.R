# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- Sensitivity analysis: load in data ------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# .This script combines takes the cleaned data for all ten diseases and excludes those with missing values, 
# i.e. only participants with follow up on all ten diseases are included and who were free of the ten diseaseses 
# at baseline

# It is identical to the '02_load.R' code, the only difference is that it works with different data in which 
# depressive symptoms are ignored for the diagnosis of depression. All the other diseases are unchanged.

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
require(lubridate)
require(grid)
require(gridExtra)
require(tidyverse)


# start here
setwd(here::here())


# ------- Prepare the data -----------------

load(here::here("Data", "Sensitivity-analysis", "shift_data_dep.Rdata")) 
shift_data <- shift_data_dep
rm(shift_data_dep)

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

# remove information about TIA, recurrent depression and recurrent cancer
# for now if you want to run edges.R or risk_multimorbidity.R or risk_second_condition.R you need to
# comment this off
shift_data <- shift_data %>%
  dplyr::select(-contains("tia")) %>%
  dplyr::select(-contains("dep2")) %>%
  dplyr::select(-contains("can2"))

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

# keep only complete cases, skip this line if you want to first exclude some 
# conditions to increase the sample size
# "prev_CHD" is the first column with data about a specific condition. The previous columns
# are basic population characteristics about the participants

shift_data <- shift_data %>%
  filter_at(vars(prev_chd:last_col()), all_vars(!is.na(.)))

# number of complete cases
nrow(shift_data) 
# 9594 participants with complete data for all conditions

# get the latest start date for each participant
shift_data <- shift_data %>%  
  rowwise() %>% 
  mutate(latest_startd = max(c_across(contains("startd_"))))

# get the earliest end date for each participant (across conditions)
shift_data <- shift_data %>% 
  rowwise() %>% 
  mutate(earliest_endd = min(c_across(contains("endd_")))) 

# get the earliest end date for each participant, excluding incident cases
shift_data <- shift_data %>% 
  rowwise() %>% 
  mutate(earliest_endd = min(c_across(contains("endd_")))) 

# let's also get the latest endd as a proxy for censordate
shift_data <- shift_data %>% 
  rowwise() %>% 
  mutate(latest_endd = max(c_across(contains("endd_")))) 

# helper function to convert yes-no string to boolean
to_logical <- function(x){ifelse(x == "yes", TRUE, FALSE)}

incidence_cohort <- shift_data %>%
  mutate_at(vars(contains("prev_")), to_logical) %>%
  mutate_at(vars(contains("inc_")), to_logical) %>%
  rowwise() %>% 
  mutate(healthy = sum(c_across(contains("prev_")))) %>%
  ungroup() %>%
  mutate(healthy = case_when(healthy == 0 ~ 1,
                             TRUE ~ 0),
         healthy = as.logical(healthy)) %>%
  
  # identify healthy individuals (those with no prevalent case)
  filter(healthy == TRUE) %>%
  dplyr::select(-contains(c("prev_", "ic_ok")), -healthy)

# The incidence cohort contains all individuals who were free of any of the medical
# conditions at the baseline


# remove those who developed an incident case before the latest start date to avoid immortal 
# time bias
incidence_cohort <- incidence_cohort %>%
  filter(earliest_endd > latest_startd)

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

# filter eligible participants and split the tables into dates and incidence data
get_event_dates <- function(cohort){
  cohort %>%
    select(ergoid, contains(c("endd_", "mortd")))
}


get_inc <- function(cohort){
  cohort %>%
    mutate(inc_mort = ifelse(is.na(mortd), FALSE, TRUE)) %>%
    select(contains("inc_"))
}

inc_data <- get_inc(incidence_cohort)
event_data <- get_event_dates(incidence_cohort)

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

# add the end of follow up for all conditions. That is the earliest end of follow 
# up among all conditions. That way we are sure we have full follow up date for 
# all conditions until that date

# We need to find the earliest end date for each participant, but excluding 
# incident cases. We do it by filtering the event_dates df with the inc_data df

# keep only data on conditions (remove mortality)
is_event <- as.matrix(inc_data[, -11])
censored_dates <- as.matrix(event_data[, 2:11])

censored_dates[is_event] <- NA

# add the dates to the incidence_cohort df
incidence_cohort$exit_date <- apply(censored_dates, 1, min, na.rm = TRUE)

 
save(incidence_cohort, file = here::here("Data", "Sensitivity-analysis", "incidence_cohort_dep.RData"))        

