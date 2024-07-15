# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT extract data ----------------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# .This script takes the incidence cohort data and transforms them so that for each 
# participants we have a sequence of diseases in chronological order. End of follow-up 
# and mortality are also included

# The script produces three dataframes that are saved into one RData file. Apart from the
#  sequence of diseases, there are also two matrices (event_data and inc_data). inc_data is a 
#  boolean matrix in which for each participants it's indicated whether they either had (TRUE) 
#  or had not (FALSE) a specific disease. event_data has the same dimension, but instead of boolean 
#  values it contains dates, either a date of diagnosis of a particular disease 
#  (if the participant has it) or date of end of follow up  (if the participants doesn't have it).


# For all questions about it, contact Premysl Velek at p.velek@erasmusmc.nl 


# .-------------------------------------------------------------------------------
# ------------------------ Preliminaries ----------------------------------------
# .--------------------------------------------------------------------------------

# libraries needed in this script
packages_needed <- c("foreign", "tidyverse", "lubridate", "here", "grid", "gridExtra",
                     "colorspace", "cowplot", "etm", "survival")

# install missing libraries
install.packages(setdiff(packages_needed, rownames(installed.packages())))  

# load libraries
library(colorspace)
library(foreign)
library(tidyverse)
library(lubridate)
library(cowplot)
library(etm)
library(survival)
library(here)


# start here
setwd(here::here())

# load in incidence cohort
load(here::here("Data", "incidence_cohort.RData"))

# ---------- Sensitivity analysis !! --------------------------------

# This is only if you want to run the sensitivity analysis. 
# Skip if running the main analysis and results.

# if we want to use the data with stricter definition for depression
load(here::here("Data", "Sensitivity-analysis", "incidence_cohort_dep.RData"))

# if we want to use the data that include one prevalent case
load(here::here("Data", "Sensitivity-analysis", "incidence_cohort_one_prev.RData"))
incidence_cohort <- incidence_cohort_one_prev
rm(incidence_cohort_one_prev)

# ---------- Sensitivity analysis end --------------------------------

# load helpers
source(here::here("script", "99_helpers.R"))

# ------- Helpers --------------------------------------------------------------

inc_data <- get_inc(incidence_cohort)
event_data <- get_event_dates(incidence_cohort)

# --------- Data manipulation --------------------------------------------------

# Get a list with the first three conditions for each participant
# 1 = CHD
# 2 = heart failure
# 3 = cancer
# 4 = depression
# 5 = diabetes
# 6 = parkinsonism
# 7 = dementia
# 8 = COPD
# 9 = asthma
# 10 = stroke
# 11 = death
# 12 = censored
conditions_seq <- get_conditions(inc_data = inc_data, event_data = event_data) %>% 
  mutate_all(as.numeric) %>%
  rowwise() %>%
  add_column(ergoid = incidence_cohort$ergoid) %>%
  select(a, b, c, d, ergoid) %>%
  
  # if an event is NA, it means the participant is censored
  replace(is.na(.), 0) %>%
  
  select(a, b, c, ergoid) %>%
  
  # if a or be is death, the subsequent events are death as well
  mutate(b = ifelse(a == 11, 11, b),
         c = ifelse(b == 11, 11, c),
         
         a = ifelse(a == 0, 12, a),
         b = ifelse(b == 0, 12, b),
         c = ifelse(c == 0, 12, c))


# add exit date to the event_data
event_data <- event_data %>%
  add_column(censored = incidence_cohort$exit_date)

# function to look up the event date based on the type of event
lookup_event_date <- function(row, value){
  event_data[row, value + 1]
}

# vector with relevant event dates for all participants
first_event_date <- mapply(lookup_event_date, 1:nrow(event_data), conditions_seq$a) %>% 
  do.call(c, .)

second_event_date <- mapply(lookup_event_date, 1:nrow(event_data), conditions_seq$b) %>% 
  do.call(c, .)

third_event_date <- mapply(lookup_event_date, 1:nrow(event_data), conditions_seq$c) %>% 
  do.call(c, .)

# add age at start of follow-up, first, second and third event 
age_start   <- incidence_cohort$birthd %--% incidence_cohort$latest_startd / years(1)
age_first   <- incidence_cohort$birthd %--% first_event_date / years(1)
age_second  <- incidence_cohort$birthd %--% second_event_date / years(1)
age_third   <- incidence_cohort$birthd %--% third_event_date / years(1)


# add event dates
conditions_seq <- conditions_seq %>%
  add_column(age_start = age_start,
             age_first = age_first,
             age_second = age_second,
             age_third = age_third) %>%
# add sex
  left_join(., incidence_cohort[c("sex", "ergoid")], by = "ergoid")

# .------------------------------------------------------------------------------
# ----------- Save dataframes ---------------------------------------------------
# .------------------------------------------------------------------------------
save(conditions_seq, inc_data, event_data, file = here::here("Data", "conditions_seq.RData"))


# ---------- Sensitivity analysis !! --------------------------------
  
# This is only if you want to run the sensitivity analysis. 
# Skip if running the main analysis and results.
  
# if we want to save the incidence cohort with stricter definition of depression
save(conditions_seq, inc_data, event_data, 
     file = here::here("Data", "Sensitivity-analysis", "conditions_seq_dep.RData"))

# if we want to save the incidence cohort including one prevalent case
save(conditions_seq, inc_data, event_data, 
     file = here::here("Data", "Sensitivity-analysis", "conditions_seq_one_prev.RData"))

# ---------- Sensitivity analysis end --------------------------------