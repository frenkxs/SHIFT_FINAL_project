# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
# ------------- life style data and socio-economic status ------------------------------------------
# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------


# This script takes the raw data from participants interviews and creates cleaned data that are then used to produce Table 1. All output is saved in the "Data" folder.
#  
# For all questions about it, contact Premysl Velek at p.velek@erasmusmc.nl 


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

# load in incidence cohort
load(here::here("Data", "incidence_cohort.RData"))
load(here::here("Data", "shift_data.RData"))


# .----------------------------------------------------------------------
# ------------- ERGO centre visit dates --------------------------------
# .----------------------------------------------------------------------

# Since the life style data don't contain dates when they were collected, we need to get them from
# the interview dates for each individual (sub)cohorts

# ERGO I 1
ergo_i_1_p <- here::here("Datasets", "ERGO-basic", "Ergo1ResponseDetail_(22-jan-2015)_excerpt.sav")
ergo_i_1 <- read.spss(ergo_i_1_p, to.data.frame = TRUE)

# convert dates
ergo_i_1[, c(7, 9, 11)] <- lapply(ergo_i_1[, c(7, 9, 11)] / 86400, 
                                as.Date, origin = "1582-10-14")

ergo_i_1 <- ergo_i_1[, -c(3:6, 8, 10)]

names(ergo_i_1) <- c("ergoid", "rs_cohort", "e1_int_d", "e1_visit1_d", "e1_visit2_d")

# ERGO I 2
ergo_i_2_p <- here::here("Datasets", "ERGO-basic", "Ergo2ResponseDetail_(22-jan-2015)_excerpt.sav")
ergo_i_2 <- read.spss(ergo_i_2_p, to.data.frame = TRUE)

# convert dates
ergo_i_2[, c(8, 10)] <- lapply(ergo_i_2[, c(8, 10)] / 86400, 
                               as.Date, origin = "1582-10-14")

ergo_i_2 <- ergo_i_2[, c(1, 2, 8, 10)]
names(ergo_i_2) <- c("ergoid", "rs_cohort", "e2_visit1_d", "e2_int_d")

# Since for ERGO-I-2, participants filled in a questionnaire at home, rather than doing
# home interview. 
# The forms were handed in at the research center, so there is not an exact date of the 
# interview / questionnaire. As an approximate date, we take the date of the centre visit

ergo_i_2$e2_int_d <- ergo_i_2$e2_visit1_d


# ERGO I 3
ergo_i_3_p <- here::here("Datasets", "ERGO-basic", "e3_(3)_RESPONS_(22-feb-2016)_excerpt.sav")
ergo_i_3 <- read.spss(ergo_i_3_p, to.data.frame = TRUE)

# convert dates
ergo_i_3[, c(7, 9, 12)] <- lapply(ergo_i_3[, c(7, 9, 12)] / 86400, 
                                  as.Date, origin = "1582-10-14")

ergo_i_3 <- ergo_i_3[, c(1, 2, 7, 9, 12)]
names(ergo_i_3) <- c("ergoid", "rs_cohort", "e3_int_d", "e3_visit1_d", "e3_visit2_d")

# ERGO I 4 (including ERGO-II-2)
ergo_i_4_p <- here::here("Datasets", "ERGO-basic", "e4_(4)_RESPONS_(12-mar-2018)_excerpt.sav")
ergo_i_4 <- read.spss(ergo_i_4_p, to.data.frame = TRUE)

# convert dates
ergo_i_4[, c(7, 9, 11)] <- lapply(ergo_i_4[, c(7, 9, 11)] / 86400, 
                                  as.Date, origin = "1582-10-14")

ergo_i_4 <- ergo_i_4[, c(1, 2, 7, 9, 11)]
names(ergo_i_4) <- c("ergoid", "rs_cohort", "e4_int_d", "e4_visit1_d", "e4_visit2_d")

# Since this comprises two cohorts, it will be treated separately from the rest of ERGO-I visit dates.
# It will be left-joined directly to the smoking dataset

# ERGO II (ERGOPLUS)
ergo_ii_p <- here::here("Datasets", "ERGO-basic", "ep_(1)_RESPONS_(15-jan-2019)_excerpt.sav")
ergo_ii <- read.spss(ergo_ii_p, to.data.frame = TRUE)

# convert dates
ergo_ii[, c(7, 9, 11)] <- lapply(ergo_ii[, c(7, 9, 11)] / 86400, 
                                 as.Date, origin = "1582-10-14")

ergo_ii <- ergo_ii[, -c(3:6, 8, 10)]
names(ergo_ii) <- c("ergoid", "rs_cohort", "ep_int_d", 
                    "ep_visit1_d", "ep_visit2_d")

# ERGO III (ERGOJONG)
ergo_iii_p <- here::here("Datasets", "ERGO-basic", "ej_(1)_RESPONS_(04-apr-2016)_excerpt.sav")
ergo_iii <- read.spss(ergo_iii_p, to.data.frame = TRUE)

# convert dates
ergo_iii[, c(7, 9, 11)] <- lapply(ergo_iii[, c(7, 9, 11)] / 86400, 
                                  as.Date, origin = "1582-10-14")

ergo_iii <- ergo_iii[, -c(3:6, 8, 10)]
names(ergo_iii) <- c("ergoid", "rs_cohort", "ej_int_d", "ej_visit1_d", "ej_visit2_d")


# join RS-I dates
ergo_visit_dates <- list(ergo_i_1, ergo_i_2 %>% select(-rs_cohort), 
                                   ergo_i_3  %>% select(-rs_cohort)) %>%
  reduce(left_join, by = "ergoid")

# join e1-3 and e4 
ergo_visit_dates <- left_join(ergo_i_4 %>% select(-rs_cohort),
                                ergo_visit_dates, by = "ergoid")

# join with ep
ergo_visit_dates <- left_join(ergo_visit_dates,
                              ergo_ii %>% select(-rs_cohort), by = "ergoid")

# join with ej
ergo_visit_dates <- bind_rows(ergo_visit_dates, ergo_iii)

ergo_visit_dates <- ergo_visit_dates[, c(1, 6:13, 2:4, 14:19)] 

# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
# -------- Smoking status --------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------


# ERGO-I
smoking_ergo_i_1_p <- here::here("Datasets", "Life-style", "smoking", 
                                 "e1_intvw_SMOKING_(23-nov-2011).sav")
smoking_ergo_i_2_p <- here::here("Datasets", "Life-style", "smoking", 
                                 "e2_intvw_SMOKING_(23-nov-2011).sav")
smoking_ergo_i_3_p <- here::here("Datasets", "Life-style", "smoking", 
                                 "e3_intvw_SMOKING_(11-nov-2011).sav")
smoking_ergo_i_4_p <- here::here("Datasets", "Life-style", "smoking", 
                                 "e4_intvw_SMOKING_(04-nov-2011).sav")

smoking_ergo_i_1 <- read.spss(smoking_ergo_i_1_p, to.data.frame = TRUE)
smoking_ergo_i_2 <- read.spss(smoking_ergo_i_2_p, to.data.frame = TRUE)
smoking_ergo_i_3 <- read.spss(smoking_ergo_i_3_p, to.data.frame = TRUE)
smoking_ergo_i_4 <- read.spss(smoking_ergo_i_4_p, to.data.frame = TRUE)

# get smoking status for each interview, we therefore have three columns: 1) ergoid, 2) interview date
# and 3) smoking status (never, former, current)

# ERGO-I-1
smoker_ergo_i_1 <- data.frame(ergoid = smoking_ergo_i_1$ergoid, 
                              current = smoking_ergo_i_1$e1_ai7_20,
                              former = smoking_ergo_i_1$e1_ai7_30) %>%
  mutate(smoker = case_when((current == "no" & former == "no") ~ "never",
                            (current == "no" & former == "yes") ~ "former",
                            (current == "yes" & former == "8") ~ "current", 
                            TRUE ~ NA_character_)) %>%
  left_join(., ergo_i_1 %>% dplyr::select(ergoid, e1_int_d), by = "ergoid") %>%
  dplyr::select(-c(current, former)) %>%
  rename(int1_d = e1_int_d)


# ERGO-I-2
smoker_ergo_i_2 <- data.frame(ergoid = smoking_ergo_i_2$ergoid, 
                              cigars = smoking_ergo_i_2$e2_b0cg,
                              pipe = smoking_ergo_i_2$e2_b0pi,
                              cigarettes = smoking_ergo_i_2$e2_b0ct) %>%
  mutate(cigars = case_when(str_detect(cigars, "never") ~ "never",
                            str_detect(cigars, "but") ~ "former",
                            str_detect(cigars, "still") ~ "current", 
                            TRUE ~ NA_character_),
         
         pipe =   case_when(str_detect(pipe, "never") ~ "never",
                            str_detect(pipe, "but") ~ "former",
                            str_detect(pipe, "still") ~ "current", 
                            TRUE ~ NA_character_),
         
         cigarettes = case_when(str_detect(cigarettes, "never") ~ "never",
                                str_detect(cigarettes, "but") ~ "former",
                                str_detect(cigarettes, "still") ~ "current", 
                                TRUE ~ NA_character_)) %>%
  
  mutate(smoker = paste(cigars, pipe, cigarettes, sep = "_")) %>%
  
  mutate(smoker = case_when(str_detect(smoker, "current") ~ "current",
                            str_detect(smoker, "former") & !str_detect(smoker, "current") ~ "former",
                            smoker == "never_never_never" ~ "never", 
                            TRUE ~ NA_character_)) %>%
  
  left_join(., ergo_i_2 %>% dplyr::select(ergoid, e2_int_d), by = "ergoid") %>%
  
  dplyr::select(-c(cigars, pipe, cigarettes)) %>%
  rename(int2_d = e2_int_d)


# ERGO-I-3 
smoker_ergo_i_3 <- data.frame(ergoid =          smoking_ergo_i_3$ergoid, 
                              cigars =          smoking_ergo_i_3$e3_cicg,
                              pipe =            smoking_ergo_i_3$e3_cipi,
                              cigarettes_pres = smoking_ergo_i_3$e3_cict,
                              cigarettes_past = smoking_ergo_i_3$e3_cictps) %>% 
  mutate(cigars = case_when(str_detect(cigars, "nooit") ~ "never",
                            str_detect(cigars, "gestopt") ~ "former",
                            str_detect(cigars, "ja") ~ "current", 
                            TRUE ~ NA_character_),
         
         pipe =   case_when(str_detect(pipe, "nooit") ~ "never",
                            str_detect(pipe, "gestopt") ~ "former",
                            str_detect(pipe, "ja") ~ "current", 
                            TRUE ~ NA_character_),
         
         cigarettes = case_when(str_detect(cigarettes_pres, "nee") & 
                                  str_detect(cigarettes_past, "nee") ~ "never",
                                str_detect(cigarettes_past, "ja") ~ "former",
                                str_detect(cigarettes_pres, "ja") ~ "current", 
                                TRUE ~ NA_character_)) %>%
  
  mutate(smoker = paste(cigars, pipe, cigarettes, sep = "_")) %>%
  
  mutate(smoker = case_when(str_detect(smoker, "current") ~ "current",
                            str_detect(smoker, "former") & !str_detect(smoker, "current") ~ "former",
                            smoker == "never_never_never" ~ "never", 
                            TRUE ~ NA_character_)) %>%
  
  left_join(., ergo_i_3 %>% dplyr::select(ergoid, e3_int_d), by = "ergoid") %>%
  
  dplyr::select(-c(cigars, pipe, cigarettes, cigarettes_past, cigarettes_pres)) %>%
  rename(int3_d = e3_int_d)


smoking_status_ergo_i <- list(smoker_ergo_i_1, smoker_ergo_i_2, smoker_ergo_i_3) %>%
  reduce(left_join, by = "ergoid", suffix = c(".1", ".2"))

names(smoking_status_ergo_i)[6] <- "smoker.3"

# ERGO-I-4 (comprises also ERGO-II-2)
smoking_ergo_i_4_p <- here::here("Datasets", "Life-style", "smoking", 
                                 "e4_intvw_SMOKING_(04-nov-2011).sav")

smoking_ergo_i_4 <- read.spss(smoking_ergo_i_4_p, to.data.frame = TRUE)

smoking_status_ergo_i_4_ii_2 <- data.frame( ergoid =          smoking_ergo_i_4$ergoid, 
                                            rs_cohort =       smoking_ergo_i_4$rs_cohort,
                                            cigars =          smoking_ergo_i_4$e4_dicg,
                                            pipe =            smoking_ergo_i_4$e4_dipi,
                                            cigarettes      = smoking_ergo_i_4$e4_dict) %>%
  mutate(cigars = case_when(cigars == "nee, nooit sigaren gerookt" ~ "never",
                            cigars == "nee, ik ben gestopt" ~ "former",
                            str_detect(cigars, "ja") ~ "current", 
                            TRUE ~ NA_character_),
         
         pipe =   case_when(pipe == "nee, nooit pijp gerookt" ~ "never",
                            pipe == "nee, ik ben gestopt" ~ "former",
                            str_detect(pipe, "ja") ~ "current", 
                            TRUE ~ NA_character_),
         
         cigarettes = case_when(cigarettes == "nee, nooit gerookt" ~ "never",
                                cigarettes == "ja" ~ "current",
                                str_detect(cigarettes, "gestopt") ~ "former", 
                                TRUE ~ NA_character_)) %>%
  
  mutate(smoker = paste(cigars, pipe, cigarettes, sep = "_")) %>%
  
  mutate(smoker = case_when(str_detect(smoker, "current") ~ "current",
                            str_detect(smoker, "former") & !str_detect(smoker, "current") ~ "former",
                            smoker == "never_never_never" ~ "never", 
                            TRUE ~ NA_character_)) %>%
  
  left_join(., ergo_i_4 %>% dplyr::select(ergoid, e4_int_d), by = "ergoid") %>%
  
  dplyr::select(-c(cigars, pipe, cigarettes)) %>%
  rename(int4_d = e4_int_d)

# split the data into separate cohorts (RS-I and RS-II)
smoking_status_ergo_i_4 <- smoking_status_ergo_i_4_ii_2 %>%
  filter(rs_cohort == "RS-I") %>%
  rename(smoker.4 = smoker)


smoking_status_ergo_ii_2 <- smoking_status_ergo_i_4_ii_2 %>%
  filter(rs_cohort == "RS-II") %>%
  rename(smoker.2 = smoker,
         int2_d = int4_d)

smoking_status_ergo_i <- left_join(smoking_status_ergo_i, smoking_status_ergo_i_4) %>%
  dplyr::select(-rs_cohort)

# ERGO-II
smoking_ergo_ii_p <-  here::here("Datasets", "Life-style", "smoking", 
                                 "ep_intvw_SMOKING_(30-sep-2011).sav")
smoking_ergo_ii <- read.spss(smoking_ergo_ii_p, to.data.frame = TRUE)


smoking_status_ergo_ii_1 <- data.frame( ergoid =          smoking_ergo_ii$ergoid, 
                                        cigars =          smoking_ergo_ii$ep_lf4,
                                        pipe =            smoking_ergo_ii$ep_lf5,
                                        cigarettes_pres = smoking_ergo_ii$ep_lf6,
                                        cigarettes_past = smoking_ergo_ii$ep_lf6e) %>%
  mutate(cigars = case_when(cigars == "nee, nooit" ~ "never",
                            cigars == "nee, ooit" ~ "former",
                            cigars == "ja" ~ "current", 
                            TRUE ~ NA_character_),
         
         pipe =   case_when(pipe == "nee, nooit pijp gerookt" ~ "never",
                            pipe == "nee, ik ben gestopt" ~ "former",
                            pipe == "ja" ~ "current", 
                            TRUE ~ NA_character_),
         
         cigarettes = case_when(cigarettes_pres == "nee" & 
                                  cigarettes_past == "nee" ~ "never",
                                cigarettes_past == "ja" ~ "former",
                                cigarettes_pres == "ja" ~ "current", 
                                TRUE ~ NA_character_)) %>%
  
  mutate(smoker = paste(cigars, pipe, cigarettes, sep = "_")) %>%
  
  mutate(smoker = case_when(str_detect(smoker, "current") ~ "current",
                            str_detect(smoker, "former") & !str_detect(smoker, "current") ~ "former",
                            smoker == "never_never_never" ~ "never", 
                            TRUE ~ NA_character_)) %>%
  
  left_join(., ergo_ii %>% dplyr::select(ergoid, ep_int_d), by = "ergoid") %>%
  
  dplyr::select(-c(cigars, pipe, cigarettes, cigarettes_past, cigarettes_pres))

smoking_status_ergo_ii <- left_join(smoking_status_ergo_ii_1, smoking_status_ergo_ii_2, 
                                    by = "ergoid") %>%
  dplyr::select(-rs_cohort)

names(smoking_status_ergo_ii)[2:3] <- c("smoker.1", "int1_d")   

# ERGO-III
smoking_ergo_iii_p <- here::here("Datasets", "Life-style", "smoking", 
                                  "ej_intvw_SMOKING_(28-mar-2011).sav")

# reading in the file with foreign package throws an error
require("haven") 
smoking_ergo_iii <- read_spss(smoking_ergo_iii_p)

smoking_status_ergo_iii <- data.frame(ergoid =          smoking_ergo_iii$ergoid, 
                                      cigars =          smoking_ergo_iii$ej_yilf4,
                                      pipe =            smoking_ergo_iii$ej_yilf5,
                                      cigarettes_pres = smoking_ergo_iii$ej_yilf6,
                                      cigarettes_past = smoking_ergo_iii$ej_yilfe) %>%
  mutate(cigars = case_when(cigars == 0 ~ "never",
                            cigars %in% c(1:3) ~ "former",
                            cigars %in% c(4:6) ~ "current", 
                            TRUE ~ NA_character_),
         
         pipe =   case_when(pipe == 0 ~ "never",
                            pipe == 1 ~ "former",
                            pipe == 2 ~ "current", 
                            TRUE ~ NA_character_),
         
         cigarettes = case_when(cigarettes_pres == 0 & 
                                  cigarettes_past == 0 ~ "never",
                                cigarettes_past == 1 ~ "former",
                                cigarettes_pres == 1 ~ "current", 
                                TRUE ~ NA_character_)) %>%
  
  mutate(smoker.1 = paste(cigars, pipe, cigarettes, sep = "_")) %>%
  
  mutate(smoker.1 = case_when(str_detect(smoker.1, "current") ~ "current",
                              str_detect(smoker.1, "former") & !str_detect(smoker.1, "current") ~ "former",
                              smoker.1 == "never_never_never" ~ "never", 
                              TRUE ~ NA_character_)) %>%
  
  left_join(., ergo_iii %>% dplyr::select(ergoid, ej_int_d), by = "ergoid") %>%
  
  dplyr::select(-c(cigars, pipe, cigarettes, cigarettes_past, cigarettes_pres)) %>%
  rename(int1_d = ej_int_d)

# merge all smoking datasets together

smoking <- bind_rows(smoking_status_ergo_i, 
                     smoking_status_ergo_ii,
                     smoking_status_ergo_iii)

save(smoking, file = here::here("Data", "smoking.Rdata"))


# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
# -------- Marital status --------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------

# ERGO I 1
marital_ergo_i_1_p <- here::here("Datasets", "Life-style", "marital_status", 
                                "e1_intvw_GENERAL_(16-jan-2012).sav")

marital_ergo_i_1 <- read.spss(marital_ergo_i_1_p, to.data.frame = TRUE)

marital_ergo_i_1 <- marital_ergo_i_1 %>%
  select(ergoid, e1_ai0_16) %>%
  rename(with_partner = e1_ai0_16) %>%
  mutate(with_partner = case_when(with_partner == "with partner" ~ TRUE,
                                  with_partner == "missing" ~ NA,
                                  TRUE ~ FALSE)) %>%
  left_join(., ergo_i_1 %>% dplyr::select(ergoid, e1_int_d), by = "ergoid") %>%
  rename(int1_d = e1_int_d)

# ERGO I 3
marital_ergo_i_3_p <- here::here("Datasets", "Life-style", "marital_status", 
                                 "e3_intvw_GENERAL_(08-nov-2011).sav")
marital_ergo_i_3 <- read.spss(marital_ergo_i_3_p, to.data.frame = TRUE)

marital_ergo_i_3 <- marital_ergo_i_3 %>%
  dplyr::select(ergoid, e3_cihomesh) %>%
  rename(with_partner = e3_cihomesh) %>%
  mutate(with_partner = case_when(with_partner == "samen met partner" ~ TRUE,
                                  is.na(with_partner) ~ NA,
                                  TRUE ~ FALSE)) %>%
  left_join(., ergo_i_3 %>% dplyr::select(ergoid, e3_int_d), by = "ergoid") %>%
  rename(int2_d = e3_int_d)

marital_ergo_i <- left_join(marital_ergo_i_1, marital_ergo_i_3, by = "ergoid", suffix = c(".1", ".2"))

# ERGO I 4
marital_ergo_i_4_p <- here::here("Datasets", "Life-style", "marital_status", 
                                 "e4_intvw_GENERAL_(04-nov-2011).sav")
marital_ergo_i_4 <- read.spss(marital_ergo_i_4_p, to.data.frame = TRUE)

marital_ergo_i_4_ii_2 <- marital_ergo_i_4 %>%
  dplyr::select(ergoid, e4_dipar, rs_cohort) %>%
  rename(with_partner = e4_dipar) %>%
  mutate(with_partner = case_when(with_partner == "ja, een partner waarmee ik samenwoon" ~ TRUE,
                                  with_partner == "geen antwoord" |  with_partner == "8" ~ NA,
                                  is.na(with_partner) ~ NA,
                                  TRUE ~ FALSE)) %>%
  left_join(., ergo_i_4 %>% dplyr::select(ergoid, e4_int_d), by = "ergoid") %>%
  rename(int4_d = e4_int_d)

# split between RS-I and RS-II
marital_ergo_i_4 <- marital_ergo_i_4_ii_2 %>%
  filter(rs_cohort == "RS-I") %>%
  dplyr::select(-rs_cohort) %>%
  rename(with_partner.3 = with_partner,
         int3_d = int4_d)


marital_ergo_i <- left_join(marital_ergo_i, marital_ergo_i_4)

marital_ergo_ii_2 <- marital_ergo_i_4_ii_2 %>%
  filter(rs_cohort == "RS-II") %>%
  dplyr::select(-rs_cohort) %>%
  rename(with_partner.2 = with_partner,
         int2_d = int4_d)

# ERGO II
marital_ergo_ii_p <- here::here("Datasets", "Life-style", "marital_status", 
                                "ep_intvw_GENERAL_(27-oct-2011).sav")
marital_ergo_ii <- read.spss(marital_ergo_ii_p, to.data.frame = TRUE)

marital_ergo_ii_1 <- marital_ergo_ii %>%
  dplyr::select(ergoid, ep_pid6) %>%
  rename(with_partner.1 = ep_pid6) %>%
  mutate(with_partner.1 = case_when(with_partner.1 == "samen met echtgenoot/partner (2)" ~ TRUE,
                                   with_partner.1 == "samen met echtgenoot/partner en kinderen" ~ TRUE,
                                   with_partner.1 == "99" ~ NA,
                                   is.na(with_partner.1) ~ NA,
                                   TRUE ~ FALSE)) %>%
  left_join(., ergo_ii %>% dplyr::select(ergoid, ep_int_d), by = "ergoid") %>%
  rename(int1_d = ep_int_d)

marital_ergo_ii <- left_join(marital_ergo_ii_1, marital_ergo_ii_2)


# ERGO III
marital_ergo_iii_p <- here::here("Datasets", "Life-style", "marital_status", 
                                 "ej_intvw_GENERAL_(23-mar-2011).sav")
marital_ergo_iii <- read.spss(marital_ergo_iii_p, to.data.frame = TRUE)

marital_ergo_iii <- marital_ergo_iii %>%
  dplyr::select(ergoid, ej_intdat, ej_yihomew) %>%
  rename(with_partner.1 = ej_yihomew,
         int1_d = ej_intdat) %>%
  mutate(int1_d = as.Date(int1_d / 86400, origin = "1582-10-14")) %>%
  mutate(with_partner.1 = case_when(with_partner.1 == "met echtgenoot/partner" ~ TRUE,
                                   with_partner.1 == "8" ~ NA,
                                   is.na(with_partner.1) ~ NA,
                                   TRUE ~ FALSE)) 

marital <- bind_rows(marital_ergo_i,
                     marital_ergo_ii,
                     marital_ergo_iii)  
  
save(marital, file = here::here("Data", "marital.Rdata"))


# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
# -------- Education -------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------

education_p <- here::here("Datasets", "Life-style", "education",
                          "Education RS-I-II-III (UNESCO class)_(12-MAR-2015).sav")
education <- read.spss(education_p, to.data.frame = TRUE)


education <- education %>%
  rename(level = ses_UNESCO_recoded) %>%
  mutate(level = case_when(level == "primary education" ~ "Primary",
                           level == "lower/intermediate general education OR lower vocational education" 
                           ~ "Lower",
                           level == "intermediate vocational education OR higher general education" 
                           ~ "Further",
                           level == "higher vocational education OR university" ~ "Higher",
                           TRUE ~ NA_character_)) %>%
  select(-rs_cohort)

save(education, file = here::here("Data", "education.Rdata"))


# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
# -------- HYpertension ----------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------

ht_p <- here::here("Datasets", "Life-style", "blood-pressure", 
                   "HT2018_analysisfile_(15-may-2018).sav")
ht <- read.spss(ht_p, to.data.frame = TRUE)

# select BP measurement
bp <- ht %>%
  dplyr::select(contains("stolicBP"), ergoid) %>%
  dplyr::select(-matches("e5|e6")) %>%
  left_join(., ergo_visit_dates, by = "ergoid")

save(bp, file = here::here("Data", "blood_pressure.Rdata"))


# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
# -------- Ancestry  ----------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
# .--------------------------------------------------------------------------------------------------
anc_i_p <-    here::here("Datasets", "Life-style", "ethnicity", 
                         "RS_I_GeneticEtnicity_(11-JAN-2016).sav")
anc_ii_p <-   here::here("Datasets", "Life-style", "ethnicity", 
                         "RS_II_GeneticEtnicity_(11-JAN-2016).sav")
anc_iii_p <-  here::here("Datasets", "Life-style", "ethnicity", 
                         "RS_III_GeneticEtnicity_(11-JAN-2016).sav")

anc_i <-   read.spss(anc_i_p, to.data.frame = TRUE)
anc_ii <-  read.spss(anc_ii_p, to.data.frame = TRUE)
anc_iii <- read.spss(anc_iii_p, to.data.frame = TRUE)

anc <- bind_rows(anc_i %>% select(ergoid, ancestry),
                 anc_ii %>% select(ergoid, ancestry),
                 anc_iii %>% select(ergoid, ancestry))

anc$ergoid <- as.factor(anc$ergoid)

# add sex
anc <- left_join(shift_data %>% select(ergoid), anc, by = "ergoid")

save(anc, file = here::here("Data", "ancestry.Rdata"))
