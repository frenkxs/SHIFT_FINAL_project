# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- Lifetime risk of multimorbidity: sensitivity analysis ------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# It's the same lifetime risk analysis, but this time with changes how the diseases are grouped. 


# .-------------------------------------------------------------------------------
# ------------------------ Preliminaries ----------------------------------------
# .--------------------------------------------------------------------------------

# libraries needed in this script
packages_needed <- c("foreign", "tidyverse", "lubridate", "here", "grid", "gridExtra",
                     "colorspace", "cowplot", "etm", "survival", "ragg")

# install missing libraries
install.packages(setdiff(packages_needed, rownames(installed.packages())))  

# load libraries
require(colorspace)
require(foreign)
require(tidyverse)
require(lubridate)
require(cowplot)
require(etm)
require(survival)
require(ragg)

# start here
setwd(here::here())



# .-----------------------------------------------------------------------------
# -------------  Grouping together dementia and depression ---------------------
# .-----------------------------------------------------------------------------

load("script/data/incidence_cohort.RData")
load("script/data/conditions_seq.RData")

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

# Add an event variable to code for different combinations of medical conditions
# The combination of dementia and depression will be classified as 'with mental health'

# 0 = censored
# 1 = with only physical diseases
# 2 = with mental health diseases
# 3 = death before first or second event

event_type <- conditions_seq %>%
  # if first event is death, the second event is also death (coded 11)
  mutate(b = ifelse(a == 11, 11, b)) %>%
  
  # concatenate the two events
  mutate(event = paste(a, b, sep = "," )) %>%
  
  # make it a vector
  mutate(event = str_split(event, pattern = ",")) %>%
  mutate(event = list(as.numeric(event))) %>%
  
  # code concordant somatic diseases
  mutate(event = case_when(
                            # if second event is censoring => censored
                            b == 12 ~ 0,
                            
                           # if first of second event is depression or dementia => mental
                           (b == 4) ~ 2,
                           (b == 7) ~ 2,
                           
                           a == 4 & !(b %in% c(11, 12)) ~ 2, 
                           a == 7 & !(b %in% c(11, 12)) ~ 2, 
                           
                           # if second event is death => death before second event 
                           b == 11 ~ 3,
                           
                           # for everything else => somatic discordant
                           TRUE ~ 1)) %>%
  pull(event)



# finally create a dataframe with event type, age of entry and age of event
multimorb_risk_mh <- data.frame(ergoid = conditions_seq$ergoid, 
                             age_start = conditions_seq$age_start,
                             age_censor = conditions_seq$age_second,
                             sex = conditions_seq$sex,
                             event_type = factor(event_type))



# ggtransfo was removed from etm, hence a custom function to get data from etmCIF to
# ggplot  friendly format
etm_to_df <- function(x){
  x <- summary(x)
  
  # only interested in events other than death
  len <- length(x[[1]]) - 1 
  res <- data.frame(time = NA, P = NA, lower = NA, upper = NA, type = NA)
  
  for(i in 1:len){
    ev <- x[[1]][[i]]
    ev$type <- rep(i, nrow(ev))
    res <- bind_rows(res, ev)
  }
  res
}


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------ Plotting: men and women separately ------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# .. Men -----------
multimorb_risk_mh_men <- multimorb_risk_mh %>%
  filter(sex == "male")

cif_multimorb_mh_men <- etmCIF(Surv(age_start, age_censor, event_type != 0) ~ 1, 
                            data = multimorb_risk_mh_men, etype = event_type)


gg_multimorb_mh_men <- etm_to_df(cif_multimorb_mh_men)
gg_multimorb_mh_men$type <- factor(gg_multimorb_mh_men$type, levels = c(1, 2),
                                labels = c("With physical diseases only",
                                           "With mental health disease(s)"))

gg_multimorb_mh_men <- na.omit(gg_multimorb_mh_men)
gg_multimorb_mh_men$sex <- "men"


# .. Women --------
multimorb_risk_mh_women <- multimorb_risk_mh %>%
  filter(sex == "female")

cif_multimorb_mh_women <- etmCIF(Surv(age_start, age_censor, event_type != 0) ~ 1, 
                              data = multimorb_risk_mh_women, etype = event_type)


gg_multimorb_mh_women <- etm_to_df(cif_multimorb_mh_women)
gg_multimorb_mh_women$type <- factor(gg_multimorb_mh_women$type, levels = c(1, 2),
                                     labels = c("With physical diseases only",
                                                "With mental health disease(s)"))

gg_multimorb_mh_women <- na.omit(gg_multimorb_mh_women)
gg_multimorb_mh_women$sex <- "women"

# .. Merge the two datasets ------------
gg_multimorb_mh <- bind_rows(gg_multimorb_mh_women, gg_multimorb_mh_men)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# .. Plotting ------------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

palette <- swatchplot(sequential_hcl(2, "Blues", l2 = 80, l1 = 20))

frmt <- list(scale_fill_discrete_sequential("Blues", l2 = 80, l1 = 20,
                                            name = "Multimorbidity type"),
             theme_minimal(base_size = 22),
             labs(x = "Age (years)", y = "Cumulative incidence (%)"),
             scale_x_continuous(limits = c(45, 100), breaks = seq(45, 100, by = 10)),
             scale_y_continuous(labels = scales::label_percent(suffix = "", accuracy = 1),
                                breaks = seq(0.1, 0.8, by = 0.1)),
             labs(title = "Lifetime risk of multimorbidity for men and women over 45 years"),
             theme(legend.position = c(0.65, 0.8),
                   legend.text = element_text(size = 13),
                   legend.background = element_rect(fill = "white", colour = "white"),
                   panel.grid.minor.y = element_blank(),
                   axis.text = element_text(size = 14),
                   legend.title = element_text(size = 16))
)


ggplot(gg_multimorb_mh, aes(x = time, y = P, group = type, fill = type)) +
  geom_area() +
  facet_wrap(~ sex) +
  frmt  


# export figure ---------
ragg::agg_png("paper/figures/Fig_4_SA_mh.png", width = 3800, height = 2500, units = "px", res = 300, scaling = 1.1)
ggplot(gg_multimorb_mh, aes(x = time, y = P, group = type, fill = type)) +
  geom_area() +
  facet_wrap(~ sex) +
  frmt  
dev.off()


# get percentages for the results section:

# at 65 years 
men_65 <- min(gg_multimorb$time[gg_multimorb$time > 65 & gg_multimorb$sex == "men"])
women_65 <- min(gg_multimorb$time[gg_multimorb$time > 65 & gg_multimorb$sex == "women"])

round(gg_multimorb[gg_multimorb$time == men_65 & gg_multimorb$sex == "men", "P"], 3)
round(gg_multimorb[gg_multimorb$time == women_65 & gg_multimorb$sex == "women", "P"], 3)

# at 99 years 
men_99 <- min(gg_multimorb_mh$time[gg_multimorb_mh$time > 99 & gg_multimorb_mh$sex == "men"])
women_99 <- min(gg_multimorb_mh$time[gg_multimorb_mh$time > 99 & gg_multimorb_mh$sex == "women"])

gg_multimorb_mh[gg_multimorb_mh$time == men_99 & 
               gg_multimorb_mh$sex == "men", c("P", "upper", "lower", "type")]
gg_multimorb_mh[gg_multimorb_mh$time == women_99 & 
               gg_multimorb_mh$sex == "women", c("P", "upper", "lower", "type")]


# .-----------------------------------------------------------------------------
# -------------  Grouping together stroke, parkinsonism and dementia -----------
# .-----------------------------------------------------------------------------
 
rm(list = ls())

load("script/data/incidence_cohort.RData")
load("script/data/conditions_seq.RData")

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

# Add an event variable to code for different combinations of medical conditions
# The combination of dementia and depression will be classified as 'with mental health'

# 0 = censored
# 1 = somatic - somatic (discordant)
# 2 = somatic - somatic (concordant)
# 3 = somatic - psychosocial
# 4 = death before first or second event

event_type <- conditions_seq %>%
  # if first event is death, the second event is also death (coded 11)
  mutate(b = ifelse(a == 11, 11, b)) %>%
  
  # concatenate the two events
  mutate(event = paste(a, b, sep = "," )) %>%
  
  # make it a vector
  mutate(event = str_split(event, pattern = ",")) %>%
  mutate(event = list(as.numeric(event))) %>%
  
  # code concordant somatic diseases
  mutate(event = case_when(setequal(c(1, 2),   event) ~ 2,
                           setequal(c(6, 7),   event) ~ 2,
                           setequal(c(6, 10),  event) ~ 2,
                           setequal(c(7, 10),  event) ~ 2,
                           setequal(c(8, 9),   event) ~ 2,
                           
                           
                           # if first or second event is depression => psychosocial
                           b == 4 ~ 3,
                           a == 4 & !(b %in% c(11, 12)) ~ 3, 
                           
                           # if second event is death => death before second event 
                           b == 11 ~ 4,
                           
                           # if second event is censoring => censored
                           b == 12 ~ 0,
                           
                           # for everything else => somatic discordant
                           TRUE ~ 1)) %>%
  pull(event)



# finally create a dataframe with event type, age of entry and age of event
multimorb_risk <- data.frame(ergoid = conditions_seq$ergoid, 
                                age_start = conditions_seq$age_start,
                                age_censor = conditions_seq$age_second,
                                sex = conditions_seq$sex,
                                event_type = factor(event_type))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------ Plotting: men and women separately ------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# .. Men -----------
multimorb_risk_men <- multimorb_risk %>%
  filter(sex == "male")

cif_multimorb_men <- etmCIF(Surv(age_start, age_censor, event_type != 0) ~ 1, 
                            data = multimorb_risk_men, etype = event_type)


gg_multimorb_men <- etm_to_df(cif_multimorb_men)
gg_multimorb_men$type <- factor(gg_multimorb_men$type, levels = c(1, 2, 3),
                                labels = c("Discordant somatic",
                                           "Concordant somatic",
                                           "Somatic and psychiatric"))

gg_multimorb_men <- na.omit(gg_multimorb_men)
gg_multimorb_men$sex <- "men"


# .. Women --------
multimorb_risk_women <- multimorb_risk %>%
  filter(sex == "female")

cif_multimorb_women <- etmCIF(Surv(age_start, age_censor, event_type != 0) ~ 1, 
                              data = multimorb_risk_women, etype = event_type)


gg_multimorb_women <- etm_to_df(cif_multimorb_women)
gg_multimorb_women$type <- factor(gg_multimorb_women$type, levels = c(1, 2, 3),
                                  labels = c("Discordant somatic",
                                             "Concordant somatic",
                                             "Somatic and psychiatric"))

gg_multimorb_women <- na.omit(gg_multimorb_women)
gg_multimorb_women$sex <- "women"

# .. Merge the two datasets ------------
gg_multimorb <- bind_rows(gg_multimorb_women, gg_multimorb_men)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# .. Plotting ------------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

palette <- swatchplot(sequential_hcl(3, "Blues", l2 = 80, l1 = 20))

frmt <- list(scale_fill_discrete_sequential("Blues", l2 = 80, l1 = 20,
                                            name = "Multimorbidity type"),
             theme_minimal(base_size = 22),
             labs(x = "Age (years)", y = "Cumulative incidence (%)"),
             scale_x_continuous(limits = c(45, 100), breaks = seq(45, 100, by = 10)),
             scale_y_continuous(labels = scales::label_percent(suffix = "", accuracy = 1),
                                breaks = seq(0.1, 0.8, by = 0.1)),
             labs(title = "Lifetime risk of multimorbidity for men and women over 45 years"),
             theme(legend.position = c(0.65, 0.8),
                   legend.text = element_text(size = 13),
                   legend.background = element_rect(fill = "white", colour = "white"),
                   panel.grid.minor.y = element_blank(),
                   axis.text = element_text(size = 14),
                   legend.title = element_text(size = 16))
)


ggplot(gg_multimorb, aes(x = time, y = P, group = type, fill = type)) +
  geom_area() +
  facet_wrap(~ sex) +
  frmt  


# export figure ---------
ragg::agg_png("paper/figures/Fig_4_SA_neuro.png", width = 3800, height = 2500, units = "px", res = 300, scaling = 1.1)
ggplot(gg_multimorb, aes(x = time, y = P, group = type, fill = type)) +
  geom_area() +
  facet_wrap(~ sex) +
  frmt  
dev.off()


# get percentages for the results section:

# at 65 years 
men_65 <- min(gg_multimorb$time[gg_multimorb$time > 65 & gg_multimorb$sex == "men"])
women_65 <- min(gg_multimorb$time[gg_multimorb$time > 65 & gg_multimorb$sex == "women"])

round(gg_multimorb[gg_multimorb$time == men_65 & gg_multimorb$sex == "men", "P"], 3)
round(gg_multimorb[gg_multimorb$time == women_65 & gg_multimorb$sex == "women", "P"], 3)

gg_multimorb[gg_multimorb$time == men_65 & 
               gg_multimorb$sex == "men", c("P", "upper", "lower", "type")]
gg_multimorb[gg_multimorb$time == women_65 & 
               gg_multimorb$sex == "women", c("P", "upper", "lower", "type")]


# at 99 years 
men_99 <- min(gg_multimorb$time[gg_multimorb$time > 99 & gg_multimorb$sex == "men"])
women_99 <- min(gg_multimorb$time[gg_multimorb$time > 99 & gg_multimorb$sex == "women"])

gg_multimorb[gg_multimorb$time == men_99 & 
               gg_multimorb$sex == "men", c("P", "upper", "lower", "type")]
gg_multimorb[gg_multimorb$time == women_99 & 
               gg_multimorb$sex == "women", c("P", "upper", "lower", "type")]


