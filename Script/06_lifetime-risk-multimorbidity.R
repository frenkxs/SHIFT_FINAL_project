# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT - Lifetime risk of multimorbidity -------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# .This script computes the lifetime risk of multimorbidity in a competing risk framework
# For all questions about it, contact Premysl Velek at p.velek@erasmusmc.nl 


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

# load in incidence cohort and sequence data
load(here::here("Data", "incidence_cohort.RData"))
load(here::here("Data", "conditions_seq.RData"))

# ---------- Sensitivity analysis !! --------------------------------

# This is only if you want to run the sensitivity analysis either with
# stricter definition of a case for depression or with participants with one
# prevalent cases included.
# 
# Skip if running the main analysis and results.

# if we want to use the data with stricter definition for depression
load(here::here("Data", "Sensitivity-analysis", "incidence_cohort_dep.RData"))
load(here::here("Data", "Sensitivity-analysis", "conditions_seq_dep.RData"))

# if we want to use the data that include one prevalent case
load(here::here("Data", "Sensitivity-analysis", "incidence_cohort_one_prev.RData"))
load(here::here("Data", "Sensitivity-analysis", "incidence_cohort_one_prev.RData"))


# ---------- classification of all multimorbidity into three types -------------------

# Add an event variable to code for different combinations of medical conditions

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
  mutate(event = case_when(setequal(c(1, 2),  event) ~ 2,
                           setequal(c(6, 7),  event) ~ 2,
                           setequal(c(8, 9),  event) ~ 2,
                           
                           
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

# .------------------------------------------------------------------------
# ------ Plotting ---------------------------------------------------------
# .------------------------------------------------------------------------


# ------ men and women together -------------------------------------------

# cumulative incidence function (CIF)
cif_multimorb <- etmCIF(Surv(age_start, age_censor, event_type != 0) ~ 1, 
                        data = multimorb_risk, etype = event_type)


gg_multimorb <- etm_to_df(cif_multimorb)
gg_multimorb$type <- factor(gg_multimorb$type, levels = c(1, 2, 3),
                            labels = c("Discordant somatic conditions",
                                       "Concordant somatic conditions",
                                       "Somatic and psychosocial condition"))
gg_multimorb <- na.omit(gg_multimorb)


ggplot(gg_multimorb, aes(x = time, y = P, group = type, fill = type)) +
  geom_area() +
  scale_fill_discrete_sequential("Blues", l2 = 80, 
                                 name = "Combination of first two chronic conditions") +
  theme_minimal(base_size = 22) +
  labs(title = "Lifetime risk of multimorbidity for 55-year-old individuals",
       x = "Age (years)", y = "Cumulative incidence") +
  theme(legend.position = c(0.2, 0.9),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "white", colour = "white"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16))


# .------------------------------------------------------------------------
# ------ Plotting: men and women separately ------------------------------------
# .------------------------------------------------------------------------

# .------------------------------------------------------------------------
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

# .------------------------------------------------------------------------
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


# .------------------------------------------------------------------------
# .. Merge the two datasets ------------
gg_multimorb <- bind_rows(gg_multimorb_women, gg_multimorb_men)


# .------------------------------------------------------------------------
# ... Confidence intervals - optional -----------------
# stack up the confidence intervals
gg_multimorb$lower[gg_multimorb$type == "Concordant somatic"] <- 
  gg_multimorb$lower[gg_multimorb$type == "Concordant somatic"] + gg_multimorb$P[gg_multimorb$type == "Somatic and psychosocial"]  

gg_multimorb$lower[gg_multimorb$type == "Discordant somatic"] <- 
  gg_multimorb$lower[gg_multimorb$type == "Discordant somatic"] + gg_multimorb$P[gg_multimorb$type == "Concordant somatic"] + gg_multimorb$P[gg_multimorb$type == "Somatic and psychosocial"]

gg_multimorb$upper[gg_multimorb$type == "Concordant somatic"] <- 
  gg_multimorb$upper[gg_multimorb$type == "Concordant somatic"] + gg_multimorb$P[gg_multimorb$type == "Somatic and psychosocial"]  

gg_multimorb$upper[gg_multimorb$type == "Discordant somatic"] <- 
  gg_multimorb$upper[gg_multimorb$type == "Discordant somatic"] + gg_multimorb$P[gg_multimorb$type == "Concordant somatic"] + + gg_multimorb$P[gg_multimorb$type == "Somatic and psychosocial"]

# only keep two points for the CI
gg_multimorb <- gg_multimorb %>%
  mutate(lower = case_when(!(time %in% c(80.6, 80.0, 89.8, 93.00)) ~ NA_real_,
                           TRUE ~ lower),
         upper = case_when(!(time %in% c(80.6, 93.00, 80.0, 89.8)) ~ NA_real_,
                           TRUE ~ upper))

# .------------------------------------------------------------------------
# .. Plotting ------------------------------------------------------------------

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
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.8, size = 1, colour = "darkslateblue") +
  facet_wrap(~ sex) +
  frmt  

# .------------------------------------------------------------------------
# .. export figure ---------
ragg::agg_tiff(here::here("Figures", "Fig_3.tiff"), 
               width = 3800, height = 2500, units = "px", res = 300, scaling = 1.1)
ggplot(gg_multimorb, aes(x = time, y = P, group = type, fill = type)) +
  geom_area() +
  facet_wrap(~ sex) +
  frmt  
dev.off()


fig_3 <- ggplot(gg_multimorb, aes(x = time, y = P, group = type, fill = type)) +
  geom_area() +
  facet_wrap(~ sex) +
  frmt  

ggsave(file = (here::here("Figures", "Fig_3.pdf")), 
       plot = fig_3, device = "pdf", width = 380,
       height = 250, units = "mm")


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

sum(round(gg_multimorb[gg_multimorb$time == men_65 & gg_multimorb$sex == "men", "P"], 3))
sum(round(gg_multimorb[gg_multimorb$time == women_65 & gg_multimorb$sex == "women", "P"], 3))



# at 99 years 
men_99 <- min(gg_multimorb$time[gg_multimorb$time > 99 & gg_multimorb$sex == "men"])
women_99 <- min(gg_multimorb$time[gg_multimorb$time > 99 & gg_multimorb$sex == "women"])

ltr_men <- gg_multimorb[gg_multimorb$time == men_99 & 
                     gg_multimorb$sex == "men", c("P", "var", "upper", "lower", "type")]
ltr_women <- gg_multimorb[gg_multimorb$time == women_99 & 
                     gg_multimorb$sex == "women", c("P", "var", "upper", "lower", "type")]

# p value for sex difference

# somatic-psychiatric 
ltr_men_ps <- ltr_men[ltr_men$type == "Somatic and psychiatric", ]
ltr_women_ps <- ltr_women[ltr_women$type == "Somatic and psychiatric", ]

z <- (ltr_men_ps$P - ltr_women_ps$P) / (sqrt(ltr_men_ps$var + ltr_women_ps$var))
1 - pnorm(abs(z)) 

# somatic discordant
ltr_men_dis <- ltr_men[ltr_men$type == "Discordant somatic", ]
ltr_women_dis <- ltr_women[ltr_women$type == "Discordant somatic", ]

z <- (ltr_men_dis$P - ltr_women_dis$P) / 
  (sqrt(ltr_men_dis$var + ltr_women_dis$var))
1 - pnorm(abs(z)) 

# somatic concordant
ltr_men_con <- ltr_men[ltr_men$type == "Concordant somatic", ]
ltr_women_con <- ltr_women[ltr_women$type == "Concordant somatic", ]

z <- (ltr_men_con$P - ltr_women_con$P) / 
  (sqrt(ltr_men_con$var + ltr_women_con$var))
1 - pnorm(abs(z)) 

# .------------------------------------------------------------------------
# ------ LIfetime risk overall -------------------------------------------------
# .------------------------------------------------------------------------

# 0 = censored
# 1 = multimorbidity
# 2 = death


# .. Men -----------
multimorb_risk_men_all <- multimorb_risk %>%
  filter(sex == "male") %>%
  mutate(event_type = case_when(event_type %in% c(1, 2, 3) ~ 1,
                                event_type == 4 ~ 2,
                                TRUE ~ 0))

cif_multimorb_men_all <- etmCIF(Surv(age_start, age_censor, event_type != 0) ~ 1, 
                            data = multimorb_risk_men_all, etype = event_type)


gg_multimorb_men_all <- etm_to_df(cif_multimorb_men_all)

gg_multimorb_men_all <- na.omit(gg_multimorb_men_all)
gg_multimorb_men_all$sex <- "men"


# .. Women --------
multimorb_risk_women_all <- multimorb_risk %>%
  filter(sex == "female") %>%
  mutate(event_type = case_when(event_type %in% c(1, 2, 3) ~ 1,
                                event_type == 4 ~ 2,
                                TRUE ~ 0))

cif_multimorb_women_all <- etmCIF(Surv(age_start, age_censor, event_type != 0) ~ 1, 
                              data = multimorb_risk_women_all, etype = event_type)


gg_multimorb_women_all <- etm_to_df(cif_multimorb_women_all)

gg_multimorb_women_all <- na.omit(gg_multimorb_women_all)
gg_multimorb_women_all$sex <- "women"

# .. Merge the two datasets ------------
gg_multimorb_all <- bind_rows(gg_multimorb_women_all, gg_multimorb_men_all)


# get percentages for the results section:

# at 65 years 
men_65 <- min(gg_multimorb_all$time[gg_multimorb_all$time > 65 & gg_multimorb_all$sex == "men"])
women_65 <- min(gg_multimorb_all$time[gg_multimorb_all$time > 65 & gg_multimorb_all$sex == "women"])

round(gg_multimorb_all[gg_multimorb_all$time == men_65 & 
                   gg_multimorb_all$sex == "men", c("P", "upper", "lower")], 3)
round(gg_multimorb_all[gg_multimorb_all$time == women_65 & 
                   gg_multimorb_all$sex == "women", c("P", "upper", "lower")], 3)


# at 75 years 
men_75 <- min(gg_multimorb_all$time[gg_multimorb_all$time > 75 & gg_multimorb_all$sex == "men"])
women_75 <- min(gg_multimorb_all$time[gg_multimorb_all$time > 75 & gg_multimorb_all$sex == "women"])

round(gg_multimorb_all[gg_multimorb_all$time == men_75 & 
                         gg_multimorb_all$sex == "men", c("P", "upper", "lower")], 3)
round(gg_multimorb_all[gg_multimorb_all$time == women_75 & 
                         gg_multimorb_all$sex == "women", c("P", "upper", "lower")], 3)

# at 85 years
men_85 <- min(gg_multimorb_all$time[gg_multimorb_all$time > 85 & gg_multimorb_all$sex == "men"])
women_85 <- min(gg_multimorb_all$time[gg_multimorb_all$time > 85 & gg_multimorb_all$sex == "women"])


round(gg_multimorb_all[gg_multimorb_all$time == men_85 & 
                   gg_multimorb_all$sex == "men", c("P", "upper", "lower")], 3)
round(gg_multimorb_all[gg_multimorb_all$time == women_85 & 
                   gg_multimorb_all$sex == "women", c("P", "upper", "lower")], 3)


# at 99 years
men_99 <- min(gg_multimorb_all$time[gg_multimorb_all$time > 99 & gg_multimorb_all$sex == "men"])
women_99 <- min(gg_multimorb_all$time[gg_multimorb_all$time > 99 & gg_multimorb_all$sex == "women"])

ltr_men <- round(gg_multimorb_all[gg_multimorb_all$time == men_99 & 
                         gg_multimorb_all$sex == "men", c("P", "var", "upper", "lower")], 5)
ltr_women <- round(gg_multimorb_all[gg_multimorb_all$time == women_99 & 
                         gg_multimorb_all$sex == "women", c("P", "var", "upper", "lower")], 5)


# p-value for difference
z <- (ltr_men$P - ltr_women$P) / (sqrt(ltr_men$var + ltr_women$var))
1 - pnorm(z) 
