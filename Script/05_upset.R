# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT - Upset data and plot ------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This script produces the dataframe needed to produce the upset plot (Figure 2) and the plot itself
# For all questions about it, contact Premysl Velek at p.velek@erasmusmc.nl

# .-------------------------------------------------------------------------------
# ------------------------ Preliminaries ----------------------------------------
# .--------------------------------------------------------------------------------

# libraries needed in this script
packages_needed <- c("foreign", "tidyverse", "lubridate", "here", "grid", "gridExtra",
                     "colorspace", "cowplot", "ComplexUpset")

# install missing libraries
install.packages(setdiff(packages_needed, rownames(installed.packages())))  

# load libraries
require(foreign)
require(tidyverse)
require(lubridate)
require(grid)
require(gridExtra)
require(cowplot)
require(ComplexUpset)

# start here
setwd(here::here())

# load in incidence cohort and sequence data
load(here::here("Data", "incidence_cohort.RData"))
load(here::here("Data", "conditions_seq.RData"))


conditions <- c("Coronary heart disease",
                "Heart failure",
                "Cancer",
                "Depression",
                "Diabetes",
                "Parkinsonism",
                "Dementia",
                "COPD",
                "Asthma",
                "Stroke")

# remove mortality
upset_data <- inc_data[, -11]

# rename column names
names(upset_data) <- conditions

# add sex
upset_data$sex <- incidence_cohort$sex

# save
save(upset_data, conditions, file = here::here("Data",  "upset_data.RData"))


fig_2 <- upset(upset_data, conditions, name = "Diseases",
      min_degree = 2, min_size = 10, n_intersections = 30,
      sort_intersections_by = c("cardinality", "degree"),
      mode = "inclusive_intersection",
      width_ratio = 0.2,
      base_annotations = list(
         "Intersection size" = intersection_size(
         mapping = aes(fill = sex),
         mode = "inclusive_intersection") +
         scale_fill_manual(values = c("male" = "#517AC9", "female" = "#C05D5D"),
                           name = NULL)
      ),
      stripes = "white",
      themes = upset_default_themes(text = element_text(size = 20)))

ggsave(file = here::here("Figures", "fig_2.pdf"), 
                 plot = fig_2, device = "pdf", width = 380,
       height = 250, units = "mm")
