# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT - Sankey data and plot ------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This script produces the dataframe needed to produce the sankey plot (Figure 1) and the plot itself
# For all questions about it, contact Premysl Velek at p.velek@erasmusmc.nl 


# .-------------------------------------------------------------------------------
# ------------------------ Preliminaries ----------------------------------------
# .--------------------------------------------------------------------------------

# libraries needed in this script
packages_needed <- c("tidyverse", "networkD3", "htmlwidgets")

# install missing libraries
install.packages(setdiff(packages_needed, rownames(installed.packages())))  

# load libraries
require(tidyverse)
require(networkD3)
require(htmlwidgets)

# start here
setwd(here::here())


# Load the cleaned shift data
load(here::here("Data", "incidence_cohort.RData"))
load(here::here("Data", "conditions_seq.RData"))

# .-------------------------------------------------------------------------------
# --------------------------------- helpers --------------------------------------
# .-------------------------------------------------------------------------------

# load helpers
source(here::here("script", "99_helpers.R"))

# remove the censor date from event_data (so that the get_condition function works)
event_data <- event_data[, -13]

conditions_seq <- get_conditions(inc_data = inc_data, event_data = event_data)
conditions_seq <- rename_endstate(conditions_seq) %>% ungroup

df_1 <- rename_conditions(conditions_seq[, c(1:2, 9)], step = 1)
df_2 <- rename_conditions(conditions_seq[, c(2:3, 9)], step = 2)
df_3 <- rename_conditions(conditions_seq[, c(3:4, 9)], step = 3)
df_4 <- rename_conditions(conditions_seq[, c(4:5, 9)], step = 4)
df_5 <- rename_conditions(conditions_seq[, c(5:6, 9)], step = 5)
df_6 <- rename_conditions(conditions_seq[, c(6:7, 9)], step = 6)


# ------------ sankey with 3 steps --------------------------

df <- bind_rows(list(df_1, df_2)) %>%


# group conditions into categories
# Asthma, COPD = Lung diseases
# CHD, Heart failure = Heart diseases
# Dementia, Parkinsonism = Neurodegenerative diseases
# All others remain the same

  mutate_all(~ str_replace(., "Asthma_1", "Lung diseases_1")) %>%
  mutate_all(~ str_replace(., "COPD_1", "Lung diseases_1")) %>%
  mutate_all(~ str_replace(., "Asthma_2", "Lung diseases_2")) %>%
  mutate_all(~ str_replace(., "COPD_2", "Lung diseases_2")) %>%
  mutate_all(~ str_replace(., "Asthma_3", "Lung diseases_3")) %>%
  mutate_all(~ str_replace(., "COPD_3", "Lung diseases_3")) %>%
             
  mutate_all(~ str_replace(., "Coronary heart disease_1", "Heart diseases_1")) %>%
  mutate_all(~ str_replace(., "Coronary heart disease_2", "Heart diseases_2")) %>%
  mutate_all(~ str_replace(., "Heart failure_1", "Heart diseases_1")) %>%
  mutate_all(~ str_replace(., "Heart failure_2", "Heart diseases_2")) %>%
  mutate_all(~ str_replace(., "Heart failure_3", "Heart diseases_3")) %>%
  mutate_all(~ str_replace(., "Coronary heart disease_3", "Heart diseases_3")) %>%
             
  mutate_all(~ str_replace(., "Dementia_1", "Neurodegenerative diseases_1")) %>%
  mutate_all(~ str_replace(., "Dementia_2", "Neurodegenerative diseases_2")) %>%
  mutate_all(~ str_replace(., "Dementia_3", "Neurodegenerative diseases_3")) %>%
  mutate_all(~ str_replace(., "Parkinsonism_1", "Neurodegenerative diseases_1")) %>%
  mutate_all(~ str_replace(., "Parkinsonism_2", "Neurodegenerative diseases_2")) %>%
  mutate_all(~ str_replace(., "Parkinsonism_3", "Neurodegenerative diseases_3")) 


# color edges by sex (not used in the final paper !!!)
df_final <- df %>%
  group_by(from, to, sex) %>%
  count %>%
  ungroup 
  
# keep the edges uniform
df_final <- df %>%
  group_by(from, to) %>%
  count %>%
  ungroup 

# ---------- Final plots ----------------------------------------------

# plot the final sankey plot. NB: to colour-code the link by sex, set the by_sex argument to TRUE. 
# in this case,the data frame supplied in df argument MUST contain counts separately by sex
plot_sankey <- function(df, by_sex = FALSE){
  # A node data frame: it lists every entities involved in the flow
  nodes <- data.frame(name = c(df$from, df$to)) %>% unique
  
  # order nodes so that censored participants are on top
  nodes$order <- 100
  nodes$order[grepl("Disease free", nodes$name, fixed = TRUE)] <- c(1, 2, 3)
  nodes$order[nodes$name == "Depression_1"] <- 4
  nodes$order[nodes$name == "One disease_2"] <- 5
  nodes$order[nodes$name == "One disease_3"] <- 6
  nodes$order[nodes$name == "Cancer_1"] <- 7
  nodes$order[nodes$name == "Depression_2"] <- 8
  nodes$order[nodes$name == "Two diseases_3"] <- 9
  nodes$order[grepl("Died", nodes$name, fixed = TRUE)] <- c(110, 200, 300)
  
  nodes <- arrange(nodes, order)
  
  # With networkD3, connection must be provided by id, not variable names => assign ids to nodes
  df$IDfrom = match(df$from, nodes$name) - 1 
  df$IDto =   match(df$to, nodes$name) - 1
  
  nodes$name <- substr(nodes$name, 1, nchar(nodes$name) - 2)
  
  # Make the Network
  if(!by_sex){
  sankey_3 <- sankeyNetwork(Links = df, Nodes = nodes,
                            Source = "IDfrom", Target = "IDto",
                            Value = "n", NodeID = "name", fontFamily = "Corbel",
                            colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20c);"),
                            fontSize = 15, nodePadding = 8)
  } 
  else {
    sankey_3 <- sankeyNetwork(Links = df, Nodes = nodes,
                              Source = "IDfrom", Target = "IDto",
                              Value = "n", NodeID = "name", fontFamily = "Corbel",
                              colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20c);"),
                              fontSize = 15, nodePadding = 8, LinkGroup = "sex")
  }
  
  sankey_3
}

sankey <- plot_sankey(df_final)
sankey <- plot_sankey(df_final, by_sex = TRUE)

sankey

# save the widget

saveWidget(sankey, file = here::here("Figures", "fig_1.html"))
saveWidget(sankey, file = here::here("Figures", "sankey_3_final_by_sex.html"))

# . With no condition removed ----------------------
df_final_no_zeros <- df_final %>%
  filter(!str_detect(from, "Disease free"))

sankey <- plot_sankey(df_final_no_zeros)
saveWidget(sankey, file = paste0( getwd(), "/sankey_3_final_no_zeros.html"))

# . Separate by sex ------------------------------
df_final_m <- df %>%
  filter(sex == "male") %>%
  group_by(from, to) %>%
  count %>%
  ungroup 


df_final_f <- df %>%
  filter(sex == "female") %>%
  group_by(from, to) %>%
  count %>%
  ungroup 

sankey_m <- plot_sankey(df_final_m)
sankey_f <- plot_sankey(df_final_f)

saveWidget(sankey_m, file = here::here("Figures", "fig_1_male.html"))
saveWidget(sankey_f, file = here::here("Figures", "fig_1_female.html"))

# .. Separate by sex and zeros removed ------------------------------
df_final_m_no_zeros <- df_final_m %>%
  filter(!str_detect(from, "Disease free"))

df_final_f_no_zeros <- df_final_f %>%
  filter(!str_detect(from, "Disease free"))

sankey_m_no_zeros <- plot_sankey(df_final_m_no_zeros)
sankey_f_no_zeros <- plot_sankey(df_final_f_no_zeros)

saveWidget(sankey_m_no_zeros, file = paste0(   getwd(), "/sankey_3_final_male_no_zeros.html"))
saveWidget(sankey_f_no_zeros, file = paste0( getwd(), "/sankey_3_final_female_no_zeros.html"))

# Save all dataframes into one file
save(df_final,
     df_final_f,
     df_final_m,
     df_final_no_zeros,
     df_final_f_no_zeros,
     df_final_m_no_zeros,
     file = here::here("Data", "sankey_data.Rdata"))  


# Save all dataframes also into the shiny app folder
save(df_final,
     df_final_f,
     df_final_m,
     df_final_no_zeros,
     df_final_f_no_zeros,
     df_final_m_no_zeros,
     file = here::here("Script", "Sankey", "sankey_data.Rdata"))  

# .-------------------------------------------------------------------------------------------------
# ------------ Sankey with 3 steps and grouped into multimorbidity classes -------------------------
# ------------ Sensitivity analysis ----------------------------------------------------------------
# .-------------------------------------------------------------------------------------------------


df_1_g <- df_1 %>%
  
  
  # group conditions into categories: 
  # Asthma, COPD = Lung diseases
  # CHD, Heart failure = Heart diseases
  # Dementia, Parkinsonism = Neurodegenerative diseases
  # All others remain the same
  
  mutate_all(~ str_replace(., "Asthma_1", "Lung diseases_1")) %>%
  mutate_all(~ str_replace(., "COPD_1", "Lung diseases_1")) %>%
  mutate_all(~ str_replace(., "Coronary heart disease_1", "Heart diseases_1")) %>%
  mutate_all(~ str_replace(., "Heart failure_1", "Heart diseases_1")) %>%
  mutate_all(~ str_replace(., "Dementia_1", "Neurodegenerative diseases_1")) %>%
  mutate_all(~ str_replace(., "Parkinsonism_1", "Neurodegenerative diseases_1")) %>%
  
  
  mutate_all(~ str_replace(., "Asthma_2", "Lung diseases_2")) %>%
  mutate_all(~ str_replace(., "COPD_2", "Lung diseases_2")) %>%
  mutate_all(~ str_replace(., "Asthma_3", "Lung diseases_3")) %>%
  mutate_all(~ str_replace(., "COPD_3", "Lung diseases_3")) %>%
  
  
  mutate_all(~ str_replace(., "Coronary heart disease_2", "Heart diseases_2")) %>%
  
  mutate_all(~ str_replace(., "Heart failure_2", "Heart diseases_2")) %>%
  mutate_all(~ str_replace(., "Heart failure_3", "Heart diseases_3")) %>%
  mutate_all(~ str_replace(., "Coronary heart disease_3", "Heart diseases_3")) %>%
  
  
  mutate_all(~ str_replace(., "Dementia_2", "Neurodegenerative diseases_2")) %>%
  mutate_all(~ str_replace(., "Dementia_3", "Neurodegenerative diseases_3")) %>%
  mutate_all(~ str_replace(., "Parkinsonism_2", "Neurodegenerative diseases_2")) %>%
  mutate_all(~ str_replace(., "Parkinsonism_3", "Neurodegenerative diseases_3")) %>%
  
  mutate_all(~ str_replace(., "Stroke_2",   "Somatic_2")) %>%
  mutate_all(~ str_replace(., "Cancer_2",   "Somatic_2")) %>%
  mutate_all(~ str_replace(., "Diabetes_2", "Somatic_2")) %>%
  
  mutate_all(~ str_replace(., "Stroke_3",   "Somatic_3")) %>%
  mutate_all(~ str_replace(., "Cancer_3",   "Somatic_3")) %>%
  mutate_all(~ str_replace(., "Diabetes_3", "Somatic_3"))
  
  
  
  

# Group disease into multimorbidity categories: STEP 2

df_1_g  <- df_1_g  %>%
                        # MM with depression as the first condition
  mutate(to = case_when(from == "Depression_1" & !(to %in% c("Died_2", "One condition_2"))  
                        ~ "Somatic-psychosocial_2",
                        
                        # MM with depression as the second condition
                        !(from %in% c("Died_1", "No condition_1")) & to == "Depression_2" 
                        ~ "Somatic-psychosocial_2",
                        
                        # Somatic concordant MM
                        from == "Lung diseases_1" & to == "Lung diseases_2" 
                        ~ "Somatic concordant_2", 
                        
                        from == "Heart diseases_1" & to == "Heart diseases_2" 
                        ~ "Somatic concordant_2", 
                        
                        from == "Neurodegenerative diseases_1" & to == "Neurodegenerative diseases_2" 
                        ~ "Somatic concordant_2",
                        
                        # Somatic discordant MM
                        !(from %in% c("Died_1", "No condition_1", "Depression_1")) & 
                          !(to %in% c("Died_2", "No condition_2", "One condition_2", "Depression_2")) 
                        ~ "Somatic discordant_2",
                        
                        TRUE ~ to)) 


df_2_g <- df_2 %>%
  mutate(from = df_1_g$to)
 
                        
# Group disease into multimorbidity categories: STEP 3
df_2_g <- df_2_g %>%
  
  # MM with depression as the third condition
  mutate(to = case_when(to == "Depression_3" ~ "Somatic-psychosocial_3",
                         
                         # MM with depression as the second or first condition
                         from == "Somatic-psychosocial_2" & !(to %in% c("Died_3", "Two condition_3"))  
                         ~ "Somatic-psychosocial_3",
                         
                         # Somatic discordant MM
                         from == "Somatic concordant_2" & !(to %in% c("Died_3", "Two condition_3")) 
                         ~ "Somatic discordant_3", 
                         
                         from == "Somatic discordant_2" & !(to %in% c("Died_3", "Two condition_3")) 
                         ~ "Somatic discordant_3", 
                         
                         TRUE ~ to)) 


df <- bind_rows(list(df_1_g, df_2_g))

# color edges by sex
df_final <- df %>%
  group_by(from, to, sex) %>%
  count %>%
  ungroup 

# keep the edges uniform
df_final <- df %>%
  group_by(from, to) %>%
  count %>%
  ungroup 

# separately men and women
df_final_m <- df %>%
  filter(sex == "male") %>%
  group_by(from, to) %>%
  count %>%
  ungroup 


df_final_w <- df %>%
  filter(sex == "female") %>%
  group_by(from, to) %>%
  count %>%
  ungroup 


# ---------- Final plots ----------------------------------------------

# plot the final sankey plot. NB: to colour-code the link by sex, set the by_sex argument to TRUE. 
# in this case,the data frame supplied in df argument MUST contain counts separately by sex
plot_sankey <- function(df, by_sex = FALSE){
  # A node data frame: it lists every entities involved in the flow
  nodes <- data.frame(name = c(df$from, df$to)) %>% unique
  
  # order nodes so that censored participants are on top
  nodes$order <- 100
  nodes$order[grepl("No condition", nodes$name, fixed = TRUE)] <- c(1, 2, 3)
  nodes$order[nodes$name == "Depression_1"] <- 4
  nodes$order[nodes$name == "One condition_2"] <- 5
  nodes$order[nodes$name == "One condition_3"] <- 6
  nodes$order[nodes$name == "Cancer_1"] <- 7
  nodes$order[nodes$name == "Somatic-psychosocial_2"] <- 8
  nodes$order[nodes$name == "Somatic discordant_2"] <- 9
  nodes$order[nodes$name == "Somatic concordant_2"] <- 10
  nodes$order[grepl("Died", nodes$name, fixed = TRUE)] <- c(1100, 2000, 3000)
  
  nodes <- arrange(nodes, order)
  
  # With networkD3, connection must be provided by id, not variable names => assign ids to nodes
  df$IDfrom = match(df$from, nodes$name) - 1 
  df$IDto =   match(df$to, nodes$name) - 1
  
  nodes$name <- substr(nodes$name, 1, nchar(nodes$name) - 2)
  
  # Make the Network
  if(!by_sex){
    sankey_3 <- sankeyNetwork(Links = df, Nodes = nodes,
                              Source = "IDfrom", Target = "IDto",
                              Value = "n", NodeID = "name", fontFamily = "Corbel",
                              colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20c);"),
                              fontSize = 15, nodePadding = 8, iterations = 0)
  } 
  else {
    sankey_3 <- sankeyNetwork(Links = df, Nodes = nodes,
                              Source = "IDfrom", Target = "IDto",
                              Value = "n", NodeID = "name", fontFamily = "Corbel",
                              colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20c);"),
                              fontSize = 15, nodePadding = 8, LinkGroup = "sex")
  }
  
  sankey_3
}

sankey <- plot_sankey(df_final)
sankey <- plot_sankey(df_final, by_sex = TRUE)

sankey_m <- plot_sankey(df_final_m)
sankey_w <- plot_sankey(df_final_w)

# save the widget
library(htmlwidgets)
saveWidget(sankey_m, file = paste0( getwd(), "/sankey_3_grouped_m.html"))
saveWidget(sankey_w, file = paste0( getwd(), "/sankey_3_grouped_w.html"))

# . With no condition removed ----------------------
df_final_no_zeros <- df_final %>%
  filter(!str_detect(from, "No condition"))

sankey <- plot_sankey(df_final_no_zeros)
saveWidget(sankey, file = paste0( getwd(), "/sankey_3_final_no_zeros.html"))