These are project files for the article: 

Velek, P., Luik, A.I., Brusselle, G.G.O. et al. Sex-specific patterns and lifetime risk of multimorbidity in the general population: a 23-year prospective cohort study. BMC Med 20, 304 (2022). https://doi.org/10.1186/s12916-022-02487-x

It contains the raw data, cleaned data used for the analysis presented in the paper, the R code used to clean and analyse the data, and the figures included in the paper. 

See bellow the project structure. Raw ERGO data are all stored in "Datasets" folder, cleaned data are in the "Data" folder. All R code is in the "Script" folder, figures inncluded in the paper are stored in the "Figures" folder (including files to run locally the interactive versions of figures 1 and 2).

The script files have to be run in sequence (indicated by the numbers at the begining of the file names), as most code relies on outcomes generated in previous steps. The code also relies on the exact structure as shown below, it is strongly recommended to run the code as part of R project: start with openinig FINAL_project.RProj.

Contact Premysl Velek (p.velek@erasmusmc.nl) for further information.


Project structure:

+-- Data
|   +-- ancestry.Rdata
|   +-- blood_pressure.Rdata
|   +-- conditions_seq.RData
|   +-- education.Rdata
|   +-- incidence_cohort.RData
|   +-- marital.Rdata
|   +-- sankey_data.Rdata
|   +-- shift_data.Rdata
|   +-- smoking.Rdata
|   +-- upset_data.Rdata
|   +-- Sensitivity-analysis
|   |   +-- conditions_seq_dep.RData
|   |   +-- conditions_seq_one_prev.RData
|   |   +-- incidence_cohort_dep.RData
|   |   +-- incidence_cohort_one_prev.RData
|   |   \-- shift_data_dep.Rdata
+-- Datasets
|   +-- Cancer
|   +-- CVD
|   +-- Dementia
|   +-- Depression
|   +-- Diabetes
|   +-- ERGO-basic
|   +-- Life-style
|   +-- Lung disease
|   +-- Parkinsonism
|   +-- Stroke
+-- Figures
|   +-- fig_1_female.html
|   +-- fig_1_female_files
|   +-- fig_1_male_files
|   +-- fig_2.pdf
|   +-- Fig_3.pdf
+-- FINAL_project.Rproj
\-- Script
    +-- 01_data_cleaning.R
    +-- 02_load.R
    +-- 03_extract.R
    +-- 04_sankey.R
    +-- 05_upset.R
    +-- 06_lifetime-risk-multimorbidity.R
    +-- 07_lifestyle_data.R
    +-- 08_population_baseline.R
    +-- 99_helpers.R
    +-- Sankey
    |   +-- app.R
    |   +-- rsconnect
    |   \-- sankey_data.Rdata
    +-- Sensitivity-analysis
    |   +-- SA_01_data_cleaning.R
    |   +-- SA_02A_load.R
    |   +-- SA_02B_load.R
    |   \-- SA_03_lifetime-risk-multimorbidity.R
    \-- Upset
        +-- app
        |   +-- app.R
        |   +-- app.Rproj
        |   +-- data
        |   |   +-- upset_data.csv
        |   |   \-- upset_data.RData
        |   \-- rsconnect
        |       \-- shinyapps.io
        |           \-- frenkxs
        |               \-- SHIFT-upset.dcf
> 