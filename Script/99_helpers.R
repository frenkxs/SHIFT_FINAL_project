# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT transform data ----------------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# A collection of helper function. No need to run this separately. The functions are called from other R scripts as needed



# produces a matrix with event dates (either date of event or end of follow up if no event)
get_event_dates <- function(cohort){
    cohort %>%
        select(ergoid, contains(c("endd_", "mortd")))
}

# produces a boolean matrix in which for each participants it's indicated whether they either had (TRUE) or had not (FALSE) a specific disease.
get_inc <- function(cohort){
    cohort %>%
        mutate(inc_mort = ifelse(is.na(mortd), FALSE, TRUE)) %>%
        select(contains("inc_"))
}


# get_conditions" takes a dataframes with event and incidence dates for all 
# conditions and all participants and return a data frame with chronological 
# sequence of conditions for each individual participants. 
get_conditions <- function(inc_data, event_data){
    
    # flatten both incidence and event data and then keep only incident dates in 
    # the event vector
    inc <- unlist(inc_data, use.names = FALSE)
    event <- unlist(event_data[, -1], use.names = FALSE)
    event[!inc] <- NA
    
    # convert the event vector back to data frame
    event_dates <- matrix(event, nrow = nrow(inc_data), ncol = ncol(inc_data)) %>%
        as.data.frame %>%
        mutate_all(as.Date, origin = "1970-01-01")
    
    # finally get a list of conditions for each participants in chronological order
    conditions <- apply(event_dates, 1, order, na.last = NA)
    
    # get the max length (maximum number of diseases)
    max_conditions <- max(sapply(conditions, length))
    
    # add "0" to empty rows (for participants with no incident case)
    conditions <- purrr::modify(conditions, ~ if(length(.) == 0){ 
        0} else {.})
    
    # collapse the vectors of conditions into one character vector
    conditions <- sapply(conditions, paste, collapse = " ")
    
    # define the split pattern
    split_pat <- letters[1:max_conditions]
    
    data.frame(conditions = conditions) %>%
        separate(conditions, split_pat) %>%
        
        #  replace the zero in the first column with NA
        mutate(a = ifelse(a == 0, NA, a)) %>%
        
        #  add zero-th column indicating health status for all participants at baseline
        # add_column(baseline = "0", .before = 1)
        
        # add sex column
        add_column(sex = incidence_cohort$sex)
}


# This is to replace the NA values with endstate (censoring or death) for each participant
rename_endstate <- function(df){
    df <- df %>% 
        mutate(across(is.character, ~ as.numeric(.))) %>%
        rowwise() %>%
        mutate(death = max(across(a:h), na.rm = TRUE)) %>%
        mutate(across(is.numeric, ~ as.character(.)))
    
    # replace NA with Death for those who died 
    died <- df %>%
        filter(death == "11") %>%
        replace(is.na(.), "Died")
    
    
    # replace NA with No event for those censored. They will be grouped into categories:
    # 1: censored after no event
    # 2: censored after one event
    # 3: censored after three events
    # ... and so on
    censored <- df %>% 
        filter(death != "11") %>%
        
        # calculate number of conditions
        rowwise() %>%
        mutate(n_conditions = 8 - sum(is.na(c_across(cols = everything())))) 
    
    # 1: censored after no event   
    censored_after_no <- censored %>%
        filter(n_conditions == 0) %>%
        replace(is.na(.), "Disease free")
    
    # 2: censored after one event   
    censored_after_one <- censored %>%
        filter(n_conditions == 1) %>%
        replace(is.na(.), "One disease")  
    
    # 3: censored after two events   
    censored_after_two <- censored %>%
        filter(n_conditions == 2) %>%
        replace(is.na(.), "Two diseases")  
    
    # 4: censored after three events   
    censored_after_three <- censored %>%
        filter(n_conditions == 3) %>%
        replace(is.na(.), "Three diseases")
    
    # 5: censored after four events 
    censored_after_four <- censored %>%
        filter(n_conditions == 4) %>%
        replace(is.na(.), "Four diseases")  
    
    # 6: censored after five events   
    censored_after_five <- censored %>%
        filter(n_conditions == 5) %>%
        replace(is.na(.), "Five diseases")
    
    # 7: censored after six events   
    censored_after_six <- censored %>%
        filter(n_conditions == 6) %>%
        replace(is.na(.), "Six diseases")
    
    # put the subsets back together
    df <- bind_rows(died, censored_after_no,
                    censored_after_one,
                    censored_after_two,
                    censored_after_three,
                    censored_after_four,
                    censored_after_five,
                    censored_after_six) %>%
        select(-c(death, n_conditions))
}


# This is to replace the numerical values with the names of each conditions and add 
# info about the step (first, second, third... disease). The function only takes a two-column dataframe at a time, with two consecutive conditions, eg: rename_conditions(df[, 2:3], step = 1). 
rename_conditions <- function(df, step){
    next_step <- paste("_", step + 1, sep = "")
    step <- paste("_", step, sep = "")
    
    colnames(df) <- c("from", "to", "sex") 
    
    df %>%
        mutate_all(~ str_replace(., "11", "Died")) %>%
        mutate_all(~ str_replace(., "10", "Stroke")) %>%
        mutate_all(~ str_replace(., "9", "Asthma")) %>%
        mutate_all(~ str_replace(., "8", "COPD")) %>%
        mutate_all(~ str_replace(., "7", "Dementia")) %>%
        mutate_all(~ str_replace(., "6", "Parkinsonism")) %>%
        mutate_all(~ str_replace(., "5", "Diabetes")) %>%
        mutate_all(~ str_replace(., "4", "Depression")) %>%
        mutate_all(~ str_replace(., "3", "Cancer")) %>%
        mutate_all(~ str_replace(., "2", "Heart failure")) %>%
        mutate_all(~ str_replace(., "1", "Coronary heart disease")) %>%
        
        mutate(from = paste(from, step, sep = ""),
               to = paste(to, next_step, sep = ""))
}