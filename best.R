source("common.R")

best <- function(selected_state, selected_care_outcome) {
        outcome_tibble <- get_care_outcome_tibble()
        
        validate_state(selected_state)
        validate_care_outcome(selected_care_outcome)
        
        selected_outcome_colname <- get_care_outcome_colname(selected_care_outcome)
        
        
        (outcome_tibble %>% 
                        filter(State == selected_state) %>% 
                        select("Hospital.Name", selected_outcome_colname) %>%
                        convert_char_col_to_double(., selected_outcome_colname) %>% 
                        arrange_at(c(selected_outcome_colname, "Hospital.Name")))[1,]$Hospital.Name
}