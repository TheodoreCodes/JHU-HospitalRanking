source("common.R")

rankall <- function(selected_care_outcome, num = "best") {
        outcome_tibble <- get_care_outcome_tibble()

        validate_care_outcome(selected_care_outcome)
        
        selected_outcome_colname = get_care_outcome_colname(selected_care_outcome)
        
        outcome_tibble %>% 
                select("Hospital.Name", "State", selected_outcome_colname) %>% 
                convert_char_col_to_double(., selected_outcome_colname) %>% 
                arrange_at(c(selected_outcome_colname, "Hospital.Name")) %>% 
                split(., .$State) %>% 
                unname() %>% 
                lapply(function(x) {
                        state_value = x[1,2]$State
                        x[if (num == "best") 1 else if (num == "worst") nrow(x) else as.numeric(num),1:2] %>% 
                                mutate(State = state_value)
                }) %>% 
                bind_rows() %>% 
                rename(hospital = 1, state=2)
}