library(R.cache)
library(tibble)
library(ggplot2)
library(tidyverse)

get_care_outcome_tibble <- function() {
        evalWithMemoization({
                care_outcome_tibble <- as_tibble(read.csv("outcome-of-care-measures.csv"))
        })
        
        care_outcome_tibble
}


validate_state <- function(selected_state) {
        valid_states <- unique(get_care_outcome_tibble()$State)
        
        if (!any(selected_state == valid_states)) {
                stop("invalid state")
        }
}


validate_care_outcome <- function(selected_outcome) {
        valid_outcomes = c(
                "heart attack", "heart failure", "pneumonia"
        )
        
        if (!any(selected_outcome == valid_outcomes)) {
                stop("invalid outcome")
        }
}


get_care_outcome_colname <- function(selected_care_outcome) {
        stub <- "Hospital.30.Day.Death..Mortality..Rates.from.%s"
        
        selected_care_outcome %>% 
                stringr::str_to_title(.) %>% 
                gsub(" ", ".", .) %>% 
                sprintf(stub, .)
}


convert_char_col_to_double <- function(tib, colname) {
        suppressWarnings(mutate(tib, across(colname, as.double)) %>% na.omit())
}