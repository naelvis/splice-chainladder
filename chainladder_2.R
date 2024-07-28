library(tidyverse)
library(magrittr)
library(vroom)

# Functions #######

# Extract ultimate over all occurrence periods
extract_ultimate <- function(data) {
  data %>% 
    filter(Development == max(Development)) %>% 
    group_by(Development) %>% 
    summarise(Ultimate = sum(Incurred)) %>% 
    extract2(1, 2)
}

# Project single claim and compute square error compared to real ultimate
ultimate_square_error <- function(selected_claim) {
  
  print(str_c("Now reserving for: ", selected_claim))
  
  single_triangle <- input %>% 
    ungroup %>% 
    filter(claim_no == selected_claim) %>% 
    select(incurred, Occurrence, Development) %>% 
    group_by(Occurrence, Development) %>% 
    summarise(incurred = sum(incurred), .groups = "drop") %>% 
    ungroup %>% 
    full_join(empty_triangle, by = c("Occurrence", "Development")) %>% 
    mutate(incurred = replace_na(incurred, 0),
           Incurred = incurred + incurred_zero) %>%
    group_by(Occurrence) %>% 
    arrange(Development) %>% 
    mutate(Incurred = cumsum(Incurred)) %>% 
    ungroup %>% 
    select(Occurrence, Development, Incurred) %>% 
    filter(Occurrence + Development <= max(Occurrence)) 
  
  projected_single_triangle <- reduce(1:max(single_triangle$Development), 
                                      apply_factor, 
                                      .init = single_triangle)
  
  projected_ultimate <- extract_ultimate(projected_single_triangle)
  
  real_ultimate <- input_raw %>%
    filter(claim_no == selected_claim) %>% 
    select(claim_size) %>% 
    extract2(1, 1)
  
  ultimate_se <- (projected_ultimate - real_ultimate)^2
  
  return(ultimate_se)
  
}

# Utility function to display triangle as triangle rather than table
triangulate <- function(data) {
  data %>% 
    pivot_wider(names_from = Development, values_from = Incurred, values_fill = 0)
}  

# Compute chain ladder factor for each development period
compute_factor <- function(development, data = triangle) {
  
  factor_data <- triangle %>% 
    filter(Occurrence <= max(Occurrence) - development,
           Development %in% c(development, development-1)) %>% 
    group_by(Development) %>% 
    summarise(Incurred = sum(Incurred))
  
  if (nrow(factor_data) < 2) {
    factor <- 1
  } else {
    factor <- factor_data[[2, 2]] %>% 
      divide_by(factor_data[[1, 2]])
  }
  
  return(factor)
}

# Apply development factor to each development period
apply_factor <- function(data, development, factor_data = factors) {
  
  projected_triangle <- data %>% 
    filter(Occurrence > max(Occurrence) - development) %>% 
    group_by(Occurrence) %>% 
    ungroup %>% 
    left_join(factors, by = "Development")  %>% 
    filter(Development == development - 1) %>% 
    mutate(Incurred_Projected = Incurred*Factor,
           Development = Development + 1) %>% 
    select(Occurrence, Development, Incurred_Projected) %>% 
    full_join(data, by = c("Occurrence", "Development")) %>% 
    mutate(Incurred = ifelse(is.na(Incurred), Incurred_Projected, Incurred)) %>% 
    arrange(Occurrence) %>% 
    select(Occurrence, Development, Incurred)
  
  return(projected_triangle)
}

# Projection ######

# Read in input
input_raw <- vroom("./data/incurred_2.csv")

## Generate aggregated triangle and compute Chain-Ladder factors ######

# For each claim take last development per development period
# Compute incremental incurred
input <- input_raw %>% 
  select(claim_no, claim_size, txn_time, incurred) %>% 
  mutate(Development_absolute = ceiling(txn_time)) %>% 
  group_by(claim_no, Development_absolute) %>% 
  filter(txn_time == max(txn_time)) %>% 
  ungroup %>% 
  group_by(claim_no) %>% 
  mutate(Occurrence = min(Development_absolute),
         Development = Development_absolute - Occurrence) %>% 
  mutate(incurred = incurred - lag(incurred, default = 0))

# Create empty triangle with all development and occurrence periods
max_development <- max(input$Development)
all_occurrence <- unique(input$Occurrence)

empty_triangle <- expand.grid(Occurrence = unique(input$Occurrence),
                              Development = 0:max(input$Development),
                              incurred_zero = 0)

# Create triangle by aggregating claims
# Join with empty triangle to make sure it has all rows and columns
# Deleted claims after diagonal
triangle <- input %>% 
  ungroup %>% 
  select(incurred, Occurrence, Development) %>% 
  group_by(Occurrence, Development) %>% 
  summarise(incurred = sum(incurred)) %>% 
  ungroup %>% 
  full_join(empty_triangle) %>% 
  mutate(incurred = replace_na(incurred, 0),
         Incurred = incurred + incurred_zero) %>%
  group_by(Occurrence) %>% 
  arrange(Development) %>% 
  mutate(Incurred = cumsum(Incurred)) %>% 
  ungroup %>% 
  select(Occurrence, Development, Incurred) %>% 
  filter(Occurrence + Development <= max(Occurrence)) 

# Compute factors for aggregated triangle
factors <- map_dfr(1:max(triangle$Development),
                   ~ tibble(Development = .x - 1, Factor = compute_factor(.x)))

# Project ultimate with given factors
projected_triangle <- reduce(1:max(triangle$Development), apply_factor, .init = triangle)

## Example projection for single claim ######

# Single claim triangle example
single_triangle <- input %>% 
  ungroup %>% 
  filter(claim_no == 2000) %>% 
  select(incurred, Occurrence, Development) %>% 
  group_by(Occurrence, Development) %>% 
  summarise(incurred = sum(incurred)) %>% 
  ungroup %>% 
  full_join(empty_triangle) %>% 
  mutate(incurred = replace_na(incurred, 0),
         Incurred = incurred + incurred_zero) %>%
  group_by(Occurrence) %>% 
  arrange(Development) %>% 
  mutate(Incurred = cumsum(Incurred)) %>% 
  ungroup %>% 
  select(Occurrence, Development, Incurred) %>% 
  filter(Occurrence + Development <= max(Occurrence)) 

# Projection for single claim
projected_single_triangle <- reduce(1:max(single_triangle$Development), apply_factor, .init = single_triangle)

## Project all single claims and compute RMSE ######

begin <- Sys.time()
errors <- map(unique(input$claim_no), ultimate_square_error)
end <- Sys.time()
print(str_c("Reserving completed! Elapsed time: ", end - begin))

errors_list <- reduce(errors, c)
write_csv(as.data.frame(errors_list), "squarederror_2.csv")
rmse <- sqrt(mean(errors_list))

