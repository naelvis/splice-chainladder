library(tidyverse)
library(vroom)
library(magrittr)

# Read in input
input_raw <- vroom("./data/incurred_1.csv")

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

# Create empty triangle with all development periods and claim numbers
max_development <- max(input$Development)
all_claims <- unique(input$claim_no)

empty_triangle <- expand.grid(claim_no = all_claims,
                              #Occurrence = all_occurrence,
                              Development = 0:max_development,
                              incurred_zero = 0)

# Create mapping of claim numbers to occurrence period
claimno_occurrence <- input %>% 
  select(claim_no, Occurrence) %>% 
  distinct

# Create triangle by joining with empty triangle to make sure it has all rows and columns
# Deleted claims after diagonal
triangle <- input %>% 
  ungroup %>% 
  select(claim_no, incurred, Development) %>% 
  full_join(empty_triangle) %>% 
  mutate(incurred = replace_na(incurred, 0),
         Incurred = incurred + incurred_zero) %>%
  group_by(claim_no) %>% 
  arrange(Development, .by_group=TRUE) %>% 
  mutate(Incurred = cumsum(Incurred)) %>% 
  ungroup %>% 
  full_join(claimno_occurrence) %>% 
  select(claim_no, Occurrence, Development, Incurred) %>% 
  filter(Occurrence + Development <= max(Occurrence)) 

# Print triangle
write_csv(triangle, "triangle_1.csv")
