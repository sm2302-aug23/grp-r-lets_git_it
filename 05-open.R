# Load libraries -------------------------------------------------------------
library(tidyverse)
library(tidyr)
library(dplyr)
library(tibble)
library(testthat, quietly = TRUE)

# Task 5 ---------------------------------------------------------------------

# Analysing the arithmetic progressions in 
# stopping times of the Collatz Conjecture. 

# THE HYPOTHESIS

# The starting integer can affect 
# the number of steps it takes a sequence to reach one.

# 1) Odd number tend to produce a longer sequence (Especially prime numbers)
#   When an odd integer is substituted in 3n+1, 
#   it becomes larger, resulting in more iterations before reaching 1.

# 2) Even number tend to produce shorter sequence 
#   When an even integer is divided by 2, it immediately becomes smaller,
#   which can lead to quicker convergence.


# Data frame of integers with stopping times more than 100 ------------------

filtered_collatz_df <- collatz_df %>% 
  filter(length > 100)

length_above_100 <- filtered_collatz_df 

length_above_100 <- length_above_100 %>%
  select(-max_val, -seq)

print(length_above_100)

# The amount of odd and even integers ---------------------------------------

Odd_amount <- length_above_100 %>%
  filter(parity == "Odd") %>%
  summarise(
    count = n()
  )

Even_amount <- length_above_100 %>%
  filter(parity == "Even") %>%
  summarise(
    count = n()
  )

print(Odd_amount)
print(Even_amount)

Comparison_Odd_Even <- bind_rows(Odd_amount, Even_amount)

print(Comparison_Odd_Even)


