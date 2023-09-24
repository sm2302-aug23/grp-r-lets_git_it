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

# 1) Odd number tend to produce a longer sequence 

# 2) Even number tend to produce shorter sequence 

# The findings should prove this hypothesis for it to be true.


# Data frame of integers with stopping times more than 100 ------------------

filtered_collatz_df <- collatz_df %>% 
  filter(length > 100)

length_above_100 <- filtered_collatz_df 

length_above_100 <- length_above_100 %>%
  select(-max_val, -seq)

print(length_above_100)

# The amount of odd and even integers ---------------------------------------

odd_even_df <- length_above_100 %>%
  group_by(parity) %>%
  summarise(
    count = n()
  )

print(odd_even_df)

# from the odd_even_df data frame, it is shown that Odd integers
# produce larger sequences compared to Even integers.
# (2065 is more than 1719)

