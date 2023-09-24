# Load libraries -------------------------------------------------------------
library(tidyverse)
library(tidyr)
library(dplyr)
library(tibble)
library(testthat, quietly = TRUE)

# Task 1 ---------------------------------------------------------------------

# Collatz Conjecture data frame 

gen_collatz <- function(n) {
  if (n <= 0 || !is.integer(n)) {
    stop("Input must be a positive integer.")
  }
  collatz_seq <- c(n)
  while (n != 1) {
    if (n %% 2 == 0) {
      n <- n / 2
    } else {
      n <- 3 * n + 1
    }
    collatz_seq <- c(collatz_seq, n)
  }
  return(collatz_seq)
}

collatz_df <- tibble(start = integer(), seq = list(), length = numeric(), 
                     parity = character(), max_val = numeric())

for (i in 1:10000) {
  collatz_seq <- gen_collatz(i)
  collatz_df <- add_row(collatz_df, start = i, seq = list(collatz_seq),
                        length = length(collatz_seq), 
                        parity = ifelse(i %% 2 == 0, "Even", "Odd"), 
                        max_val = max(collatz_seq))
}
head(collatz_df)
print(collatz_df)

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

