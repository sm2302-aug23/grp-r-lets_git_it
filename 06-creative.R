# From task 5 
library(tidyverse)
library(tidyr)
library(dplyr)
library(tibble)
library(testthat, quietly = TRUE)

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

# Analysing the arithmetic progressions in stopping times 
# of the Collatz Conjecture. 

# Data frame of integers with stopping times more than 100 

filtered_collatz_df <- collatz_df %>% 
  filter(length > 100)

length_above_100 <- filtered_collatz_df 

length_above_100 <- length_above_100 %>%
  select(-max_val, -seq)

print(length_above_100)

# Data frame of the comparison between the odd and even parity 

odd_even_df <- length_above_100 %>%
  group_by(parity) %>%
  summarise(
    count = n()
  )

print(odd_even_df)

# Task 6 ------------------------------------------------------------------

library(ggplot2)

# As a continuation from task 5, we wondered how many integers would have 
# a certain amount of sequence length. i.e. how frequent is there a sequence 
# length of 100 or 150. 
# So, that could be described in a histogram to visualize the distribution of
# Collatz sequence lengths for integers with stopping times greater than 100.

# Now to create the histogram

ggplot(filtered_collatz_df, aes(x= length)) +
  geom_histogram(binwidth = 1, fill = "cyan", color = "black") +
  labs(x = "Sequence Length", y = "Frequency") +
  ggtitle("Distribution of Sequence Length for Stopping Times >100")

# Still taking into consideration task 5, as parity is taken into account, 
# we actually wanted to see odd and even integers compared sequence length. 

ggplot(filtered_collatz_df, aes(x = parity,
                                y = length,
                                fill = parity)) +
  geom_boxplot() +
  labs(x = "Parity", y = "Sequence Length") +
  ggtitle("Sequence Lengths by Parity for Stopping Times > 100")

