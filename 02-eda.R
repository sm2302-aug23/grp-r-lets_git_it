library(tibble)
library(tidyverse)
gen_collatz <- function(n) {
  if (n <= 0 || !is.integer(n)) {
    stop("Input must be a positive integer.")
  }
  collatz_seq <- c(n)
  while (n != 1) { 
    if (n %% 2 == 0) {
      n <- n / 2
    } 
    else {
      n <- 3 * n + 1
    }
    collatz_seq <- c(collatz_seq, n)
  }
  return(collatz_seq)
}

start_values <- 1:10000


collatz_df <- tibble(start = start_values,
                     seq = map(start_values, gen_collatz)
)

head(collatz_df)



summary_stats <- collatz_df %>%
  mutate(is_even = start %% 2 == 0) %>%
  group_by(is_even) %>%
  summarise(
    avg_length = mean(length),
    sd_length = sd(length)
    )


