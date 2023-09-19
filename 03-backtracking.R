# Task 3 --------------------------------------------------------

#1 ----------------------------------------------
library(tibble)
library(dplyr)

a_backtrack <- function(seq) {
  seq_length <- length(seq)
  if (seq_length < 3) {
    return(FALSE)
  }
#For-Loop
  for (i in 2 :(seq_length - 1)) {
    if (seq[i] < seq[1] && seq[i+1] > seq[i]) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

backtracks_df <- collatz_df %>%
  group_by(start) %>%
  filter(any(sapply(seq, a_backtrack))) %>%
  ungroup()

head(backtracks_df)

#2 -------------------------------------------

backtracks_df <- collatz_df %>%
  filter(start != 1 & max_val > start)

backtrack_counts <- backtrack_df %>%
  mutate(above_start = map_int(seq, function(seq) sum(seq > start))
         ) %>%
  count(above_start)

mode_backtrack <- backtrack_counts %>%
  filter(n == max(n)) %>%
  pull(above_start)

mode_backtrack