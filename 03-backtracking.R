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
  ungroup() %>%
  select(start)

head(backtracks_df)
print(backtracks_df)

#2 -------------------------------------------
#For sequences that backtrack, what is the most frequently occurring number of times they go above their starting integer? $$`mode_backtrack`$$


mode_backtrack <- backtracks_df %>%
  group_by(start) %>%
  summarize(freq_above_start = sum(unlist(seq) > start)) %>%
  ungroup() %>%
  group_by(freq_above_start) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(1) %>%
  pull(freq_above_start)

print(mode_backtrack)

#3 -----------------------------------------

max_after_backtrack <- max(backtracks_df$max_after_backtrack, na.rm = TRUE)

print(max_after_backtrack)

#4 -----------------------------------------

even_odd_backtrack <- backtracks_df %>%
  group_by(parity) %>%
  summarize(count = n()) %>%
  ungroup()

print(even_odd_backtrack)
