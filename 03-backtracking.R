# Task 3 --------------------------------------------------------

#1 ----------------------------------------------
#Starting integer for backtrack data frame

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
#For sequences that backtrack, what is the most frequently occurring number of times they go above their starting integer

mode_backtrack <- backtracks_df %>%
  group_by(start) %>%
  summarise(
    most_common_count = max(table(sapply(seq, function(s) sum(s > start))))
  )

mode_backtrack <- mode_backtrack$most_common_count[which.max(mode_backtrack$most_common_count)]
print(mode_backtrack)


#3 -----------------------------------------
#maximum value reached after the first backtrack for these sequences

max_after_backtrack <- max(backtracks_df$max_after_backtrack, na.rm = TRUE)

print(max_after_backtrack)

#4 -----------------------------------------
#the frequency counts for even and odd backtracking integers

even_frequency <- sum(backtracks_df$start %% 2 ==0)
odd_frequency <- sum(backtracks_df$start %% 2 ==1)

even_odd_backtrack <- c(Even = even_frequency, Odd = odd_frequency)

print(even_odd_counts)