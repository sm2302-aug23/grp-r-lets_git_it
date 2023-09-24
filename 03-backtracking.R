# Task 3 --------------------------------------------------------

#1 ----------------------------------------------
#Starting integer for backtrack data frame

library(tibble)
library(dplyr)

has_backtrack <- function(seq) {
  seq_length <- length(seq)
  if (seq_length < 3) {
    return(FALSE)
  }

  above_starting_value <- FALSE
  
# Iterate through the sequence starting from the second element
  for (i in 2:(seq_length - 1)) {
    if (seq[i] < seq[1] && seq[i + 1] > seq[i]) {
      above_starting_value <- TRUE
    }
    
    if (above_starting_value && seq[i] > seq[1]) {
      return(TRUE)  # The sequence has gone above the starting value more than once
    }
  }
  
  # If the loop completes without returning TRUE, it means the condition was not met
  return(FALSE)
  
}

backtracks_df <- collatz_df %>%
  group_by(start) %>%
  filter(any(sapply(seq, has_backtrack))) %>%
  ungroup()

head(backtracks_df)
print(backtracks_df)

#2 -------------------------------------------
#The most frequently occurring number of times they go above their starting integer.

mode_backtrack <- backtracks_df %>%
  group_by(start) %>%
  summarise(
    most_common_count = max(table(sapply(seq, function(s) sum(s > start))))
  )

mode_backtrack <- mode_backtrack$most_common_count[which.max(mode_backtrack$most_common_count)]
print(mode_backtrack)


#3 -----------------------------------------
#maximum value reached after the first backtrack for these sequences

max_after_backtrack <- pmax(backtracks_df$max_val)

head(max_after_backtrack)
print(max_after_backtrack)

#4 -----------------------------------------
#the frequency counts for even and odd backtracking integers

even_frequency <- sum(backtracks_df$start %% 2 == 0)
odd_frequency <- sum(backtracks_df$start %% 2 != 0)

even_odd_backtrack <- c(even_frequency, odd_frequency)

print(even_odd_backtrack)