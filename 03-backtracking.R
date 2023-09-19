# Task 3 --------------------------------------------------------

#1 ----------------------------------------------
library(tibble)
library(dplyr)

mode_backtrack <- function(seq) {
  seq_length <- length(seq)
  if (seq_length < 3) {
    return(FALSE)
  }
  
  for (i in 2 :(seq_length - 1)) {
    if (seq[i] < seq[1] && seq[i+1] > seq[i]) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

backtracks_df <- collatz_df %>%
  filter(map_lgl(seq, mode_backtrack))

head(backtracks_df)
