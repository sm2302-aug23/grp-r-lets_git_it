gen_collatz <- function(n) {
  if (!is.numeric(n) || n <= 0 || !is.integer(n)) {
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

collatz_df <- tibble(start = 1:10000, seq = list(rep(NA_real_, 1)),
                     length = NA_real_, parity = NA_character_, 
                     max_val = NA_real_)
for(i in 1:10000) {
  collatz_seq <- gen_collatz(i)
  collatz_df[i, c("seq", "length", "parity", "max_val")] <- list(
    list(collatz_seq), length(collatz_seq),
    ifelse(i %% 2 == 0, "Even", "Odd"),
    max(collatz_seq)
  )
}
head(collatz_df)
print(collatz_df)

# Task 2------------------------------------------------------------

#1 ------------------------------------------------------------------

top10longest <- collatz_df %>%
  arrange(desc(length)) %>%
  head(10)
head(top10longest)

#2 -----------------------------------------------------------------
max_val_int <- collatz_df %>%
  filter(max_val == max(max_val)) %>%
  select(start, max_val)
head(max_val_int)

#3 ------------------------------------------------------------------

# For even integers

summary_stats <- collatz_df %>%
  mutate(is_even = start %% 2 == 0) %>%
  group_by(is_even) %>%
  summarise(
    avg_length = mean(length),
    sd_length = sd(length)
  )
head(summary_stats)

# For odd integers 

summary_stats_odd <- collatz_df %>%
  filter(start %% 2 == 1) %>%
  summarise(
    avg_length_odd = mean(length),
    sd_length_odd = sd(length)
  )
head(summary_stats_odd)



