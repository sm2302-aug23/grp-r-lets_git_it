# Task 4 ----------------------------------------------
library(tidyverse)
library(ggplot2)
library(tibble)


ggplot(
  data = backtracks_df,
  mapping = aes(x = start,
                y = length)
) + geom_point() +
  labs(
    title = "Scatter plot 1",
    x = "Starting Integers",
    y = "Length of the sequence"
  )

# Identify the top 10 starting integers
top_10_starting_integers <- backtracks_df %>%
  group_by(start) %>%
  summarise(total_length = sum(length)) %>%
  arrange(desc(total_length)) %>%
  select(start)
  head(10)

# Print the top 10 starting integers
print(top_10_starting_integers)


#2 ----------------------------------------------------------




#3 --------------------------------------------------------

# Create a boxplot comparing sequence lengths for even and odd starting integers
