# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(tibble)
library(dplyr)

# task 4 -----------------------------------------------------------------

#1 -----------------------------------------------------------------------  

# scatterplot

ggplot(
  data    = backtracks_df,
  mapping = aes(x = start,
                y = length)
) + geom_point() +
  labs(
    title = "Scatter plot 1",
    x     = "Starting Integers",
    y     = "Length of the sequence"
  )

# Identify the top 10 starting integers

top_10_starting_integers <- backtracks_df %>%
  group_by(start) %>%
  summarise(total_length = sum(length)) %>%
  arrange(desc(total_length)) %>%
  select(start)  # i think u missed %>% here
  head(10)

# Print the top 10 starting integers
  
print(top_10_starting_integers)


#2 -----------------------------------------------------------------------

# create a new function for the top 10 starting integers

top_10_starting_integers_02 <- backtracks_df %>%
  group_by(start) %>%
  summarise(total_max_val = sum(max_val)) %>%
  arrange(desc(total_max_val)) %>%
  select(start) %>%
  head(10)
  
# print the top 10 starting integers for the maximum value

print(top_10_starting_integers_02)

# creating a new variable 

backtracks_df <- backtracks_df %>%
  mutate(top_10 = start %in% top_10_starting_integers_02$start)

# create 10 distinct colours 

colour_palette <- c(
  "cyan", "pink", "orange", "brown", "violet",
  "green", "yellow", "blue", "magenta", "cyan4"
  )

# scatterplot 02 

ggplot(
  data    = collatz_df,
  mapping = aes(x = start,
                y = max_val,
                colour = top_10)
          
) +
  geom_point() +
  labs(
    title    = "Scatter plot 2",
    x        = "Starting integers",
    y        = "Maximum value reached in the sequence"
  ) +
  scale_color_manual(values = colour_palette)


#3 -----------------------------------------------------------------------

# Create a boxplot comparing sequence lengths for even and odd starting integers
