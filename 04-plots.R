# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(tibble)
library(dplyr)

# task 4 -----------------------------------------------------------------

#1 -----------------------------------------------------------------------  

# scatterplot

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

top_10_starting_integers_01 <- backtracks_df %>%
  group_by(start) %>%
  summarise(total_length = sum(length)) %>%
  arrange(desc(total_length)) %>%
<<<<<<< Updated upstream
  select(start) %>%  
  head(10)

print(top_10_starting_integers)
=======
  select(start) %>%
  head(10)

# Print the top 10 starting integers
  
print(top_10_starting_integers_01)
>>>>>>> Stashed changes

backtracks_df <- backtracks_df %>%
  mutate(top_10 = start %in% top_10_starting_integers_02$start)

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

# create 10 distinct colours 

top_10_colors <- c(
  "start_int_1" = "pink",
  "start_int_2" = "cyan4",
  "start_int_3" = "red",
  "start_int_4" = "maroon",
  "start_int_5" = "blue",
  "start_int_6" = "yellow",
  "start_int_7" = "gold",
  "start_int_8" = "violet",
  "start_int_9" = "brown",
  "start_int_10" = "green"
)

# scatterplot 02 

ggplot(
  data = backtracks_df,
  mapping = aes(x = start,
                y = max_val,
                color = factor(start, levels = top_10_starting_integers_02$start)
                )
          
) +
  geom_point(
    size = 3
  ) +
  labs(
    title = "Scatter plot 2",
    x = "Starting integers",
    y = "Max value reached in the sequence"
  ) +
  scale_color_manual(values = top_10_colors)


#3 -----------------------------------------------------------------------

# Create a boxplot comparing sequence lengths for even and odd starting integers
