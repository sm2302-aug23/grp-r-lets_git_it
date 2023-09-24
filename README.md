#### Contribution declaration

-   Task 1: @author1
-   Task 2: @author2
-   Task 3: @author3
-   Task 4: @author2
-   Task 5: @author4
-   Task 6: @author1
-   README: @author3

### Used packages 
1. tidyverse
2. tidyr
3. dplyr
4. tibble 
5. ggplot2

## 1) Generating the Collatz Conjecture 


As instructed, I am creating a function 'gen_collatz' to generate the Collatz
sequence for a given positive integer 'n' and also implementing a safeguard to 
handle invalid input values (non-positive integers). 

```
gen_collatz <- function(n) {
# SAFEGUARDING
  if (n <= 0 || !is.integer(n)) {
    stop("Input must be a positive integer.")
  }
  
  # 
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

collatz_df <- tibble(start = integer(), seq = list(), length = numeric(), 
                     parity = character(), max_val = numeric())

for (i in 1:10000) {
  collatz_seq <- gen_collatz(i)
  collatz_df <- add_row(collatz_df, start = i, seq = list(collatz_seq),
                        length = length(collatz_seq), 
                        parity = ifelse(i %% 2 == 0, "Even", "Odd"), 
                        max_val = max(collatz_seq))
}
head(collatz_df)
print(collatz_df)
``` 

## 2) Exploratory data analysis 

## 3) Investigating "backtracking" in sequences

1. Creating data frame of backtracking within the collatz sequences :

Filter out any sequence length that has less than 3 sequence since there is no backtracking occurs.
```
has_backtrack <- function(seq) {
  seq_length <- length(seq)
  if (seq_length < 3) {
    return(FALSE)
  }

  above_starting_value <- FALSE
```

(i) Iterate through the sequence starting from the second element.
(ii) The sequence has gone above the starting value more than once.
(iii) If the loop completes without returning TRUE, it means the condition was not met.
```
  for (i in 2:(seq_length - 1)) {
    if (seq[i] < seq[1] && seq[i + 1] > seq[i]) {
      above_starting_value <- TRUE
    }
    
    if (above_starting_value && seq[i] > seq[1]) {
      return(TRUE)  
    }
  }
  
 
  return(FALSE)
  
}
```

Create a function of `backtracks_df` to create the data.
```
backtracks_df <- collatz_df %>%
  group_by(start) %>%
  filter(any(sapply(seq, has_backtrack))) %>%
  ungroup()

head(backtracks_df)
print(backtracks_df)
```

Output:
```
backtracks_df
#> #A tibble: 8,229 × 5
#>   start seq        length parity max_val
#>   <int> <list>      <dbl> <chr>    <dbl>
#> 1     6 <dbl [9]>       9 Even        16
#> 2     7 <dbl [17]>     17 Odd         52
#> 3     9 <dbl [20]>     20 Odd         52
#> 4    10 <dbl [7]>       7 Even        16
#> 5    11 <dbl [15]>     15 Odd         52
#> 6    12 <dbl [10]>     10 Even        16
#> 7    13 <dbl [10]>     10 Odd         40
#> 8    14 <dbl [18]>     18 Even        52
#> 9    15 <dbl [18]>     18 Odd        160
#> 10    17 <dbl [13]>     13 Odd         52
#> # ℹ 8,219 more rows
```

2. The most frequently occurring number of times they go above their starting integer.

```
mode_backtrack <- backtracks_df %>%
  group_by(start) %>%
  summarise(
    most_common_count = max(table(sapply(seq, function(s) sum(s > start))))
  )

mode_backtrack <- mode_backtrack$most_common_count[which.max(mode_backtrack$most_common_count)]
print(mode_backtrack)
```

3. The maximum value reached after the first backtrack for these sequences.

Using the `pmax` to find the maximum value of every integers.
```
max_after_backtrack <- pmax(backtracks_df$max_val)
head(max_after_backtrack)

print(max_after_backtrack)
```

4. The frequency counts for even and odd backtracking integers.

```
even_frequency <- sum(backtracks_df$start %% 2 == 0)
odd_frequency <- sum(backtracks_df$start %% 2 != 0)

even_odd_backtrack <- c(even_frequency, odd_frequency)

print(even_odd_backtrack)
```

Output:
```
#> [1] 3943 4286
```

## 4) Visualisations 

## 5) Open-ended exploration

For task 5, we analysed the arithmetic progressions in the amount of stopping times of the Collatz Conjecture. 

The hypothesis is that the starting integer can affect the number of steps it takes a sequence to reach one.

 1) Odd integers tend to produce a larger sequence.

This is because when an odd integer is multiplied by 3 and added by 1, it becomes larger, resulting in more iterations before reaching 1. 

 2) Even integers tend to produce a smaller sequence. 

This is because when an even integer is divided by 2, it immediately becomes smaller, which can lead to a quicker convergence.

Since we want to analyse large sequences, we focused on length of sequences more than 100. Hence, the *length_above_100* data frame. 

Since we only want to compare the parity of even and odd, the odd_even_df data frame is formed. 

```
#> #A tibble: 2 × 2
#>  parity count
#>  <chr>  <int>
#> 1 Even    1719
#> 2 Odd     2065
```

Although there are even integers producing large sequences, the amount of odd integers (n = 2065) is more than the number of even integers (n = 1719). Therefore, the hypothesis of this finding is proven and it is true that odd integers produces larger sequences compared to even integers. 


## 6) Creative visualisation challenge 

For task 6, we were curious about how it task 5 would show on a graph. 

We also wanted to try if we could try to do an interactive visualisation 
that could allow for others to explore the Collatz Conjecture by 
inputting their own starting integers. 