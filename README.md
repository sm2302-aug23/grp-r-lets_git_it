## 1) Generating the Collatz Conjecture 
For generating 
## 2) Exploratory data analysis 

## 3) Investigating "backtracking" in sequences

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

# A tibble: 2 × 2
  parity count
  <chr>  <int>
1 Even    1719
2 Odd     2065

Although there are even integers producing large sequences, the amount of odd integers (n = 2065) is more than the number of even integers (n = 1719). Therefore, the hypothesis of this finding is proven and it is true that odd integers produces larger sequences compared to even integers. 


## 6) Creative visualisation challenge 