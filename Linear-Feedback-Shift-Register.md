Linear Feedback Shift Register
================
Francesco Pinto

## Function to shift the LFSR

To implement a Linear Feedback Shift Register I first need a function
that shifts my LFSR:

``` r
shift.vector <- function(x, shift = 0, fill = FALSE) {
  n          <- length(x)
  
  if (shift > 0) {
    x        <- c(rep(fill, shift), x[-((n - shift + 1):n)])
  }
  if (shift < 0) {
    x        <- c(x[-(1:abs(shift))], rep(fill, abs(shift)))
  }
  x
}
```

\[this function is taken from
<https://github.com/timriffe/DemoTools/blob/master/R/utils.R>\]

## Ececution of the LFSR

``` r
lfsr = function(state, taps){
    #1. I create a copy of the given initial state (I will need the last bit to do the XOR
    # with the bits in the taps)
    initial = state 
    
    #2. I shift the vector of one place to the right
    state = shift.vector(state,1) 
    
    #3. if the xor of the given taps of the lfsr is 0, then the new entry is 0
    if(sum(initial[taps])%%2==0) state[1] = 0 

    #3'. if the xor of the given taps the lfsr is 1 then the new entry is 1
    if(sum(initial[taps])%%2==1) state[1] = 1
                                            
    return(state)
}
```

Now I’m gonna iterate the LFSR over all the possible initial states with
the following function:

``` r
#combinations is the entire set of combinations of {0,1}^n
#taps is the position of taps to apply the XOR


check_maximum = function(combinations, taps){ 
  #1. n is the number of all possible initial states 
  n = nrow(combinations) 
  
  #2. With the following loop I will repeat the cycle until the state of the LFSR returns
  #back to the initial state
  for(i in 1:n){
    
    
    #[unlist is a function to "clean" the result (not important now)]
    initial_state = unlist(combinations[i,], use.names = F)
    
    #2.1 I create a copy to be modified in the operations
    initial_state2 = initial_state 
    
    #2.2 I append the first output to the vector of the outputs
    vector = c(initial_state[5]) 
    
    #2.3 I Show the initial state ant the consequent cycles
    print(paste0('The initial state is ', paste(initial_state,collapse = ' ')))

    #3. As we know, the maximum length of a cycle is 2^n - 1, so I'm gonna iterate 
    # 2^n - 1 times
    for(j in 1:((2^5 - 1))){ 
      
      #3.1 I apply the LFSR over every state
      initial_state2 = lfsr(initial_state2, taps)
      
      #3.2 This vector contains all the outputs
      vector = append(vector, initial_state2[5])
      
      #3.3 I print every state 
      print(initial_state2)
      
      #3.4 If I see that I've reached the initial state, I'm gonna print the number of steps
      if(paste(initial_state2,collapse = ' ') == paste(initial_state,collapse = ' ')){
        print(paste0('stop, the cycle has ', j, ' steps'))
        break
      }
    }
  }
}
```

## Example of using the function

I choose to take a LFSR with length = 5.  
All the combinations of hypothetical initial states are:

``` r
combinations = expand.grid(0:1, 0:1, 0:1, 0:1, 0:1)
combinations
```

    ##    Var1 Var2 Var3 Var4 Var5
    ## 1     0    0    0    0    0
    ## 2     1    0    0    0    0
    ## 3     0    1    0    0    0
    ## 4     1    1    0    0    0
    ## 5     0    0    1    0    0
    ## 6     1    0    1    0    0
    ## 7     0    1    1    0    0
    ## 8     1    1    1    0    0
    ## 9     0    0    0    1    0
    ## 10    1    0    0    1    0
    ## 11    0    1    0    1    0
    ## 12    1    1    0    1    0
    ## 13    0    0    1    1    0
    ## 14    1    0    1    1    0
    ## 15    0    1    1    1    0
    ## 16    1    1    1    1    0
    ## 17    0    0    0    0    1
    ## 18    1    0    0    0    1
    ## 19    0    1    0    0    1
    ## 20    1    1    0    0    1
    ## 21    0    0    1    0    1
    ## 22    1    0    1    0    1
    ## 23    0    1    1    0    1
    ## 24    1    1    1    0    1
    ## 25    0    0    0    1    1
    ## 26    1    0    0    1    1
    ## 27    0    1    0    1    1
    ## 28    1    1    0    1    1
    ## 29    0    0    1    1    1
    ## 30    1    0    1    1    1
    ## 31    0    1    1    1    1
    ## 32    1    1    1    1    1

The code generated would print more than two pages of code, so I’m gonna
show you the output obtained by running the algorithm on **0 0 0 0 0**
(that has only one step \[itself\]) and the chosen state **1 0 1 1 0**:

``` r
check_maximum(combinations[1,], c(1,3,5)) #0 0 0 0 0
```

    ## [1] "The initial state is 0 0 0 0 0"
    ## [1] 0 0 0 0 0
    ## [1] "stop, the cycle has 1 steps"

``` r
check_maximum(combinations[14,], c(1,3,5)) #1 0 1 1 0
```

    ## [1] "The initial state is 1 0 1 1 0"
    ## [1] 0 1 0 1 1
    ## [1] 1 0 1 0 1
    ## [1] 1 1 0 1 0
    ## [1] 1 1 1 0 1
    ## [1] 1 1 1 1 0
    ## [1] 0 1 1 1 1
    ## [1] 0 0 1 1 1
    ## [1] 0 0 0 1 1
    ## [1] 1 0 0 0 1
    ## [1] 0 1 0 0 0
    ## [1] 0 0 1 0 0
    ## [1] 1 0 0 1 0
    ## [1] 1 1 0 0 1
    ## [1] 0 1 1 0 0
    ## [1] 1 0 1 1 0
    ## [1] "stop, the cycle has 15 steps"
