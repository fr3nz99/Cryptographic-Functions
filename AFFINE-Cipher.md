AFFINE-Cipher
================
Francesco Pinto
11/21/2022

## Associating letters to numbers

-First of all I create a function that maps every number to the
correspondent letter (it will be useful for the next function):

``` r
letter_to_number = function(numbers){
  #"letters" is the built-in list of all letters of the alphabet in R
  alphabet = letters 
  
  #I initialize a vector that will contain the letters
  text = c() 
  for(i in numbers){
    #I append to the vector the number corresponding to every letter
    text = append(text, alphabet[i+1]) 
    
  }
  return(text)
}
```

Now, in order to decrypt the ciphertext, I will try all the possible
combinations of
![\\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha "\alpha")
and
![\\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta")
(where
![\\alpha = 9](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha%20%3D%209 "\alpha = 9")).

The used decryption rule is

![D(x) = \\alpha^{-1} (E(x) - \\beta) mod(26)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;D%28x%29%20%3D%20%5Calpha%5E%7B-1%7D%20%28E%28x%29%20-%20%5Cbeta%29%20mod%2826%29 "D(x) = \alpha^{-1} (E(x) - \beta) mod(26)")

The multiplicative inverse of
![\\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha "\alpha")
is
![\\alpha^{-1} = 3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha%5E%7B-1%7D%20%3D%203 "\alpha^{-1} = 3")

To do it, I use the following code:

``` r
decryption = function(ciphertext, alpha_minus_1){

  #The following list contains all the possible values for beta 
  #(such that gcd(beta,26) = 1):
  betas = c(1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25)
  
  
  
  for(beta in betas){
    #D(x) = a^-1 * (E(x) - b) mod(26):
      D = ((alpha*(ciphertext - beta))%%26) 
 
      #In the following way I print all the possible decrypted texts
      print(beta)
      print(paste(letter_to_number(D),collapse = ''))
  }
}
```
