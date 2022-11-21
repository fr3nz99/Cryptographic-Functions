Points of an Elliptic Curve
================
Francesco Pinto

## Finding the points on an elliptic curve

In order to show how this function works, I’m gonna find the points on
the elliptic curve

![y^2 = x^3 - x - 3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y%5E2%20%3D%20x%5E3%20-%20x%20-%203 "y^2 = x^3 - x - 3")

Iterating over the values of
![Z\_{11}=\\{0,1,2,3,4,5,6,7,8,9,10\\}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Z_%7B11%7D%3D%5C%7B0%2C1%2C2%2C3%2C4%2C5%2C6%2C7%2C8%2C9%2C10%5C%7D "Z_{11}=\{0,1,2,3,4,5,6,7,8,9,10\}")

``` r
#Polynomial of the elliptic curve
poly = function(x) x^3 - x -3



for(x in 0:10){ #I iterate over all the Z_11 elements

  
  #If the result is >=0, I check the condition explained above
  if(poly(x)>= 0){
    for(i in 0:10){
      
      #i check if the sqrt returns an integer
      if(sqrt(i*11 + (poly(x))%%11) == round(sqrt(i*11 + (poly(x))%%11))){
        
        
        #If the condition above are satisfied, I print the generated points
        print(paste('x = ', x, ', y = ', sqrt(i*11 + (poly(x))%%11)))

      }
    }
  }
  
}
```

    ## [1] "x =  2 , y =  5"
    ## [1] "x =  2 , y =  6"
    ## [1] "x =  6 , y =  3"
    ## [1] "x =  6 , y =  8"
    ## [1] "x =  7 , y =  5"
    ## [1] "x =  7 , y =  6"
