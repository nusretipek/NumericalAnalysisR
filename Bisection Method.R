##################################################################################
# Edit Following Function/Parameters
##################################################################################

rm(list = ls())

#Options

options(scipen = 999)
options(digits =10)

#Function

f1 <- quote(x^3 - 2*x^2 - 5)

#Parameters

lower <- 2
upper <- 4
maxiteration <- 20

##################################################################################
# WARNING: Editting the following code may reuslt with crash! Use caution
##################################################################################

#Set Environment

f1_a = eval(f1, list(x=lower))
f1_b = eval(f1, list(x=upper))

results <- data.frame(Iteration = numeric(maxiteration), x_a = numeric(maxiteration), x_b = numeric(maxiteration), x_m = numeric(maxiteration),
                      fx_a = numeric(maxiteration), fx_b = numeric(maxiteration), fx_c = numeric(maxiteration))

#Compute

{
if(f1_a*f1_b > 0){ 
  print("There is no root in the range!")}
else{
  for(i in 1:maxiteration){
    midpoint <- (lower + upper)/2
    f1_a = eval(f1, list(x=lower))
    f1_b = eval(f1, list(x=upper))
    f1_c = eval(f1, list(x=midpoint))
    results[i,] <- list(i, lower, upper, midpoint, f1_a, f1_b, f1_c)
    if(f1_a*f1_c < 0){
      upper = midpoint
    }
    else{
      lower = midpoint
    }
  }
}}
print(results)
