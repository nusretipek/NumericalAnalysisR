##################################################################################
# Edit Following Function/Parameters
##################################################################################

rm(list = ls())

#Options

options(scipen = 999)
options(digits =10)

#Function: Enter 0 Function in the f1 and Range, then use xfunc to enter formula leving x alone!

f1 <- quote(sin(x)-6*x+15)
#Another Example: quote(((68.1*9.81)/x)*(1-exp((-10*x)/68.1))-40)

#Parameters: If a starting point specified enter instead of 99999 else keep it like that.

lower <- 1.9
upper <- 2
Start <- 99999
xfunc <- quote((sin(x)+15)/6)
maxiteration <- 20

##################################################################################
# Run Below for function Root Availablity Check 
##################################################################################

f1_a <- eval(f1, list(x=lower))[1]
f1_b <- eval(f1, list(x=upper))[1]
if(f1_a*f1_b < 0){print("Continue with Method!")} else{print("Method cannot be used!")}

##################################################################################
# WARNING: Editting the following code may reuslt with crash! Use caution
##################################################################################

Derivative <- D(xfunc, "x")
Df1_a = eval(Derivative, list(x=lower))[1]
Df1_b = eval(Derivative, list(x=upper))[1]
Nf1_a = eval(xfunc, list(x=lower))[1]
Nf1_b = eval(xfunc, list(x=upper))[1]
{if(abs((Nf1_a-Nf1_b)/(lower-upper)) && abs((Nf1_b-eval(xfunc, list(x=Nf1_b))[1])/(upper-Nf1_b)))
  {print("Function is good, results are computed!")
  results <- data.frame(Iteration = numeric(maxiteration), Value = numeric(maxiteration))
  if(Start != 99999){value = Start} else{value = runif(1, lower, upper)}
  for(i in 1:maxiteration){
    value <- eval(xfunc, list(x=value))[1]
    results[i,] <- list(i, value)}
  print(results)} 
else{print("ALERT: Function is not good!")}}
