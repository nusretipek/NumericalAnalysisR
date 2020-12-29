##################################################################################
# Edit Following Function/Parameters
##################################################################################

rm(list = ls())

#Options
options(scipen = 999)
options(digits =10)

#Function - Initial Values

f1 <- quote((exp(x)/x)-(sin(x)/x^2)+2*x-10)
Start <- 10

#Another Example: quote((k/2)*y^2+(2/5)*kk*y^(5/2)-m*g*y-m*g*h)
#Initial Values -> list(y=Start, k=40000, kk=40, m=95, g=9.81, h=0.43)

#Parameters

maxiteration <- 20

##################################################################################
# WARNING: Editting the following code may reuslt with crash! Use caution
##################################################################################

InitialValueList <- list(x=Start)

Derivative <- D(f1, "x")
f1_a = eval(f1, InitialValueList)[1]
Df1_a = eval(Derivative, InitialValueList)[1]
results <- data.frame(Iteration = numeric(maxiteration), x = numeric(maxiteration), NewX = numeric(maxiteration),
                      fx = numeric(maxiteration), fDx = numeric(maxiteration))
value <- Start
for(i in 1:maxiteration){
   InitialValueList[[1]] <- value
   f1_a = eval(f1, InitialValueList)[1]
   Df1_a = eval(Derivative, InitialValueList)[1]
   NewX <- value - (f1_a/Df1_a)
   results[i,] <- list(i, value, NewX, f1_a, Df1_a)
   value <- NewX
   if(i == maxiteration){print(results)}}
