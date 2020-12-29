library(Matrix)

rm(list = ls())

A <- matrix(c(6,-2, 3, 4, -10, 4, 1, -3, 5), 3, 3, byrow=T)
B <- matrix(c(1,1,1), nrow = 3)
dim(A) <- c(3, 3)

elu <- expand(lu(A))
L <- elu$L
U <- elu$U
P <- elu$P

all.equal(Matrix(A), with(elu, P %*% L %*% U))

(Y <- solve(L, solve(P) %*% B)) # solve LY = inv(P).B instead of LY = PB
(X <- solve(U, Y))

all.equal(X, Matrix(solve(A, B)))

U <- matrix(c(2, 3, -1, 1, 0, 2, 1, 0, 0, 0, -1, 3, 0, 0, 0, 4), 4, 4, byrow=T)
D <- Diagonal(4)
diag(D) <- (diag(U))
UN <- Diagonal(4)
UN <- solve(D, U %*% solve(UN)) 
UN

f1 <- quote((1+2*y-3*z)/6)
f2 <- quote((2-4*x-4*z)/-10)
f3 <- quote((3-x+3*y)/5)
FunctionList <- list(x=1, y=1, z=1)
f1_a <- eval(f1, FunctionList)[1]
FunctionList <- list(x=f1_a, y=1, z=1)
f1_b <- eval(f2, FunctionList)[1]
FunctionList <- list(x=f1_a, y=f1_b, z=1)
f1_c <- eval(f3, FunctionList)[1]
FunctionList <- list(x=f1_a, y=f1_b, z=f1_c)
print(c(f1_a, f1_b, f1_c))
maxiteration <- 100
results <- data.frame(Iteration = numeric(maxiteration), x = numeric(maxiteration), 
                      y = numeric(maxiteration), z = numeric(maxiteration))
FunctionList <- list(x=1, y=1, z=1)

for(i in 1:maxiteration){
  f1_a <- eval(f1, FunctionList)[1]
  FunctionList <- list(x=f1_a, y=1, z=1)
  f1_b <- eval(f2, FunctionList)[1]
  FunctionList <- list(x=f1_a, y=f1_b, z=1)
  f1_c <- eval(f3, FunctionList)[1]
  FunctionList <- list(x=f1_a, y=f1_b, z=f1_c)
  results[i,] <- list(i, f1_a, f1_b, f1_c)}
print(results)

xfunc <- quote(x-sqrt(x^2-1))
Derivative <- D(xfunc, "x")

f1 <- quote(sin(2*x-y)-1.2*x-0.4)
f2 <- quote(0.8*x^2+1.5*y^2-1)
StartX <- 0.4
StartY <- -0.75
maxiteration <- 20
f1x <- D(f1, "x")
f1y<- D(f1, "y")
f2x <- D(f2, "x")
f2y<- D(f2, "y")
maxiteration <- 10
results <- data.frame(Iteration = numeric(maxiteration), xDelta = numeric(maxiteration), 
                      yDelta = numeric(maxiteration), NewX = numeric(maxiteration), NewY = numeric(maxiteration))
FunctionList <- list(x=StartX, y=StartY)
DeltaX <- 0
DeltaY <- 0

for(i in 1:maxiteration){
  f1xE <- eval(f1x, FunctionList)[1]
  f1yE <- eval(f1y, FunctionList)[1]
  f2xE <- eval(f2x, FunctionList)[1]
  f2yE <- eval(f2y, FunctionList)[1]
  f1E <- eval(f1, FunctionList)[1]
  f2E <- eval(f2, FunctionList)[1]
  DeltaX <- ((f1yE*f2E) - (f1E*f2yE))/((f1xE*f2yE)-(f1yE*f2xE))
  DeltaY <- ((f1E*f2xE) - (f2E*f1xE))/((f1xE*f2yE)-(f1yE*f2xE))
  results[i,] <- list(i, DeltaX, DeltaY, FunctionList[[1]]+DeltaX, FunctionList[[2]]+DeltaY)
  FunctionList <- list(x=FunctionList[[1]]+DeltaX, y=FunctionList[[2]]+DeltaY)}
print(results)



