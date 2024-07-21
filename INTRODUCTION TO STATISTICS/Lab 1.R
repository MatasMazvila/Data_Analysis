x1 <- matrix(1:20, nrow = 5)
x1
x2 <- matrix(1:20, ncol = 4)
x2
x3 <- matrix(1:20, nrow = 5, byrow = TRUE)
x3
x1 + x2
dim(x1)
?dim(x1)
t(x1) %*% x2 #matrix product
# Combine matrix
cbind(x1, x2)
rbind(x1, x2)
# inverse of matrix
x4 <- matrix(c(1,5,6, 4,8,9, 6,8,3),nrow = 3)
x4
dim(x4)
solve(x4)
x4[1,2]
x4[3,1]
x4[3.1] <- 60
x4
x4[3,1] <- 70
x4
x1
x4
x1 + x4
cbind(x1 , x2[,1])
rbind(x1, x2[,1])
# writing a function
# name <- function(input_arg){expression}
# square function
square_fun <- function(x){
  x^2
}

square_fun(5)
square_fun(9)

add(4,5)
add(4, 5)

add(4,5)
add(4, 5)
add <- function(a, b){
  return(a + b)
}

#for(me in expr1){expr2}
v <- 1:10  
#z0 = [1^2, 2^2, ..., 10^2]

z0 <- rep(0,10)
for(i in 1:10) {
 z0[i] <- i^2
}
z0
y <- rep(0,10)
v1 <- seq(1, 10, 2)
v2 <- seq(2, 10, 2)

for (i in v1){
  y <- i^3
}
for(j in v2) {
  y[j] <- j^2
}
y <- rep(0, 10)

i <- 1

z1 <- rep(0, 10)
while (i < 5) {
  z1[i] <- i^3
  i <- i+1
}
y <- 1
# if (expr1) expr2 else expr 3
#logical operators <, <=, >, >=, ==< != foe inequality
cc <- 1
if(cc == 2) dd <- 4 else dd <- 5
dd