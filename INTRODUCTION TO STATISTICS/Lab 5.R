####################################
## Lab 5
## 2022-03-16
## Written by Matas
####################################



## Question 1 ---------------------------
## 1) --------------------------

# Load the data
y <- c(64, 72, 84, 73, 98, 85, 85, 94, 72, 93)

# Prior mean
m <- 80

# Prior SD
s <- 10

# Match prior belief with parameters
r <- m^2 / s^2
v <- m/s^2 

prior_fun <- function(m, s){
  r <- m^2 / s^2
  v <- m/s^2 
  
  return(c(r,v))
}

prior_fun(95,20)

## 2) ----------------------------
sum_y <- sum(y)
n <- length(y)

# Posterior
r1 <- r + sum_y
v1 <- v + n

posterior_fun <- function(m, s){
  # Prior p.
  r <- m^2 / s^2
  v <- m/s^2 
  
  # Likelihood
  sum_y <- sum(y)
  n <- length(y)
  
  # Posterior
  r1 <- r + sum_y
  v1 <- v + n
  
  return(c(r1, v1))
}

pos <- posterior_fun(85, 10)

pos[1] #r1
pos[2] #v1

## 3) ----------------------------

pos_mean <- r1 / v1
pos_var <- r1 / v1^2

pri_w <- v / (v + n)
pri_m <- r / v

data_w <- n / (v + n)
data_m <- sum_y / n

pri_w * pri_m + data_w * data_m - pos_mean

## 5) ----------------------------

low <- qgamma(0.025, r1, v1)
up <- qgamma(0.975, r1, v1)

CI_fun <- function(m, s){
  # Prior p.
  r <- m^2 / s^2
  v <- m/s^2 
  
  # Likelihood
  sum_y <- sum(y)
  n <- length(y)
  
  # Posterior
  r1 <- r + sum_y
  v1 <- v + n
  # CI
  low <- qgamma(0.025, r1, v1)
  up <- qgamma(0.975, r1, v1)
  
  return(c(low, up))
}

CI_fun(80, 10)

## 6) ----------------------------

pgamma(75, r1, v1)

### Plot ----------------------------

# Specify mu values for gamma
mu <- seq(0, 150, 0.1)

# Calculate the posterior density
mu_post <- dgamma(mu, r1, v1)

# Plot the posterior density
plot(mu, mu_post)


## Question 2 ---------------------------

priors <- prior_fun(6 ,2)

prior_fun <- function(m, s){
  r <- m^2 / s^2
  v <- m / s^2
  
  return(c(r,v))
}

r <- priors[1]
v <- priors[2]

sum_y <- 71
n <- 100 / 10

r1 <- r + sum_y
v1 <- v + n
posterior_fun2 <- function(m, s, sum_y = 71, n = 10){
  # Prior p.
  r <- m^2 / s^2
  v <- m/s^2 
  
  # Posterior
  r1 <- r + sum_y
  v1 <- v + n
  
  return(c(r1, v1))
}

posterior_fun2(6, 2)

## 5) ---------------------------
qgamma(0.025, r1, v1)
qgamma(0.975, r1, v1)