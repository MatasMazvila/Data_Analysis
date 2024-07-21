############################
## Lab 2
## 2022-02-22
## Written by Matas
############################

# Create the values of y
y <- c(1:5)
y

# Probability
py <- c(0.1, 0.2, 0.1, 0.2, 0.4)
py

# E(Y)
my <- sum(y * py)
my

# Var(Y) = E(Y^2) - E(Y)^2
var_y <- sum(y^2 * py) - my^2
var_y

size <- 10^5

# Set the seed of random simulation
set.seed(123)

ys <- sample(y, size, replace = TRUE, prob = py)
ys
head(ys)

table(ys) / size


## P(1<Y<5) --------------

d2 <- which(ys == 2)
d3 <- which(ys == 3)
d4 <- which(ys == 4)

P2 <- (length(d2)+length(d3)+length(d4))/length(ys)


# mean Y
ms <- mean(ys)
ms

vars <- var(ys)
vars

## W
W <- 2 * ys + 5

mean(W)
var(W)

# E(W+Y)
mwy <- mean(W+ys)
var_wy <- var(W+ys)

### Q2 ---------------
x <- seq(0, 4, 1)
x

prior_x <- rep(1/5, 5)
prior_x

# Likelihood
likelihood <- rep(0, 5)
likelihood

for(i in 0:4){
 likelihood[i+1] <- i/4
}
likelihood

# Prior * Likelihood
weight_x <- prior_x * likelihood
weight_x

# Posterior
posterior_x <- weight_x / sum(weight_x)
posterior_x

# Plot posterior
plot(x, posterior_x, # coordinates of x and y
type = "l",
col = "blue",
ylim = c(0, 1),
ylab = "y")

lines(x, prior_x,
      col = "purple")

# Table of results
results1_Q2 <- cbind(x, prior_x, likelihood, weight_x, posterior_x)
results1_Q2

colnames(results1_Q2)[5] <- c("Posterior_x")

## Q2, part 2 ------------
a <- seq(0, 4, 1)

prior_a <- rep(1/5, 5) * 2
prior_a

weight_a <- prior_a * likelihood
posterior_a <- weight_a / sum(weight_a)
posterior_a


b <- seq(0, 4, 1)

likelihood_b <- likelihood * 4

weight_b <- prior_x * likelihood_b

posterior_b <- weight_b / sum(weight_b)
posterior_b

## Q4 --------------------

mu <- c(1, 1.5, 2, 2.5)
prior_mu <- c(2/6, 1/6, 2/6, 1/6)

likelihood_mu <- rep(0, 4)

for(i in 1:4){
  likelihood_mu[i] <- dpois(1, mu[i])
}

weight_mu <- prior_mu * likelihood_mu
posterior_mu <- weight_mu / sum(weight_mu)

## Q5 ---------------------

pi <- c(0.3, 0.5)

prior_pi <- c(0.6, 0.4)

likelihood_pi <- rep(0, 2)

for(i in 1:2){
  likelihood_pi[i] <- dbinom(2, 3, pi[i])
}

