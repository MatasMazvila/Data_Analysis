#########################
## Lab 3
## 2022-02-22
## Written by Matas
#########################


### Q1 ------------------

## Find P(X < 5)
## P(X < q) = pexp(q, rate)
P5 <- pexp(5, 0.25)

## Find P(X < 4)
P4 <- pexp(4, 0.25)

## Find P(4 < X < 5)
P45 <- P5-P4
P45

round(P45, digits=2)


### Q2 ------------------
# P (X < 5 + 10 | X > 5) = P(X<10)

## Find P(5 < X <5+10)
A1 <- pexp(15, 0.25) - pexp(5, 0.25)

## Find P(X > 5)
A2 <- 1-pexp(5, 0.25)

R1 <- A1/A2

R2 <- pexp(10, 0.25)

R1 - R2


### Q3 ------------------
## pnorm(q, mean, sd)
## Find P(0 < Z < 1.52)

P1 <- pnorm(1.52, 0, 1) - 0.5
# pnorm(1.52, 0, 1) - pnorm(0, 0, 1)


## Find P(Z>2.11)
P2 <- 1 - pnorm(2.11, 0, 1)

## Find P(-1.45<Z<1.74)
P3 <- pnorm(1.74, 0, 1) - pnorm(-1.45, 0, 1)
P3

## Find 2.5 percentile and 97.5 percentile

# 2.5
qnorm(0.025, 0, 1)

# 97.5
qnorm(0.975, 0, 1)

### Q4 -------------------
## punif(q, min, max)

P1 <- punif(60, 0, 60) - punif(57, 0, 60)
P1

### Q5 -------------------
a <- 10
b <- 12

Ey <- a / (a+b)
Ey

Vary <- (a * b) / ((a + b)^2 * (a + b + 1))
Vary

## Generate a sample of obs. from bera(10,12)
Y <- rbeta(1000, a, b)
Y

M <- mean(Y)
M

V <- var(Y)
V

hist(Y, freq = FALSE, main = "Density") #sample 1000 obs. from beta(10,12)
curve(dnorm(x, Ey, sqrt(Vary)), add = TRUE, col = "red", lty = 4) #norm. gp.
curve(dbeta(x, a, b), add = TRUE, col = "green", lty = 3) #beta(10,12)
legend("topleft",
       col = c("red", "green"),
       lty = c(4,3),
       legend = c("Normal approximation","Beta(10, 12)"))

P1 <- 1 - pnorm(5, Ey, sqrt(Vary))
P1

P2 <- 1 - pbeta(5, a, b)
P2


### Q6 ----------------

# P(Y<130)
P1 <- pnorm(130, 120, sqrt(64))
P1

# P(Y<135)
P2 <- pnorm(135, 120, sqrt(64))
P2

# P(114<Y<127)
P3 <- pnorm(127, 120, sqrt(64)) - pnorm(114, 120, sqrt(64))
P3


### Plot
pdf("densnorm.pdf")
set.seed(11)
y <- rnorm(1000, mean = 120, sd = sqrt(64))

hist(y, freq = FALSE)
curve(dnorm(x, 120, sqrt(64)),add = TRUE, col = "cyan")
dev.off()

### Q7 ----------------
r <- 12
v <- 4

#Mean
Ey <- r / v
Vary <- r / v^2

## Find P(Y<4)
P1 <- pgamma(4, r, v)
P1

## Find P(Y<4), using Normal
P2 <- pnorm(4, Ey, sqrt(Vary))
P2