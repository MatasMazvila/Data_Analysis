####################################
## Lab 6
## 2022-03-22
## Written by Matas
####################################


setwd("C:/Users/DELL i5/OneDrive/Desktop/Statistics/Programming")

dataG1G2 <- read.table("DataG1G2.txt", header = TRUE)

# Group 1
y1 <- dataG1G2$G1

# Group 2
y2 <- dataG1G2$G2

### Function
posterior_mean_var <- function(y, var){
  ## Inputs:
  # y = data
  # var= variance of normally distributed data
  
  ## Outputs:
  # mp = posterior mean
  # var_p = posterior variance
  
  mp <- mean(y) # posterior mean
  
  var_p <- var / length(y) # posterior var
  
  ### Plot
  
  # Possible value of mu
  mu <- seq(mp - 10 * sqrt(var_p), mp + 10 * sqrt(var_p), 0.1)
  
  # Prior density
  y_prior <- rep(1, length(mu))
  
  # Posterior density
  y_posterior <- dnorm(mu, mp, sqrt(var_p))
  
  # Plot:
  par(mar = c(5, 5, 5, 5)) #Space for each side
  
  plot(mu, y_prior, col = "red", ylab = "Prior Density", type = "l") #prior
  
  par(new = TRUE)
  
  plot(mu, y_posterior, col = "blue", type = "l", axes = FALSE, ylab = " ") # posterior
  axis(side = 4)
  
  mtext("Posterior Density", side = 4, line = 3)
  
  legend("topright", legend = c("Prior", "Posterior"),
         col = c("red", "blue"),
         lty = c(1,1)
         )
  
  ## Output:
  results_p <- c(mp, var_p)
  
  return(results_p)
  
}

m1 <- posterior_mean_var(y1, 100^2)[1]
v1 <- posterior_mean_var(y1, 100^2)[2]

m2 <- posterior_mean_var(y2, 100^2)[1]
v2 <- posterior_mean_var(y2, 100^2)[2]


##### H0: mu0 <= 299 720
##### H1: mu0 > 299 720

m <- 299720

pnorm(m, m1, sqrt(v1)) # G1 -> reject the H0

pnorm(m, m2, sqrt(v2)) # G2 -> reject the H0

################# 2-sided test ------------------*
### 95% CI
##### H0: mu0  = 299 720
##### H0: mu0 != 299 720

dlow <- qnorm(0.025, m1, sqrt(v1))
dup <- qnorm(0.975, m1, sqrt(v1))

BCI_G1 <- c(dlow, dup)
BCI_G1 # -> reject the H0

## Group 2
dlow2 <- qnorm(0.025, m2, sqrt(v2))
dup2 <- qnorm(0.975, m2, sqrt(v2))

BCI_G2 <- c(dlow2, dup2)
BCI_G2 # -> reject the H0

################### md ---------------------
md <- m1 - m2
md

## Posterior variance

vd <- v1 + v2
vd


# H0: md  = 0
# H1: md != 0

BCI_d <- c(qnorm(0.025, md, sqrt(vd)),
         qnorm(0.975, md, sqrt(vd))
)
BCI_d

############# 1-sided test ---------------
## H0: d <= 0
## H1: d > 0

pnorm(0, md, sqrt(vd)) #-> rekect H0

##############################
##############################
y1s <- sum((y1 - mean(y1))^2)
y2s <- sum((y2 - mean (y2))^2)

# Common variance
var12 <- (y1s + y2s) / (length(y1) + length(y2) - 2)

# Group 1:
m1 <- posterior_mean_var(y1, var12)[1]
v1 <- posterior_mean_var(y1, var12)[2]

# Group 2:
# Group 1:
m2 <- posterior_mean_var(y2, var12)[1]
v2 <- posterior_mean_var(y2, var12)[2]

############ 1-sided test
## H0: mu0 <= 299 720
## H1: mu0 > 299 720
m <- 299720


df1 <- length(y1) - 1

T1 <- (m - m1) / sqrt(v1)

pt(T1, df1) #-> reject H0

################ 2-sided test: 5%
## H0: mu0  = 299 720
## H1: mu0 != 299 720

tc1 <- qt(0.975, df1)

BCI <- c(m1 - tc1 * sqrt(v1),
        m1 + tc1 * sqrt(v1)) #-> reject H0





