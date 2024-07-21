setwd("C:/Users/DELL i5/OneDrive/Desktop/Statistics/Programming")
binbeta_plot(1,1,100,20)

source("binbeta_fun.R")
source("binbeta_fun2.R")
source("binbeta_plot.R")
source("binbeta_plot2.R")

binbeta_fun(1,1,1,1,0.05)
binbeta_fun(1,1,9,6,0.05)

binbeta_plot(1,1,1,1,"topright")
binbeta_plot(1,1,9,6,"topright")

a <- 12
b <- 12
n <- 1000
y <- 250
alpha <- 0.05

binbeta_fun(a, b, n, y, alpha)
binbeta_plot(a, b, n, y, alpha)

binbeta_fun2(12, 12,1000,250,0.05)

res2 <- binbeta_fun2(12,12,1000,250,0.05)
res2$ 'Lower CI'
res2$ 'Upper CI'

binbeta_plot2(12,12,1000,250,
              "topright",
              res2$ 'Lower CI',
              res2$ 'Upper CI')