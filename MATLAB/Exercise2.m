%This script derives formulas for OLS estimators when we have two
%regressors (x1 and x2), using the symbolic toolbox

clear

%Symbolic variables for variances and covariances
syms cwy czy cwx czx vw czw

%Variance-covariance matrix of x1 and x2
A = [cwx vw; czx czw];
%Covariances with y
B = [ cwy; czy];

%Use matrix algebra to solve for b1_hat and b2_hat:
My_betas = (A^-1)*B;
My_betas