%This script performs a simple Montercarlo simulation to test properties of
%the basic one-regressor OLS estimator

clear;

%Linear model: parameters
b0 = 3; %intercept
b1 = 7; %slope

N = 50; %number of observations

%Generate simulated data (column vectors of size N-by-1)
%This is the random iid sample
X = rand(N,1); %For the regressor (population follows uniform distribution)
u = normrnd(0,0.5,N,1); %For the unobserved error term (population follows normal distribution)
%type "help rand" and "help normrnd" for details!

%Generate values for dependent variable using the linear model
Y = b0 + b1*X + u;

%Compute the OLS estimator
%formulas (using sample statistics):
%b1_hat = cov(X,Y)/var(X),
%b0_hat = mean(Y) - b1_hat*mean(X),

%First, compute the variance-covariance matrix, a 2-by-2 matrix with
%variances of X and Y in the diagonal, and the covariance in (1,2) and (2,1) elements.
Z = [X Y];
mcov = cov(Z); %type "help cov" for details!
mY = mean(Y); %mean of X
mX = mean(X); %mean of Y

%Apply the theoretical formulas to produce the samples estimates for b1 and
%b0
b1_hat = mcov(2,1)/mcov(1,1);
b0_hat = mY - b1_hat*mX;

%Picture time
vX = 0:0.01:1; %A dense grid for X
vY = b0_hat + b1_hat*vX; %Regression line using the estimates
vYr = b0 + b1*vX; %True (unobserved) linear relationship (conditional expectation of yi given xi)

figure;scatter(X,Y); %scatter plot
hold on; %keep the current pic
plot(vX,vY,'r'); %plot the regression line (in red)
plot(vX,vYr,'g'); %plot the true conditional expectation (in green)
hold off;