%This script performs a simple Montercarlo simulation to check the
%distribution of the basic one-regressor OLS estimator

clear;

%Linear model: parameters
b0 = 3; %intercept
b1 = 7; %slope

N = 100; %number of observations
T = 10000; %number of random samples

%Generate simulated data: matrices of size N-by-T,
%where each column is a new iid sample)
vX = rand(N,T); %For the regressor (uniform distribution)
vu = normrnd(0,0.5,N,T);  %For the unobserved error term (normal distribution)

%Generate values for dependent variable using the linear model
%Another N-by-T matrix
vY = b0 + b1*vX + vu;

%Matrix of zeros, to be filled with our sample estimates for b0 and b1
v_b_hat = zeros(2,T);

%run a "for" loop, from counter variable t=1 to t=T
for t=1:T
    
    X = vX(:,t); %sample data for X (column t of matrix vX)
    Y = vY(:,t); %sample data for Y (column t of matrix vY)
    
    Z = [X Y];
    mcov = cov(Z); %type "help cov" for details!
    mY = mean(Y); %mean of X
    mX = mean(X); %mean of Y
    
    %Apply the theoretical formulas to produce the samples estimates for b1 and
    %b0
    b1_hat = mcov(2,1)/mcov(1,1);
    b0_hat = mY - b1_hat*mX;
    
    %Store the estimates in matrix v_b_hat (first column for b0_hat, second for b1_hat)
    v_b_hat(:,t) = [b0_hat b1_hat];
    
end

%plot histograms to see the distribution
figure;histogram(v_b_hat(2,:));title('distribution of b1-hat');
figure;histogram(v_b_hat(1,:));title('distribution of b0-hat');

%compute the means of b1_hat and b0_hat
mb1_hat = mean(v_b_hat(2,:));
mb0_hat = mean(v_b_hat(1,:));