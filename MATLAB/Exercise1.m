%Excercise 1 part a -----------------

clear

%Parameters
f0 = 1;
f1 = 2;
b0 = 6;
b1 = 2.2;

%Number of obervations
N = 100;

%Creating variables
Z = normrnd(0,0.25,N,1);
w = normrnd(0,0.1,N,1);
u = normrnd(0,0.5,N,1);
v = w - u;
X = f0 + f1*Z + v;
Y = b0 + b1*X + u;

%Table of covariances
MYvars = [X Y];
mcov = cov(MYvars);
mX = mean(X);
mY = mean(Y);

%Formulas for b0 and b1
b1_hat = mcov(2,1) / mcov(1,1);
b0_hat = mY - b1_hat * mX;

%Computing asymptotic bias
asymptotic_bias_b11 = b1 - b1_hat;
asymptotic_bias_b01 = b0 - b0_hat;

%% 
%With N=1000

clear

%Parameters
f0 = 1;
f1 = 2;
b0 = 6;
b1 = 2.2; 

%Number of observations
N = 1000;

%Creating variables
Z = normrnd(0, 0.25,N,1);
w = normrnd(0,0.1,N,1);
u = normrnd(0,0.5,N,1);
v = w - u;
X = f0 + f1*Z + v;
Y = b0 + b1*X + u;

%Table of covariances
MYvars = [X Y];
mcov = cov(MYvars);
mY = mean(Y);
mX = mean(X);

%Formulas for b0 and b1
b1_hat=mcov(2,1)/mcov(1,1);
b0_hat=mY - b1_hat*mX;

%Computing asymptotic bias
asymptotic_bias_b12 = b1 - b1_hat;
asymptotic_bias_b02 = b0 - b0_hat;
%% 
%Excercise 1 part c -----------------

clear

%Parameters
cf0 = 1;
cf1 = 2;
cb0 = 6;
cb1 = 2.2; 

%Number of observations
cN = 100;

%Creating variables
cZ = normrnd(0, 0.25,cN,1); 
cw = normrnd(0, 0.1,cN,1);
cu = normrnd(0,0.5,cN,1);
cv = cw - cu;
cX = cf0 + cf1*cZ + cv;
cY = cb0 + cb1*cX + cu;

%Table of covariances
cMYvars = [cX cY cZ];
cmcov = cov(cMYvars);
cmY = mean(cY);
cmX = mean(cX);

%Formulas for b0 and b1
cb1_hat=cmcov(2,3)/cmcov(3,1);
cb0_hat=cmY - cb1_hat*cmX;

%Computing asymptotic bias
asymptotic_bias_b1 = cb1 - cb1_hat;
asymptotic_bias_b0 = cb0 - cb0_hat;

%% 

%with N=1000
clear

%Parameters
cf0 = 1;
cf1 = 2;
cb0 = 6;
cb1 = 2.2; 

%Number of observations
cN = 1000;

%Creating variables
cZ = normrnd(0, 0.25,cN,1); 
cw = normrnd(0,0.1,cN,1);
cu = normrnd(0,0.5,cN,1);
cv = cw - cu;
cX = cf0 + cf1*cZ + cv;
cY = cb0 + cb1*cX + cu;

%Table of covariances
cMYvars = [cX cY cZ];
cmcov = cov(cMYvars);
cmY = mean(cY);
cmX = mean(cX);

%Formulas for b0 and b1
cb1_hat=cmcov(2,3)/cmcov(3,1);
cb0_hat=cmY - cb1_hat*cmX;

%Computing asymptotic bias
casymptotic_bias_b1 = cb1 - cb1_hat;
casymptotic_bias_b0 = cb0 - cb0_hat;






