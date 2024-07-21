lear

load('problem3_data.mat');

%----------------------
%Part 1 
%----------------------

%Computing basic sample statistics
 nE = length(energy_price);
 nY = length(subs_price);
 nI = length(income);
 nEq = length(equi_price);
 nQ = length(Q);
 observations = [nE;nY;nI;nEq;nQ];

 mE = mean(energy_price);
 mY = mean(subs_price);
 mI = mean(income);
 mEq = mean(equi_price);
 mQ = mean(Q);
means = [mE;mY;mI;mEq;mQ];

 sE = std(energy_price);
 sY = std(subs_price);
 sI = std(income);
 sEq = std(equi_price);
 sQ = std(Q);
 devations = [sE;sY;sI;sEq;sQ];

 minE = min(energy_price);
 minY = min(subs_price);
 minI = min(income);
 minEq = min(equi_price);
 minQ = min(Q);
 mins = [minE;minY;minI;minEq;minQ];

 maxE = max(energy_price);
 maxY = max(subs_price);
 maxI = max(income);
 maxEq = max(equi_price);
 maxQ = max(Q);
 maxs = [maxE;maxY;maxI;maxEq;maxQ];

A = table(observations, means, devations, mins, maxs, 'VariableNames', ...
    {'Number of observations', 'Mean', 'SD', 'MIN', 'MAX'}, 'RowNames', ...
    {'Energy price', 'Yogurt price', 'Income', 'Equilibrium price', 'Quantities'});
disp(A);

%It can be clearly seen that there are some outliers as, for instance,
%minimum values of energy and yougurt prices are negative

%Producing a table of correlations
correlmatrix = corrcoef([Q, energy_price, equi_price, income, subs_price]) ;
 
varNames = {'Quantities','Energy Price', 'Equilibrium Price', 'Income', 'Yoghurt Price'} ;

corrTable = array2table(correlmatrix, 'VariableNames', varNames, 'RowNames', varNames) ;

corrTable

%Plotting histograms
histogram(energy_price,'FaceColor', 'g') ;
title('Histogram of energy prices') ;
xlabel('Data Values') ;
ylabel('Frequency') ;

histogram(subs_price,'FaceColor', 'r')
title('Histogram of substitute yogurt prices')
xlabel('Data Values')
ylabel('Frequency')

histogram(income,'FaceColor', 'b')   
title('Histogram of income')
xlabel('Data Values')
ylabel('Frequency')

histogram(equi_price,'FaceColor', 'y');   
title('Histogram of equilibrium prices');
xlabel('Data Values');
ylabel('Frequency');

histogram(Q,'FaceColor', [0.5, 0, 0.5]);
title('Histogram of quantities');
xlabel('Data Values');
ylabel('Frequency');

%We can state that all of the variables are normally distributed as 
%histograms have an evident bell-shape

%Computing scatterplots
scatter(energy_price, subs_price);
title('Relationship between energy prices and yogurt prices');
xlabel('Energy prices');
ylabel('Substitute yogurt prices');

coeffs = polyfit(energy_price, subs_price, 1); 
x = linspace(min(energy_price), max(energy_price), 100); 
y = polyval(coeffs, x); 
hold on;
plot(x, y, 'r'); 
hold off;


scatter(energy_price, income);
title('Relationship between energy prices and income');
xlabel('Energy prices');
ylabel('Average income');

coeffs = polyfit(energy_price, income, 1); 
x = linspace(min(energy_price), max(energy_price), 100); 
y = polyval(coeffs, x); 
hold on;
plot(x, y, 'r'); 
hold off;


scatter(energy_price, equi_price);
title('Relationship between energy prices and equilibrium prices');
xlabel('Energy prices');
ylabel('Equilibrium prices');

coeffs = polyfit(energy_price, equi_price, 1); 
x = linspace(min(energy_price), max(energy_price), 100); 
y = polyval(coeffs, x); 
hold on;
plot(x, y, 'r')
hold off;



scatter(energy_price, Q);
title('Relationship between energy prices and quantities');
xlabel('Energy prices');
ylabel('Quantity');

coeffs = polyfit(energy_price, Q, 1); 
x = linspace(min(energy_price), max(energy_price), 100); 
y = polyval(coeffs, x); 
hold on;
plot(x, y, 'r')
hold off;

scatter(subs_price, income);
title('Relationship between yogurt prices and average income');
xlabel('Yogurt prices');
ylabel('Average income');

coeffs = polyfit(subs_price, income, 1); 
x = linspace(min(subs_price), max(subs_price), 100); 
y = polyval(coeffs, x); 
hold on;
plot(x, y, 'r')
hold off;

scatter(subs_price, equi_price);
title('Relationship between yogurt prices and equilibrium prices');
xlabel('Yogurt prices');
ylabel('Equilibrium prices');

coeffs = polyfit(subs_price, equi_price, 1); 
x = linspace(min(subs_price), max(subs_price), 100); 
y = polyval(coeffs, x); 
hold on;
plot(x, y, 'r')
hold off;

scatter(subs_price, Q);
title('Relationship between yogurt prices and quantities');
xlabel('Yogurt prices');
ylabel('Quantity');

coeffs = polyfit(subs_price, Q, 1); 
x = linspace(min(subs_price), max(subs_price), 100); 
y = polyval(coeffs, x); 
hold on;
plot(x, y, 'r')
hold off;

scatter(income, equi_price);
title('Relationship between average income and equilibrium prices');
xlabel('Average income');
ylabel('Equilibrium prices');

coeffs = polyfit(income, equi_price, 1); 
x = linspace(min(income), max(income), 100); 
y = polyval(coeffs, x); 
hold on;
plot(x, y, 'r')
hold off;

scatter(income, Q);
title('Relationship between average income and quantities');
xlabel('Average income');
ylabel('Quantity');

coeffs = polyfit(income, Q, 1); 
x = linspace(min(income), max(income), 100); 
y = polyval(coeffs, x); 
hold on;
plot(x, y, 'r')
hold off;

scatter(equi_price, Q);
title('Relationship between equilibrium prices and quantities');
xlabel('Equilibrium prices');
ylabel('Quantities');

coeffs = polyfit(equi_price, Q, 1); 
x = linspace(min(equi_price), max(equi_price), 100); 
y = polyval(coeffs, x); 
hold on;
plot(x, y, 'r')
hold off;

%Describing the patterns we've observed:
%Energy prices have a negative relationship with yogurt prices, average
%income and quantities, while we observe a positive relationship with
%equilibrium prices. Yogurt prices are positively correlated with income,
%equilibrium prices and quantities but the relationship with energy prices
%is weakly negative. Average income moves to the same direction with yogurt prices
%and quantities but moves to the opposite direction of energy prices and
%equilibrium prices. Equilibrium prices have a positive relationsip with
%energy and yogurt prices, while the relationship eith income and
%quantities is negative. Lastly, quantities are positively correlated with
%yogurt prices and income but negatively correlated with energy prices and
%equilibrium prices. 
%Substitute price and Energy price have very weak correlation . Also Income
%and Substite price too have very weak correlation, thus we could state that
%they are uncorrelated at all. Lastly, the same goes with Equilibrium price and
%Income.

%% 

%-------------------
%Part 2
%-------------------

%Running the OLS estimator to get b0, b1, b2 and b3 for the demand function
D = [ones(size(equi_price)) equi_price subs_price income];
regression_d = regress(Q, D);
regression_d

%Display the betas
disp(['beta0: ' num2str(regression_d(1))]);
disp(['beta1: ' num2str(regression_d(2))]);
disp(['beta2: ' num2str(regression_d(3))]);
disp(['beta3: ' num2str(regression_d(4))]); 

%Running the OLS estimator to get g0, g1 and g2 for the supply function
S = [ones(size(equi_price)) equi_price energy_price];
regression_S = regress(Q, S);
regression_S

%Display the gammas
disp(['gamma0: ' num2str(regression_S(1))]);
disp(['gamma1: ' num2str(regression_S(2))]);
disp(['gamma2: ' num2str(regression_S(3))]);


%The estimates make sense (at least the signs of the coefficients).
%However, the estimates might be biased as we don't know whether the 
%equilibrium price is not correlated with the error terms u and v.

%% 

%--------------------
%Part 3
%--------------------

%Our instrument is energy prices.

%We start with the first stage regression:
% Pt = alpha0 + alpha1*energyprices + alpha2*substitute prices +
% alpha3*income
%This line of code below is creating a matrix Pt that will be used as the independent 
% variable matrix in the first stage regression model to predict the equilibrium price 
Pt = [ones(size(energy_price)) energy_price subs_price income];  
first = regress(equi_price, Pt);  
P_fixed = Pt*first;

%P_fixed refers to the fitted values of the first stage regression model. 
% %Specifically, it is the predicted values of the dependent variable (equilibrium price Pt) 
% based on the estimated coefficients

%Computing the coefficient values:
ones1 = ones(10000, 1);
Demand = [ones1 P_fixed subs_price income];
Demand_estimates = regress(Q, Demand);

%Display the betas
disp(['beta0: ' num2str(Demand_estimates(1))]);
disp(['beta1: ' num2str(Demand_estimates(2))]);
disp(['beta2: ' num2str(Demand_estimates(3))]); 
disp(['beta3: ' num2str(Demand_estimates(4))]); 
%%

%-------------------
%Part 4
%-------------------

%Our instruments are substitute prices and income. They are different from
%the previous part because one of the critical assumptions for the
%instruments is that they cannot be correlated with the error term u which
%might have impact on our Pt. In the previous part we used energy prices as
%an instrument because it was not related to the error term v.

%We have already computed the value for fixed Pt, so we can compute g0, g1
%and g2:
Supply = [ones1 P_fixed energy_price];
Supply_estimates = regress(Q, Supply);

%Display the gammas
disp(['gamma0: ' num2str(Supply_estimates(1))]);
disp(['gamma1: ' num2str(Supply_estimates(2))]);
disp(['gamma2: ' num2str(Supply_estimates(3))]); 



