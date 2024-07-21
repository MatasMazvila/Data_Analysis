% Exercise 15.6
clear;

% Part i and ii
std = 1; %standard deviation for standard normal distribution
T = 100; %number of periods

% Coefficients of random walk model
b0 = 0;
b1 = 1;

% Generate T standard normal variables
e = normrnd(0,std,T,1);
a = normrnd(0,std,T,1);

% Vector for variables Y and X
Y = zeros(T,1);
X = zeros(T,1);

% Set a value for the first dependent variables
Y(1) = e(1);
X(1) = a(1);

% Generate the time series Y(t) and X(t)
for t=2:T
     Y(t) = Y(t-1) + e(t);
     X(t) = X(t-1) + a(t);
end

% Part iii
Y_l0 = Y(1:T,1); %our Y_t variable
X_l0 = X(1:T,1); %our X_t variable

% Run the regression model using the function fitlm
my_est = fitlm(Y_l0,X_l0);

OLS_estimator = my_est.Coefficients.Estimate(2);
R_squared = my_est.Rsquared.Ordinary;
t_statistic = my_est.Coefficients.tStat(2);

%% Part a

alpha = 0.05; % Significance level
critical_value = 1.96; % Critical value for a 5% significance level

% Hypothesis test
if abs(t_statistic) > critical_value
    disp('Reject the null hypothesis. b1 is significantly different from 0.');
else
    disp('Fail to reject the null hypothesis. b1 is not significantly different from 0.');
end

% Print the R-squared
fprintf('R-squared: %.4f\n', R_squared);

%% Parts b and c

% Define the values of T to be tested
T_values = [50, 100, 200];

% Loop over each value of T
for T = T_values
    num_simulations = 1000; % Number of simulations
    R_squared_values = zeros(num_simulations, 1); % Array to store R-squared values
    t_statistic_values = zeros(num_simulations, 1); % Array to store t-statistic values

    for i = 1:num_simulations
        e = normrnd(0,std,T,1);
        a = normrnd(0,std,T,1);
        Y = zeros(T,1);
        X = zeros(T,1);
        Y(1) = e(1);
        X(1) = a(1);
        
        for t = 2:T
            Y(t) = Y(t-1) + e(t);
            X(t) = X(t-1) + a(t);
        end
        
        Y_l0 = Y(1:T,1);
        X_l0 = X(1:T,1);
        my_est = fitlm(Y_l0, X_l0); % Perform the regression

        % Store the R-squared and t-statistic values
        R_squared_values(i) = my_est.Rsquared.Ordinary;
        t_statistic_values(i) = my_est.Coefficients.tStat(2);
    end

    % Plot histogram of R-squared values
    figure;
    histogram(R_squared_values);
    xlabel('R-squared');
    ylabel('Frequency');
    title(['Histogram of R-squared (T = ', num2str(T), ')']);

    % Plot histogram of t-statistic values
    figure;
    histogram(t_statistic_values);
    xlabel('t-statistic');
    ylabel('Frequency');
    title(['Histogram of t-statistic (T = ', num2str(T), ')']);

    % Calculate percentiles
    r2_percentiles = prctile(R_squared_values, [5, 50, 95]);
    t_stat_percentiles = prctile(t_statistic_values, [5, 50, 95]);

    disp(['For T = ', num2str(T)]);
    disp('R-squared percentiles:');
    disp(r2_percentiles);
    disp('t-statistic percentiles:');
    disp(t_stat_percentiles);

    fraction_exceeding = mean(abs(t_statistic_values) > 1.96);
    disp(['Fraction of t-statistic values exceeding 1.96: ', num2str(fraction_exceeding)]);
end

% As the sample size increases, we can see that the fraction of times the
%null hypothesis is rejected does not approach the expected 5% significance
%level. Instead, it seems to approach a different limit. In this case, the
%fraction seems to increase as T gets larger. As the number of obervations
%increases, the fraction seems to approach 1. This suggests that there may
%be factors other than the independence of X and Y that contribute to the
%deviation from the expected rejection rate.







