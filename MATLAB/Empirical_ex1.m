% Empirical exercise 1
clear;

ds = spreadsheetDatastore('cpi_data.xlsx');
data = read(ds);

%% Part a

% Part i

% Getting the ln of CPI(t) and CPI(t-1) 
CPIt1 = log(table2array(data(13:231, "CPI")));
CPIt = log(table2array(data(14:232, "CPI")));

% Estimating the difference of logs
diffln = CPIt-CPIt1;

% Computing the inflation rate
infl = 400 * diffln;

% Inflation is measured in quarterly percentage change per annum. It represents the
% rate at which the general price level of goods and services is increasing
% or decreasing.

% Part ii
% Extract the relevant dates
dates = data(14:232, "Date");
dates.inflation = infl;

% Plot the inflation values
plot(dates, "Date", "inflation")
xlabel('Date')
ylabel('Inflation rate')
title('Inflation rate over time')

% Inflation rate does have a stockastic trend. It is obvious that the
% variance would differ if we chopped data into shorter intervals.

%% Part b

% Part i

% Computing the difference of inflation
Y = diff(infl);

% Calculate the lags
Y_l0 = Y(5:218); % Y(t)
Y_l1 = Y(4:217); % Y(t-1)
Y_l2 = Y(3:216); % Y(t-2)
Y_l3 = Y(2:215); % Y(t-3)
Y_l4 = Y(1:214); % Y(t-4)

% Find the autocorrelations
autocorrelation(1) = corr(Y_l0, Y_l1); % Autocorrelation at lag 1
autocorrelation(2) = corr(Y_l0, Y_l2); % Autocorrelation at lag 2
autocorrelation(3) = corr(Y_l0, Y_l3); % Autocorrelation at lag 3
autocorrelation(4) = corr(Y_l0, Y_l4); % Autocorrelation at lag 4

% Display the autocorrelation values
disp(autocorrelation);

% Part ii

% Extract the relevant dates
dates1 = data(15:232, "Date");
dates1.inflation = Y;

% Plot the ∆inflation values
plot(dates1, "Date", "inflation")
xlabel('Date')
ylabel('∆Inflation rate')
title('∆Inflation rate over time')

% The jagged behavior in the plot indicates that the differences in inflation
% fluctuate between positive and negative values over time. The choppy or
% jagged behavior of the plot is consistent with the negative first
% autocorrelation that was computed, it implies that periods of high inflation
% are likely to be followed by periods of low inflation and vice versa, leading
% to the choppy movement in the plot.

%% Part c

% Part i

% Run an OLS of Y_l0 and Y_l1
my_est = fitlm(Y_l1, Y_l0);

t_statistic = my_est.Coefficients.tStat;
disp(t_statistic);

% t-statistic of -3.9316 indicates that the coefficient of ∆Inflt−1 is
% statistically significant at 5% level. The negative sign suggests an inverse
% relationship between the change in inflation over the current quarter
% (∆Inflt−1) and the change in inflation over the next quarter (∆Inflt).

%% Part ii

% Run an AR(2)
my_est1 = fitlm([Y_l1, Y_l2], Y_l0);

% Calculate AIC and BIC for AR(2) model
AIC_AR2 = my_est1.ModelCriterion.AIC;
BIC_AR2 = my_est1.ModelCriterion.BIC;

% Calculate AIC and BIC for AR(1) model
AIC_AR1 = my_est.ModelCriterion.AIC;
BIC_AR1 = my_est.ModelCriterion.BIC;

% Compare AIC and BIC values
disp([AIC_AR2, AIC_AR1]);
disp([BIC_AR2, BIC_AR1]);

% Comparing the AIC and BIC values, we can see that both the AIC and BIC
% values are lower for the AR(2) model compared to the AR(1) model. In general,
% lower AIC and BIC values indicate a better model fit. Therefore, based on
% these criteria, the AR(2) model is better than the AR(1) model for
% predicting changes in inflation.

%% Part iii

% Variables for AR(p), p = 0, ..., 8
Y0 = Y(9:218); %our current variable
Y1 = Y(8:217); % the first lag Y(t-1) 
Y2 = Y(7:216); % the second lag Y(t-2) 
Y3 = Y(6:215); % the third lag Y(t-3) 
Y4 = Y(5:214); % the fourth lag Y(t-4) 
Y5 = Y(4:213); % the fifth lag Y(t-5) 
Y6 = Y(3:212); % the sixth lag Y(t-6) 
Y7 = Y(2:211); % the seventh lag Y(t-7) 
Y8 = Y(1:210); % the eighth lag Y(t-8) 

% Estimating the sum of squared residuals for each model

SSR0 = fitlm(Y0, Y0).SSR;
SSR1 = fitlm(Y1,Y0).SSR;
SSR2 = fitlm([Y1, Y2], Y0).SSR; 
SSR3 = fitlm([Y1, Y2, Y3], Y0).SSR;
SSR4 = fitlm([Y1, Y2, Y3, Y4], Y0).SSR;
SSR5 = fitlm([Y1, Y2, Y3, Y4, Y5], Y0).SSR;
SSR6 = fitlm([Y1, Y2, Y3, Y4, Y5, Y6], Y0).SSR;
SSR7 = fitlm([Y1, Y2, Y3, Y4, Y5, Y6, Y7], Y0).SSR;
SSR8 = fitlm([Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8], Y0).SSR;

T0 = 210;

SSR_values = {SSR0, SSR1, SSR2, SSR3, SSR4, SSR5, SSR6, SSR7, SSR8};
BIC_values = zeros(1, 9);
AIC_values = zeros(1, 9);
p_values = 0:8;

for p = 0:8
    SSR = SSR_values{p+1};  % Retrieve the SSR value for the current p
    BIC_values(p+1) = log(SSR/T0) + (p + 1) * (log(T0) / T0);
    AIC_values(p+1) = log(SSR/T0) + (p + 1) * (2 / T0);
end

BICs = table(p_values', BIC_values', 'VariableNames', {'p','BIC'});
AICs = table(p_values', AIC_values', 'VariableNames', {'p', 'AIC'});

[~, min_index] = min(BICs.BIC);
best_BICs_p = BICs.p(min_index);

[~, min_index] = min(AICs.AIC);
best_AICs_p = AICs.p(min_index);

disp(['BIC: Lag Length = ', num2str(best_BICs_p), ', BIC Value = ', num2str(min_BIC)]);
disp(['AIC: Lag Length = ', num2str(best_AICs_p), ', AIC Value = ', num2str(min_AIC)]);

%The lag length chosen by BIC and AIC is 1.

%% Part iv

% Our AR(2) model estimates:
y0 = Y(3:218);
y1 = Y(2:217); 
y2 = Y(1:216);

reg = fitlm([y1, y2], y0);
beta0 = reg.Coefficients.Estimate(1);
beta1 = reg.Coefficients.Estimate(2);
beta2 = reg.Coefficients.Estimate(3);

% Predicting the value of change in inflation in 2018:Q1
diffinfl_2018Q1 = beta0 + beta1*Y(218,1) + beta2*Y(217,1);

disp(diffinfl_2018Q1);

%% Part v

% Predicting the inflation rate in 2018:Q1
inflation2018_Q1 = infl(219,1) + diffinfl_2018Q1;

disp(inflation2018_Q1);

%% Part d

% Part i

% Perform the linear regression
regressi = fitlm([infl(1:216, 1), Y(1:216), Y(2:217)], Y(3:218));

% Extract the residuals from the regression
residuals = regressi.Residuals.Raw;

% Perform the ADF test on the residuals
adf_test = adftest(residuals, 'lags', 3, 'model', 'ARD');

% Display the test results
disp(adf_test);

% The ADF test value is 1 at a 5% significance level, we fail to reject the
% null hypothesis, suggesting that there is evidence of a stochastic trend
% in the data.

% Part ii

% The ADF test based on Equation (15.32) is preferred over Equation (15.33)
% for testing a stochastic trend in Infl because it focuses solely on lagged
% differences, allowing for a more specific test on the presence of a unit
% root or a stochastic trend. Equation (15.33) introduces the possibility
% of spurious regression, making the test less reliable.

% Part iii

% We should use one lag since both BIC and AIC tests showed that this is 
% the optimal number of lags.

% Part iv

% Based on the test conducted in (i), where the null hypothesis of a unit
% root is not rejected, it indicates that the AR model for Infl may contain
% a unit root.The failure to reject the null hypothesis does not necessarily
% mean that the null hypothesis is true, but rather that there is insufficient
% evidence to reject it.

%% Part e

% Set the trimming values
T = numel(Y);
a = round(0.15 * T);
b = T - a;

% Estimating the OLS for each period separately
reg2a = fitlm([Y(1:a), Y(2:a+1)], Y(3:a+2));
reg2b = fitlm([Y(a+1:b), Y(a+2:b+1)], Y(a+3:b+2));
reg2c = fitlm([Y(b+1:end-2), Y(b+2:end-1)], Y(b+3:end));

% Extract t-statistics from regression models
t_stats_a = coefTest(reg2a);
t_stats_b = coefTest(reg2b);
t_stats_c = coefTest(reg2c);

disp([t_stats_a, t_stats_b, t_stats_c]);

% Since all t-stats fall within the "fail to reject" interval, it suggests
% that the coefficients in the AR(2) model remain stable across the different
% periods tested. Therefore, you can conclude that the AR(2) model is stable
% based on the provided t-statistics.

%% Part f

% Part i

% AR(2) model for period 1963:Q1-2002:Q4
estimateh = fitlm([Y(1:156), Y(2:157)], Y(3:158));

b0 = estimateh.Coefficients.Estimate(1);
b1 = estimateh.Coefficients.Estimate(2);
b2 = estimateh.Coefficients.Estimate(3);

FY = Y(1:158);
YFor = zeros(218, 1);
YFor(1:158, 1) = FY;

forecast = zeros(60, 1);  % Preallocate the forecast vector

for t = 1:60
    forecast(t) = b0 + b1 * YFor(t+157) + b2 * YFor(t+156);
    YFor(t+158) = forecast(t);
end

%% Part ii

% Compute the mean forecast error
forecast_errors = forecast - Y(159:218);
mean_forecast_error = mean(forecast_errors);
disp(mean_forecast_error);

% Based on the result of mean forecast error being approximately -0.0040,
% it suggests that the pseudo out-of-sample forecasts are slightly biased.
% The nonzero mean indicates a systematic tendency for the forecasts to
% consistently underestimate the actual values. However, the magnitude of
% the bias appears to be relatively small.

%% Part iii

% Compute RMSFE of the pseudo out-of-sample forecasts
RMSFE = sqrt(mean(forecast_errors.^2));
disp(RMSFE);

% Compute the RMSFE for the AR(2) model
YEst = Y(1:158);  % Data for the estimation period
YEstFor = zeros(158, 1);  % Array to store the estimated values

for t = 1:156
    YEstFor(t+2) = b0 + b1 * YEstFor(t+1) + b2 * YEstFor(t);
end

forecast_errors_est = YEst(3:158) - YEstFor(3:158);
RMSFE_est = sqrt(mean(forecast_errors_est.^2));
disp(RMSFE_est);

% The RMSFE (Root Mean Square Forecast Error) of the pseudo out-of-sample
% forecasts is 3.0248. Comparing it with the RMSFE of 1.6927 obtained from
% the estimation of the AR(2) model, the pseudo out-of-sample forecasts
% have a larger RMSFE. This suggests a potential decrease in the accuracy
% of the AR(2) model when applied to the out-of-sample period. 

% Part iv

% In 2008:Q4, there was a significant decrease in inflation, which can be
% attributed to the sharp decline in oil prices during that period. Oil
% prices experienced a substantial drop in 2008 due to various factors such
% as the global financial crisis, reduced demand for oil, and increased oil
% supply. The decline in oil prices had a direct impact on overall inflation,
% as energy costs play a significant role in determining inflation levels.
% Therefore, the decrease in inflation in 2008:Q4 can be primarily attributed
% to the substantial decline in oil prices during that time.
