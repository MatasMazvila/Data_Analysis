%% Loading the data

clear; close all; clc
format short g

% define the working directory
cd("C:\Users\DELL i5\OneDrive\Desktop\Time Series\");

% add path to the package
addpath(genpath('VAR-Toolbox-main/v3dot0'));

dataraw = readtable('data_uncertainty.xlsx', 'Sheet', 'data');
addpath('C:\Users\DELL i5\OneDrive\Desktop\Matas\MatLab\toolbox\stats');
addpath('C:\Users\DELL i5\OneDrive\Desktop\Matas\MatLab\toolbox\econ');
savepath;  % This saves the path for future MATLAB sessions
%% Creating a dataset for a baseline VAR model

rows = 1021:1378;
columns = {'year', 'month', 'EPU', 'lsp', 'ffr', 'lemp', 'indus_prod', 'vix', 'eu'};
data = dataraw(rows, columns);
date_data = datetime(data.year, data.month, 1); 
data.date = date_data;
data.lip = log(data.indus_prod)*100;
data.lemp = data.lemp*100;
nobs = size(data, 1);

%% Plotting EPU

plot(data.date, data.EPU,'LineWidth', 2)
xlabel('\bf{Year}', 'FontWeight', 'bold', 'FontSize', 14)
ylabel('\bf{Economic Policy Uncertainty Index}', 'FontWeight', 'bold', 'FontSize', 14)

%% Baseline VAR (1 sd shock)

% Selecting the list of endogenous variables
Xvnames = {'EPU','lsp','ffr', 'lemp', 'lip'};

%Corresponding labels to be used in plots
Xvnames_long = {'Economic policy uncertainty','Log of S&P', 'Federal funds rate','Log of Employment', 'Log of industrial production'};
% Number of endo variables
Xnvar = length(Xvnames);
% Create matrix X of variables to be used in the VAR
X = nan(nobs,Xnvar);
for ii=1:Xnvar
    X(:,ii) = data.(Xvnames{ii});
end

% Make a common sample by removing NaNs
[X, fo, lo] = CommonSample(X);

% Set the deterministic variable in the VAR (1=constant, 2=trend)
det = 1;
% Set number of lags
nlags = 3; 
% Estimate VAR by OLS
[VAR, VARopt] = VARmodel(X,nlags,det);

% Update the VARopt structure with additional details
VARopt.vnames = Xvnames_long;
% Print at screen VAR coefficients and create table 
[TABLE, beta] = VARprint(VAR,VARopt,3);
 
VARopt.ident = 'short';
% Update the VARopt structure with additional details 
VARopt.vnames = Xvnames_long;            % variable names in plots
VARopt.nsteps = 36;                      % max horizon of IRF
VARopt.FigSize = [26,12];                % size of window (figures)
VARopt.firstdate = date_data(1);          % first date in plots
VARopt.frequency = 'm';                  % frequency of the data
VARopt.pctg = 90;                        % confidence level is set to 90%

VARopt.snames = {'\epsilon^{EPU}'};

% Compute impulse response
[IR, VAR] = VARir(VAR,VARopt);

format short
disp(VAR.B) % the structural matrix
disp(chol(VAR.sigma,'lower')) %Cholesky
disp(IR(1:12,:,1))

%VAR.resid = VAR.resid * scale;
% Compute structural shocks (Tx2)
eps_short = (VAR.B\VAR.resid')';
disp(corr(eps_short)) % this shows that structural shocks are uncorrellated by construction

%% Plotting the impulse response of employment and industrial production.
%  The response of variables is scaled, so that EPU increases by 90 points.

scale = 90/IR(1,1,1);
IR = scale * IR;

[IRinf,IRsup,IRmed,IRbar] = VARirband(VAR,VARopt);

subplot(1, 2, 1);
plot(IR(1:36, 4, 1), 'k', 'LineWidth', 1.5);
hold on;
fill([1:36, fliplr(1:36)], [scale * IRsup(1:36, 4, 1)', fliplr(scale * IRinf(1:36, 4, 1)')], 'b', 'FaceAlpha', 0.2, 'EdgeColor', 'none')
line(xlim, [0 0], 'Color', 'k', 'LineStyle', '--')
[min_value, min_index] = min(IR(1:36, 4, 1));
text(35, -0.45, ['Min: ', num2str(min_value)], 'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right');
hold off;
xlim([1, 36])
xlabel('\bf{Months}', 'FontWeight', 'bold');
title('Employment Response, %', 'FontSize', 14);

subplot(1, 2, 2);
plot(IR(1:36, 5, 1), 'k', 'LineWidth', 1.5);
hold on;
fill([1:36, fliplr(1:36)], [scale * IRsup(1:36, 5, 1)', fliplr(scale * IRinf(1:36, 5, 1)')], 'b', 'FaceAlpha', 0.2, 'EdgeColor', 'none')
line(xlim, [0 0], 'Color', 'k', 'LineStyle', '--')
[min_value, min_index] = min(IR(1:36, 5, 1));
text(35, -1.4, ['Min: ', num2str(min_value)], 'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right');
hold off;
xlim([1, 36])
xlabel('\bf{Months}', 'FontWeight', 'bold');
title('Industrial Production Response, %', 'FontSize', 14);

close(gcf);
%% Quarterly VAR for GDP and investment responses to EPU shock (1 sd shock)

rows = 1021:1378;
columns = {'year', 'quarter', 'EPU', 'lsp', 'ffr', 'lgross', 'lgdp'};
data_q = dataraw(rows, columns);
data_q.lgross = data_q.lgross*100;
data_q.lgdp = data_q.lgdp*100;

% Calculate average values per quarter
data_q = varfun(@mean, data_q, 'GroupingVariables', {'year', 'quarter'});
quarter_start_month = mod(data_q.quarter - 1, 4) * 3 + 1; 
quarter_start_year = data_q.year + floor((data_q.quarter - 1) / 4);  
date_data_q = datetime(quarter_start_year, quarter_start_month, 1);

data_q.Properties.VariableNames{4} = 'EPU';
data_q.Properties.VariableNames{5} = 'lsp';
data_q.Properties.VariableNames{6} = 'ffr';
data_q.Properties.VariableNames{7} = 'lgross';
data_q.Properties.VariableNames{8} = 'lgdp';

nobs_q = size(data_q, 1);

% Selecting the list of endogenous variables
Xvnames_q = {'EPU', 'lsp', 'ffr', 'lgross', 'lgdp'};

%Corresponding labels to be used in plots
Xvnames_long_q = {'Economic policy uncertainty','Log of S&P', 'Federal funds rate','Log of gross investment', 'Log of GDP'};
% Number of endo variables
Xnvar_q = length(Xvnames_q);
% Create matrix X of variables to be used in the VAR
X_q = nan(nobs_q,Xnvar_q);
for ii=1:Xnvar_q
    X_q(:,ii) = data_q.(Xvnames_q{ii});
end

% Make a common sample by removing NaNs
[X_q, fo_q, lo_q] = CommonSample(X_q);

% Set the deterministic variable in the VAR (1=constant, 2=trend)
det = 1;
% Set number of lags
nlags = 3; 
% Estimate VAR by OLS
[VAR_q, VARopt_q] = VARmodel(X_q,nlags,det);

% Update the VARopt structure with additional details
VARopt_q.vnames = Xvnames_long_q;
% Print at screen VAR coefficients and create table 
[TABLE, beta] = VARprint(VAR_q,VARopt_q,2);

% Update the VARopt structure to select zero short-run restrictions 
VARopt_q.ident = 'short';
% Update the VARopt structure with additional details 
VARopt_q.vnames = Xvnames_long_q;            % variable names in plots
VARopt_q.nsteps = 12;                      % max horizon of IRF
VARopt_q.FigSize = [26,12];                % size of window (figures)
VARopt_q.firstdate = date_data_q(1);          % first date in plots
VARopt_q.frequency = 'q';                  % frequency of the data
VARopt_q.pctg = 90; 

VARopt_q.snames = {'\epsilon^{EPU}'};
% Compute impulse response
[IR_q, VAR_q] = VARir(VAR_q,VARopt_q);
% Print at screen 
format short
disp(VAR_q.B) % the structural matrix
disp(chol(VAR_q.sigma,'lower')) 

disp(IR_q(1:4,:,1))

% Compute structural shocks (Tx2)
eps_short_q = (VAR_q.B\VAR_q.resid')';
disp(corr(eps_short_q)) 

%% Plotting the impulse response of GDP and investment.
%  The response of variables is scaled, so that EPU increases by 90 points.

scale_q = 90/IR_q(1,1,1);
IR_q = scale_q * IR_q;

[IRinf,IRsup,IRmed,IRbar] = VARirband(VAR_q,VARopt_q);

subplot(1, 2, 1);
plot(IR_q(1:12, 4, 1), 'k', 'LineWidth', 1.5);
hold on;
fill([1:12, fliplr(1:12)], [scale_q * IRsup(1:12, 4, 1)', fliplr(scale_q * IRinf(1:12, 4, 1)')], 'b', 'FaceAlpha', 0.2, 'EdgeColor', 'none')
line(xlim, [0 0], 'Color', 'k', 'LineStyle', '--')
[min_value, min_index] = min(IR_q(1:12, 4, 1));
text(11, -7.8, ['Min: ', num2str(min_value)], 'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right');
hold off;
xlim([1, 12])
xlabel('\bf{Quarters}', 'FontWeight', 'bold');
title('Investment Response, %', 'FontSize', 14);

subplot(1, 2, 2);
plot(IR_q(1:12, 5, 1), 'k', 'LineWidth', 1.5);
hold on;
fill([1:12, fliplr(1:12)], [scale_q * IRsup(1:12, 5, 1)', fliplr(scale_q * IRinf(1:12, 5, 1)')], 'b', 'FaceAlpha', 0.2, 'EdgeColor', 'none')
line(xlim, [0 0], 'Color', 'k', 'LineStyle', '--')
[min_value, min_index] = min(IR_q(1:12, 5, 1));
text(11, -3.8, ['Min: ', num2str(min_value)], 'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right');
hold off;
xlim([1, 12])
xlabel('\bf{Quarters}', 'FontWeight', 'bold');
title('GDP Response, %', 'FontSize', 14);

close(gcf);

%% VARs with Michigan Consumer Sentiment Index

%Creating Michigan First
rows = 1021:1378;
columns = {'year', 'month','mich', 'EPU', 'lsp', 'ffr', 'lemp', 'indus_prod'};
data_mich = dataraw(rows, columns);
date_data = datetime(data_mich.year, data_mich.month, 1); 
data_mich.date = date_data;
data_mich.lip = log(data_mich.indus_prod)*100;
data_mich.lemp = data_mich.lemp*100;
nobs = size(data_mich, 1);

% Selecting the list of endogenous variables
Xvnames = {'mich', 'EPU','lsp','ffr', 'lemp', 'lip'};

%Corresponding labels to be used in plots
Xvnames_long = {'Michigan Consumer Sentiment Index', 'Economic policy uncertainty','Log of S&P', 'Federal funds rate','Log of Employment', 'Log of industrial production'};
% Number of endo variables
Xnvar = length(Xvnames);
% Create matrix X of variables to be used in the VAR
X = nan(nobs,Xnvar);
for ii=1:Xnvar
    X(:,ii) = data_mich.(Xvnames{ii});
end

% Make a common sample by removing NaNs
[X, fo, lo] = CommonSample(X);

% Set the deterministic variable in the VAR (1=constant, 2=trend)
det = 1;
% Set number of lags
nlags = 3; 
% Estimate VAR by OLS
[VAR_mich, VARopt_mich] = VARmodel(X,nlags,det);

% Update the VARopt structure with additional details
VARopt_mich.vnames = Xvnames_long;
 
VARopt_mich.ident = 'short';
% Update the VARopt structure with additional details 
VARopt_mich.vnames = Xvnames_long;            % variable names in plots
VARopt_mich.nsteps = 36;                      % max horizon of IRF
VARopt_mich.FigSize = [26,12];                % size of window (figures)
VARopt_mich.firstdate = date_data(1);          % first date in plots
VARopt_mich.frequency = 'm';                  % frequency of the data
VARopt_mich.pctg = 90;                        % confidence level is set to 90%

VARopt_mich.snames = {'\epsilon^{EPU}'};

% Compute impulse response
[IR_mich, VAR_mich] = VARir(VAR_mich,VARopt_mich);

format short
disp(VAR_mich.B) % the structural matrix
disp(chol(VAR_mich.sigma,'lower')) %Cholesky
disp(IR_mich(1:12,:,2))

%VAR.resid = VAR.resid * scale;
% Compute structural shocks (Tx2)
eps_short = (VAR_mich.B\VAR_mich.resid')';
disp(corr(eps_short)) % this shows that structural shocks are uncorrellated by construction

%Creating Michigan Second 

% Selecting the list of endogenous variables
Xvnames = {'EPU', 'mich','lsp','ffr', 'lemp', 'lip'};

%Corresponding labels to be used in plots
Xvnames_long = {'Economic policy uncertainty', 'Michigan Consumer Sentiment Index', 'Log of S&P', 'Federal funds rate','Log of Employment', 'Log of industrial production'};
% Number of endo variables
Xnvar = length(Xvnames);
% Create matrix X of variables to be used in the VAR
X = nan(nobs,Xnvar);
for ii=1:Xnvar
    X(:,ii) = data_mich.(Xvnames{ii});
end

% Make a common sample by removing NaNs
[X, fo, lo] = CommonSample(X);

% Set the deterministic variable in the VAR (1=constant, 2=trend)
det = 1;
% Set number of lags
nlags = 3; 
% Estimate VAR by OLS
[VAR_mich2, VARopt_mich2] = VARmodel(X,nlags,det);

% Update the VARopt structure with additional details
VARopt_mich2.vnames = Xvnames_long;
 
VARopt_mich2.ident = 'short';
% Update the VARopt structure with additional details 
VARopt_mich2.vnames = Xvnames_long;            % variable names in plots
VARopt_mich2.nsteps = 36;                      % max horizon of IRF
VARopt_mich2.FigSize = [26,12];                % size of window (figures)
VARopt_mich2.firstdate = date_data(1);          % first date in plots
VARopt_mich2.frequency = 'm';                  % frequency of the data
VARopt_mich2.pctg = 90;                        % confidence level is set to 90%

VARopt_mich2.snames = {'\epsilon^{EPU}'};

% Compute impulse response
[IR_mich2, VAR_mich2] = VARir(VAR_mich2,VARopt_mich2);

format short
disp(VAR_mich2.B) % the structural matrix
disp(chol(VAR_mich2.sigma,'lower')) %Cholesky
disp(IR_mich2(1:12,:,1))

%VAR.resid = VAR.resid * scale;
% Compute structural shocks (Tx2)
eps_short = (VAR_mich2.B\VAR_mich2.resid')';
disp(corr(eps_short)) % this shows that structural shocks are uncorrellated by construction


%% Plotting baseline, Michigan First, Michigan Second impulse responses

scale_mich = 90/IR_mich(1,2,2);
IR_mich = IR_mich * scale_mich;

scale_mich2 = 90/IR_mich2(1,1,1);
IR_mich2 = IR_mich2 * scale_mich2;

plot(IR(1:36, 5, 1), 'k', 'LineWidth', 2, 'DisplayName', 'Baseline VAR');
hold on;
plot(IR_mich(1:36, 6, 2),'r--', 'LineWidth', 2, 'DisplayName', 'Michigan First');
plot(IR_mich2(1:36, 6, 1),'x-', 'Color', 'b', 'LineWidth', 2, 'DisplayName', 'Michigan Second');
line(xlim, [0 0], 'Color', 'k', 'LineStyle', '--', 'HandleVisibility', 'off')
hold off;
xlim([1, 36])
xlabel('\bf{Months}', 'FontWeight', 'bold');
title('Industrial Production Response, %', 'FontSize', 14);
legend('Location', 'southeast', 'FontSize', 26);
set(legend, 'EdgeColor', 'none'); 

%% Robustness check - comparing the performance with multiple modifications
%  of the base model.

%A model with six monthly lags instead of 3

Xvnames = {'EPU','lsp','ffr', 'lemp', 'lip'};

%Corresponding labels to be used in plots
Xvnames_long = {'Economic policy uncertainty','Log of S&P', 'Federal funds rate','Log of Employment', 'Log of industrial production'};
% Number of endo variables
Xnvar = length(Xvnames);
% Create matrix X of variables to be used in the VAR
X = nan(nobs,Xnvar);
for ii=1:Xnvar
    X(:,ii) = data.(Xvnames{ii});
end

% Make a common sample by removing NaNs
[X, fo, lo] = CommonSample(X);

% Set the deterministic variable in the VAR (1=constant, 2=trend)
det = 1;
% Set number of lags
nlags = 6; 
% Estimate VAR by OLS
[VAR6, VARopt6] = VARmodel(X,nlags,det);

% Update the VARopt structure with additional details
VARopt6.vnames = Xvnames_long;
% Print at screen VAR coefficients and create table 
[TABLE, beta] = VARprint(VAR6,VARopt6,3);
 
VARopt6.ident = 'short';
% Update the VARopt structure with additional details 
VARopt6.vnames = Xvnames_long;            % variable names in plots
VARopt6.nsteps = 36;                      % max horizon of IRF
VARopt6.FigSize = [26,12];                % size of window (figures)
VARopt6.firstdate = date_data(1);          % first date in plots
VARopt6.frequency = 'm';                  % frequency of the data
VARopt6.pctg = 90;                        % confidence level is set to 90%

VARopt6.snames = {'\epsilon^{EPU}'};

% Compute impulse response
[IR6, VAR6] = VARir(VAR6,VARopt6);

scale = 90/IR6(1,1,1);
IR6 = scale * IR6;

%% A bivariate VAR (EPU and log of industrial production)

Xvnames = {'EPU', 'lip'};
Xvnames_long = {'Economic policy uncertainty', 'Log of industrial production'};
Xnvar = length(Xvnames);
% Create matrix X of variables to be used in the VAR
X = nan(nobs,Xnvar);
for ii=1:Xnvar
    X(:,ii) = data.(Xvnames{ii});
end

% Make a common sample by removing NaNs
[X, fo, lo] = CommonSample(X);

% Set the deterministic variable in the VAR (1=constant, 2=trend)
det = 1;
% Set number of lags
nlags = 3; 
% Estimate VAR by OLS
[VARbi, VARoptbi] = VARmodel(X,nlags,det);

% Update the VARopt structure with additional details
VARoptbi.vnames = Xvnames_long;
 
VARoptbi.ident = 'short';
% Update the VARopt structure with additional details 
VARoptbi.vnames = Xvnames_long;            % variable names in plots
VARoptbi.nsteps = 36;                      % max horizon of IRF
VARoptbi.FigSize = [26,12];                % size of window (figures)
VARoptbi.firstdate = date_data(1);          % first date in plots
VARoptbi.frequency = 'm';                  % frequency of the data
VARoptbi.pctg = 90;                        % confidence level is set to 90%

VARoptbi.snames = {'\epsilon^{EPU}'};

% Compute impulse response
[IRbi, VARbi] = VARir(VARbi,VARoptbi);

scalebi = 90/IRbi(1,1,1);
IRbi = scalebi * IRbi;

%% A bivariate VAR (EPU and log of industrial production) with reverse ordering

Xvnames = {'lip','EPU'};
Xvnames_long = {'Log of industrial production', 'Economic policy uncertainty'};
Xnvar = length(Xvnames);
% Create matrix X of variables to be used in the VAR
X = nan(nobs,Xnvar);
for ii=1:Xnvar
    X(:,ii) = data.(Xvnames{ii});
end

% Make a common sample by removing NaNs
[X, fo, lo] = CommonSample(X);

% Set the deterministic variable in the VAR (1=constant, 2=trend)
det = 1;
% Set number of lags
nlags = 3; 
% Estimate VAR by OLS
[VARbir, VARoptbir] = VARmodel(X,nlags,det);

% Update the VARopt structure with additional details
VARoptbir.vnames = Xvnames_long;
 
VARoptbir.ident = 'short';
% Update the VARopt structure with additional details 
VARoptbir.vnames = Xvnames_long;            % variable names in plots
VARoptbir.nsteps = 36;                      % max horizon of IRF
VARoptbir.FigSize = [26,12];                % size of window (figures)
VARoptbir.firstdate = date_data(1);          % first date in plots
VARoptbir.frequency = 'm';                  % frequency of the data
VARoptbir.pctg = 90;                        % confidence level is set to 90%

VARoptbir.snames = {'\epsilon^{EPU}'};

% Compute impulse response
[IRbir, VARbir] = VARir(VARbir,VARoptbir);

scalebir = 90/IRbir(1,2,2);
IRbir = scalebir * IRbir;

%% VAR including the VIX (after the EPU index)

% Selecting the list of endogenous variables
Xvnames = {'EPU', 'vix', 'lsp','ffr', 'lemp', 'lip'};

%Corresponding labels to be used in plots
Xvnames_long = {'Economic policy uncertainty', ' VIX','Log of S&P', 'Federal funds rate','Log of Employment', 'Log of industrial production'};
% Number of endo variables
Xnvar = length(Xvnames);
% Create matrix X of variables to be used in the VAR
X = nan(nobs,Xnvar);
for ii=1:Xnvar
    X(:,ii) = data.(Xvnames{ii});
end

% Make a common sample by removing NaNs
[X, fo, lo] = CommonSample(X);

% Set the deterministic variable in the VAR (1=constant, 2=trend)
det = 1;
% Set number of lags
nlags = 3; 
% Estimate VAR by OLS
[VARv, VARoptv] = VARmodel(X,nlags,det);

% Update the VARopt structure with additional details
VARoptv.vnames = Xvnames_long;
% Print at screen VAR coefficients and create table 
[TABLE, beta] = VARprint(VARv,VARoptv,3);
 
VARoptv.ident = 'short';
% Update the VARopt structure with additional details 
VARoptv.vnames = Xvnames_long;            % variable names in plots
VARoptv.nsteps = 36;                      % max horizon of IRF
VARoptv.FigSize = [26,12];                % size of window (figures)
VARoptv.firstdate = date_data(1);          % first date in plots
VARoptv.frequency = 'm';                  % frequency of the data
VARoptv.pctg = 90;                        % confidence level is set to 90%

VARoptv.snames = {'\epsilon^{EPU}'};

% Compute impulse response
[IRv, VARv] = VARir(VARv,VARoptv);

scalev = 90/IRv(1,1,1);
IRv = scalev * IRv;

%% VAR including the EU index (after the EPU index)

% Selecting the list of endogenous variables
Xvnames = {'EPU', 'eu', 'lsp','ffr', 'lemp', 'lip'};

%Corresponding labels to be used in plots
Xvnames_long = {'Economic policy uncertainty', ' EU','Log of S&P', 'Federal funds rate','Log of Employment', 'Log of industrial production'};
% Number of endo variables
Xnvar = length(Xvnames);
% Create matrix X of variables to be used in the VAR
X = nan(nobs,Xnvar);
for ii=1:Xnvar
    X(:,ii) = data.(Xvnames{ii});
end

% Make a common sample by removing NaNs
[X, fo, lo] = CommonSample(X);

% Set the deterministic variable in the VAR (1=constant, 2=trend)
det = 1;
% Set number of lags
nlags = 3; 
% Estimate VAR by OLS
[VARe, VARopte] = VARmodel(X,nlags,det);

% Update the VARopt structure with additional details
VARopte.vnames = Xvnames_long;

VARopte.ident = 'short';
% Update the VARopt structure with additional details 
VARopte.vnames = Xvnames_long;            % variable names in plots
VARopte.nsteps = 36;                      % max horizon of IRF
VARopte.FigSize = [26,12];                % size of window (figures)
VARopte.firstdate = date_data(1);          % first date in plots
VARopte.frequency = 'm';                  % frequency of the data
VARopte.pctg = 90;                        % confidence level is set to 90%

VARopte.snames = {'\epsilon^{EPU}'};

% Compute impulse response
[IRe, VARe] = VARir(VARe,VARopte);

scalee = 90/IRe(1,1,1);
IRe = scalee * IRe;

%% Dropping the S&P500 index

% Selecting the list of endogenous variables
Xvnames = {'EPU', 'ffr', 'lemp', 'lip'};

%Corresponding labels to be used in plots
Xvnames_long = {'Economic policy uncertainty', 'Federal funds rate','Log of Employment', 'Log of industrial production'};
% Number of endo variables
Xnvar = length(Xvnames);
% Create matrix X of variables to be used in the VAR
X = nan(nobs,Xnvar);
for ii=1:Xnvar
    X(:,ii) = data.(Xvnames{ii});
end

% Make a common sample by removing NaNs
[X, fo, lo] = CommonSample(X);

% Set the deterministic variable in the VAR (1=constant, 2=trend)
det = 1;
% Set number of lags
nlags = 3; 
% Estimate VAR by OLS
[VARs, VARopts] = VARmodel(X,nlags,det);

% Update the VARopt structure with additional details
VARopts.vnames = Xvnames_long;

VARopts.ident = 'short';
% Update the VARopt structure with additional details 
VARopts.vnames = Xvnames_long;            % variable names in plots
VARopts.nsteps = 36;                      % max horizon of IRF
VARopts.FigSize = [26,12];                % size of window (figures)
VARopts.firstdate = date_data(1);          % first date in plots
VARopts.frequency = 'm';                  % frequency of the data
VARopts.pctg = 90;                        % confidence level is set to 90%

VARopts.snames = {'\epsilon^{EPU}'};

% Compute impulse response
[IRs, VARs] = VARir(VARs,VARopts);

scales = 90/IRs(1,1,1);
IRs = scales * IRs;

%% VAR including time trends

% Selecting the list of endogenous variables
Xvnames = {'EPU', 'lsp','ffr', 'lemp', 'lip'};

%Corresponding labels to be used in plots
Xvnames_long = {'Economic policy uncertainty', 'Log of S&P', 'Federal funds rate','Log of Employment', 'Log of industrial production'};
% Number of endo variables
Xnvar = length(Xvnames);
% Create matrix X of variables to be used in the VAR
X = nan(nobs,Xnvar);
for ii=1:Xnvar
    X(:,ii) = data.(Xvnames{ii});
end

% Make a common sample by removing NaNs
[X, fo, lo] = CommonSample(X);

% Set the deterministic variable in the VAR (1=constant, 2=trend)
det = 2;
% Set number of lags
nlags = 3; 
% Estimate VAR by OLS
[VARt, VARoptt] = VARmodel(X,nlags,det);

% Update the VARopt structure with additional details
VARoptt.vnames = Xvnames_long;
% Print at screen VAR coefficients and create table 
[TABLE, beta] = VARprint(VARt,VARoptt,3);
 
VARoptt.ident = 'short';
% Update the VARopt structure with additional details 
VARoptt.vnames = Xvnames_long;            % variable names in plots
VARoptt.nsteps = 36;                      % max horizon of IRF
VARoptt.FigSize = [26,12];                % size of window (figures)
VARoptt.firstdate = date_data(1);          % first date in plots
VARoptt.frequency = 'm';                  % frequency of the data
VARoptt.pctg = 90;                        % confidence level is set to 90%

VARoptt.snames = {'\epsilon^{EPU}'};

% Compute impulse response
[IRt, VARt] = VARir(VARt,VARoptt);

scalet = 90/IRt(1,1,1);
IRt = scalev * IRt;
%% Plots of with baseline and several modifications

plot(IR(1:36,5,1), '+-', 'Color', 'k','DisplayName', 'Baseline VAR','LineWidth', 1.5)
hold on;
plot(IR6(1:36, 5, 1), '--', 'DisplayName', 'Baseline VAR with 6 lags','LineWidth', 1.5);
plot(IRbi(1:36, 2, 1), '-.', 'DisplayName', 'Bivariate VAR with EPU and log of industrial production','LineWidth', 1.5);
plot(IRbir(1:36, 1, 2), ':', 'DisplayName', 'Riverse bivariate VAR with EPU and log of industrial production','LineWidth', 1.5);
plot(IRv(1:36, 6, 1), 'DisplayName', 'Baseline VAR with VIX','LineWidth', 1.5);
plot(IRe(1:36, 6, 1), 'DisplayName', 'Baseline VAR with EU','LineWidth', 1.5);
plot(IRs(1:36, 4, 1), 'DisplayName', 'Baseline VAR without S&P 500','LineWidth', 1.5);
plot(IRt(1:36, 5, 1), '-..','DisplayName', 'Baseline VAR with time trends','LineWidth', 1.5);
hold off;
xlim([1, 36])
xlabel('\bf{Months}', 'FontWeight', 'bold', 'FontSize', 14);
ylabel('\bfIndustrial Production Response, %', 'FontSize', 14, 'FontWeight', 'bold');
legend('Location', 'southoutside', 'FontSize', 10);
set(legend, 'EdgeColor', 'none');