%% Octave script to learn from bundesliga data


%% Initialization
clear ; close all; clc

%% Read features and labels into X
raw_data = csvread("../resources/features.csv");

%% number of features
n = size(raw_data)(2);

%% number of samples
m = size(raw_data)(1);


raw_data_shuffled = raw_data(randperm(m),:);


X = raw_data_shuffled(:,2:n);
y = raw_data_shuffled(:,1);

labels = unique(y);
num_labels = length(unique(y));

label_to_index = {};
index_to_label = {};

%%y_labels = zeros(length(y));
%%for i = 1:length(labels)
%%    label_to_index{labels(i)} = i;
%%    index_to_label{i} = labels(i);
%%end
%%for i = 1:length(y)
%%    y(i) = label_to_index{y(i)};
%%end;

i_train = int32(m * 0.6);
i_test = int32(m * 0.8);

X_train = X(1:i_train, :); 
y_train = y(1:i_train); 

X_test = X(i_train + 1 :i_test, :); 
y_test = y(i_train + 1 :i_test); 

X_cv = X(i_test + 1:m, :); 
y_cv = y(i_test + 1:m); 


%% Train 

C = 1.0;
sigma = 0.1;
[X_train_normalized, mu_X_train, sigma_X_train] = featureNormalize(X_train);


all_models = oneVsAllSVM(X_train_normalized, y_train, num_labels, C);

%% ================ Part 3: Predict for One-Vs-All ================
%  After ...

X_test_normalized_1 = bsxfun(@minus, X_test, mu_X_train);
X_test_normalized = bsxfun(@rdivide, X_test_normalized_1, sigma_X_train);

pred = predictOneVsAllSVM(all_models, X_test_normalized);

fprintf('\nTraining Set Accuracy: %f\n', mean(double(pred == y_test)) * 100);


