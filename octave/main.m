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

lambda = 1.0;
[all_theta] = oneVsAll(X_train, y_train, num_labels, lambda);

%% ================ Part 3: Predict for One-Vs-All ================
%  After ...
pred = predictOneVsAll(all_theta, X_test);

fprintf('\nTraining Set Accuracy: %f\n', mean(double(pred == y_test)) * 100);


