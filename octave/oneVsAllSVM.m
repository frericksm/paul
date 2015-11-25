function [all_models] = oneVsAllSVM(X, y, num_labels, C)
%ONEVSALLSVM trains multiple svm classifiers and returns all
%the classifiers in a matrix all_models, where the i-th row of all_models 
%corresponds to the classifier for label i
%   [all_models] = ONEVSALL(X, y, num_labels, C) trains num_labels
%   logisitc regression classifiers and returns each of these classifiers
%   in a matrix all_models, where the i-th row of all_models corresponds 
%   to the classifier for label i


% Some useful variables

sigma = 0.1;
m = size(X, 1);
n = size(X, 2);

% You need to return the following variables correctly 
all_models = cell(num_labels, 1);

% Add ones to the X data matrix
%X = [ones(m, 1) X];

% ====================== YOUR CODE HERE ======================
% Instructions: You should complete the following code to train num_labels
%               svm classifiers with regularization
%               parameter C and sigma. 
%
% Hint: theta(:) will return a column vector.
%
% Hint: You can use y == c to obtain a vector of 1's and 0's that tell use 
%       whether the ground truth is true/false for this class.
%
% Note: For this assignment, we recommend using fmincg to optimize the cost
%       function. It is okay to use a for-loop (for c = 1:num_labels) to
%       loop over the different classes.
%
%       fmincg works similarly to fminunc, but is more efficient when we
%       are dealing with large number of parameters.
%
% Example Code for fmincg:
%
%     % Set Initial theta
%     initial_theta = zeros(n + 1, 1);
%     
%     % Set options for fminunc
%     options = optimset('GradObj', 'on', 'MaxIter', 50);
% 
%     % Run fmincg to obtain the optimal theta
%     % This function will return theta and the cost 
%     [theta] = ...
%         fmincg (@(t)(lrCostFunction(t, X, (y == c), lambda)), ...
%                 initial_theta, options);
%

labels = unique(y);
num_labels = length(labels);

for c = 1:num_labels

     % Set Initial theta
    % initial_theta = zeros(n + 1, 1);
     
     % Run svmTrain to obtain the optimal model
     % This function will return theta and the cost 


     y_bool = (y == c);


     model = svmTrain(X, y_bool, C, @(x1, x2) gaussianKernel(x1,x2,sigma));
         %fmincg (@(t)(lrCostFunction(t, X, (y == c), lambda)), initial_theta, options);

     all_models{c} = model;

end









% =========================================================================


end
