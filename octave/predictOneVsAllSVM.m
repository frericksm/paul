function p = predictOneVsAllSVM(all_models, X)
%PREDICT Predict the label for a trained one-vs-all classifier. The labels 
%are in the range 1..K, where K = size(all_models, 1). 
%  p = PREDICTONEVSALL(all_models, X) will return a vector of predictions
%  for each example in the matrix X. Note that X contains the examples in
%  rows. all_models is a matrix where the i-th row is a trained logistic
%  regression theta vector for the i-th class. You should set p to a vector
%  of values from 1..K (e.g., p = [1; 3; 1; 2] predicts classes 1, 3, 1, 2
%  for 4 examples) 

m = size(X, 1);
num_labels = size(all_models, 1);

% You need to return the following variables correctly 
p = zeros(size(X, 1), 1);

% Add ones to the X data matrix
%X = [ones(m, 1) X];

% ====================== YOUR CODE HERE ======================
% Instructions: Complete the following code to make predictions using
%               your learned logistic regression parameters (one-vs-all).
%               You should set p to a vector of predictions (from 1 to
%               num_labels).
%
% Hint: This code can be done all vectorized using the max function.
%       In particular, the max function can also return the index of the 
%       max element, for more information see 'help max'. If your examples 
%       are in rows, then, you can use max(A, [], 2) to obtain the max 
%       for each row.
%       

lm = length(all_models);

predictions = zeros(m,num_labels);

for i = 1:lm
  fprintf('\na1');
  pd = svmPredict(all_models{i}, X);
  pd(1:4,:)
  predictions(:,i) = pd;
  fprintf("\na2");
end;

predictions(1:4,:)

%predictions
[m, p2] = max(predictions, [], 2); 


% Convert predictions into 0 / 1

%p2(1:4,:)

p(p2 >= 0) =  1;
p(p2 <  0) =  0;

%p(1:4,:)

% =========================================================================


end
