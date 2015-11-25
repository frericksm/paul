function [C, sigma] = chooseBestParams(X, y, Xval, yval)
%CHOOSEBESTPARAMS returns your choice of C and sigma 
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = CHOOSEBESTPARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% You need to return the following variables correctly.
C = 1;
sigma = 0.1;

% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%

steps = [0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30];
num_steps = length(steps);
prediction_error = -1;

do_work = false;

if (do_work)
  for i = 1:num_steps
    for j  = 1:num_steps
      C_temp = steps(i);
      sigma_temp = steps(j); 
      fprintf(['C = %f,  sigma = %f, C_temp = %f,  sigma_temp = %f,  pred_err = %f\n'], 
              C, sigma, C_temp, sigma_temp, prediction_error );

      model= svmTrain(X, y, C_temp, @(x1, x2) gaussianKernel(x1, x2, sigma_temp));
      predictions = svmPredict(model, Xval);
      new_prediction_error = mean(double(predictions ~= yval));
      if (prediction_error == -1)
        C = C_temp;
        sigma = sigma_temp;
        prediction_error = new_prediction_error;
      elseif (new_prediction_error < prediction_error) 
        C = C_temp;
        sigma = sigma_temp;
        prediction_error = new_prediction_error;
      endif
    end    
  end
endif

end
% =========================================================================
