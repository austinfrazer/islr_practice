# This problem involves the OJ data set which is part of the ISLR
# package.
 
library(ISLR)
library(e1071)
 
# (a) Create a training set containing a random sample of 800
# observations, and a test set containing the remaining
# observations.

nrow(OJ) -> N
training_sample_size = 800
training_sample_proportion = training_sample_size/N

#### Sourcing the my_sample function.
# On silver personal laptop
# path <- "C:\\Users\\Austin\\Documents\\ISLR_Labs\\MyWork"
# On work computer, 
   path <- 'P:\\AccountFinance\\Planning\\Private\\Strategy&Loyalty\\Users\\Austin Frazer\\Data_Science_Practice\\ISLR_Labs\\islr_practice'
# On black personal laptop
#   path <- 'C:\\Users\\Team Crazer\\Documents\\data_science_practice\\ISLR\\islr_practice'
# For other computers, uncomment these lines to get it to work
#   path = choose.dir()

source(paste0(path, "\\my_sample.R"))
set.seed(1)

train = my_sample(OJ, training_sample_proportion)
length(train)

OJ_test = OJ[-train, ]
OJ_train = OJ[train, ]

names(OJ)

# (b) Fit a support vector classifier to the training data using
# cost = 0.01, with Purchase as the response and the other variables
# as predictors. Use the summary() function to produce summary
# statistics, and describe the results obtained.


svmfit = svm(Purchase ~ .,
    data = OJ_train,
    kernel = "linear",
    cost = 0.01)

summary_svmfit = summary(svmfit)

number_of_support_vectors = summary_svmfit['tot.nSV']
number_of_training_points = lengths(summary_svmfit['fitted'])[[1]]
level_1 = summary_svmfit['levels'][[1]][1]
level_2 = summary_svmfit['levels'][[1]][2]
nsv_1 = summary_svmfit['nSV'][[1]][1]
nsv_2 = summary_svmfit['nSV'][[1]][2]


message = paste0("This support vector classifier creates ",
                 number_of_support_vectors,
                 " support vectors out of ",
                 number_of_training_points,
                 " training points.  Out of these, ",
                 nsv_1,
                 " belong to level ",
                 level_1,
                 ", and the remaining ",
                 nsv_2,
                 " belong to level ",
                 level_2,
                 ".")

print(message)


# (c) What are the training and test error rates?
y_pred_train = predict(svmfit, OJ_train)
confusion_matrix_train = table(predict = y_pred_train, truth = OJ_train$Purchase)
error_rate_train = round((confusion_matrix_train[2] + confusion_matrix_train[3])/ sum(confusion_matrix_train), 4)

y_pred_test = predict(svmfit, OJ_test)
confusion_matrix_test = table(predict = y_pred_test, truth = OJ_test$Purchase)
error_rate_test = round((confusion_matrix_test[2] + confusion_matrix_test[3])/ sum(confusion_matrix_test), 4)

if(error_rate_test > error_rate_train){
  error_message = "training error rate is better"
}else {error_message = "training error rate is not better"}

message = paste0("The ",
                 error_message,
                 ".  The training error rate is ",
                 error_rate_train,
                 " and the test error rate is ",
                 error_rate_test,
                 ".")

print(message)


# (d) Use the tune() function to select an optimal cost. Consider values
# in the range 0.01 to 10.

cost_range = seq(.01, 10, .01)
tune_out = tune(svm, Purchase ~ ., data = OJ_train, kernel = "linear",
                ranges = list(cost = cost_range))
 
tune_out_summary = summary(tune_out)

cost = tune_out$performances$cost
error = tune_out$performances$error
plot(x = cost, y = error, type = "l")

best_cost_parameter = tune_out$best.parameters[[1]]

message = paste0("The best cost parameter for this support vector classifier is ",
                 best_cost_parameter, ".")
print(message)



# (e) Compute the training and test error rates using this new value
# for cost.

svmfit_best_cost = svm(Purchase ~ .,
             data = OJ_train,
             kernel = "linear",
             cost = best_cost_parameter)


best_cost_number_of_support_vectors = svmfit_best_cost['tot.nSV']
best_cost_number_of_training_points = lengths(svmfit_best_cost['fitted'])[[1]]
best_cost_level_1 = svmfit_best_cost['levels'][[1]][1]
best_cost_level_2 = svmfit_best_cost['levels'][[1]][2]
best_cost_nsv_1 = svmfit_best_cost['nSV'][[1]][1]
best_cost_nsv_2 = svmfit_best_cost['nSV'][[1]][2]


message = paste0("This support vector classifier creates ",
                 best_cost_number_of_support_vectors,
                 " support vectors out of ",
                 best_cost_number_of_training_points,
                 " training points.  Out of these, ",
                 best_cost_nsv_1,
                 " belong to level ",
                 best_cost_level_1,
                 ", and the remaining ",
                 best_cost_nsv_2,
                 " belong to level ",
                 best_cost_level_2,
                 ".")

print(message)


best_cost_y_pred_train = predict(svmfit_best_cost, OJ_train)
best_cost_confusion_matrix_train = table(predict = best_cost_y_pred_train, truth = OJ_train$Purchase)
best_cost_error_rate_train = round((best_cost_confusion_matrix_train[2] + best_cost_confusion_matrix_train[3])/ sum(best_cost_confusion_matrix_train), 4)

best_cost_y_pred_test = predict(svmfit_best_cost, OJ_test)
best_cost_confusion_matrix_test = table(predict = best_cost_y_pred_test, truth = OJ_test$Purchase)
best_cost_error_rate_test = round((best_cost_confusion_matrix_test[2] + best_cost_confusion_matrix_test[3])/ sum(best_cost_confusion_matrix_test), 4)

if(best_cost_error_rate_test > best_cost_error_rate_train){
  best_cost_error_message = "training error rate is better"
}else {best_cost_error_message = "training error rate is not better"}

message = paste0("The ",
                 best_cost_error_message,
                 ".  The training error rate is ",
                 best_cost_error_rate_train,
                 ", and the test error rate is ",
                 best_cost_error_rate_test,
                 ".")

print(message)

# For comparison later
linear_kernel_results = c(best_cost_parameter, best_cost_error_rate_train, best_cost_error_rate_test)



# (f) Repeat parts (b) through (e) using a support vector machine
# with a radial kernel. Use the default value for gamma.

svmfit = svm(Purchase ~ .,
             data = OJ_train,
             kernel = "radial",
             cost = 0.01)

kernel_type = svmfit$call$kernel
number_of_support_vectors = svmfit['tot.nSV']
number_of_training_points = lengths(svmfit['fitted'])[[1]]
level_1 = svmfit['levels'][[1]][1]
level_2 = svmfit['levels'][[1]][2]
nsv_1 = svmfit['nSV'][[1]][1]
nsv_2 = svmfit['nSV'][[1]][2]

message = paste0("This ",
                 kernel_type,
                 " support vector machine creates ",
                 number_of_support_vectors,
                 " support vectors out of ",
                 number_of_training_points,
                 " training points.  Out of these, ",
                 nsv_1,
                 " belong to level ",
                 level_1,
                 ", and the remaining ",
                 nsv_2,
                 " belong to level ",
                 level_2,
                 ".")
print(message)


# What are the training and test error rates?  (this was (c))
y_pred_train = predict(svmfit, OJ_train)
confusion_matrix_train = table(predict = y_pred_train, truth = OJ_train$Purchase)
error_rate_train = round((confusion_matrix_train[2] + confusion_matrix_train[3])/ sum(confusion_matrix_train), 4)

y_pred_test = predict(svmfit, OJ_test)
confusion_matrix_test = table(predict = y_pred_test, truth = OJ_test$Purchase)
error_rate_test = round((confusion_matrix_test[2] + confusion_matrix_test[3])/ sum(confusion_matrix_test), 4)

if(error_rate_test > error_rate_train){
  error_message = "training error rate is better"
}else {error_message = "training error rate is not better"}

message = paste0("The ",
                 error_message,
                 ".  The training error rate is ",
                 error_rate_train,
                 " and the test error rate is ",
                 error_rate_test,
                 ".")
print(message)


#Use the tune() function to select an optimal cost. Consider values in the range 0.01 to 10.
#  this was (d)

cost_range = seq(.01, 10, .01)
tune_out = tune(svm, Purchase ~ ., data = OJ_train, kernel = "radial",
                ranges = list(cost = cost_range))

cost = tune_out$performances$cost
error = tune_out$performances$error
plot(x = cost, y = error, type = "l")

best_cost_parameter = tune_out$best.parameters[[1]]
message = paste0("The best cost parameter for this ",
                 kernel_type,
                 " support vector machine is ",
                 best_cost_parameter, ".")
print(message)

# Compute the training and test error rates using this new value for cost.  (Was (e))

svmfit_best_cost = svm(Purchase ~ .,
                       data = OJ_train,
                       kernel = "radial",
                       cost = best_cost_parameter)

best_cost_number_of_support_vectors = svmfit_best_cost['tot.nSV']
best_cost_number_of_training_points = lengths(svmfit_best_cost['fitted'])[[1]]
best_cost_level_1 = svmfit_best_cost['levels'][[1]][1]
best_cost_level_2 = svmfit_best_cost['levels'][[1]][2]
best_cost_nsv_1 = svmfit_best_cost['nSV'][[1]][1]
best_cost_nsv_2 = svmfit_best_cost['nSV'][[1]][2]


message = paste0("This ",
                 kernel_type,
                 " support vector machine creates ",
                 best_cost_number_of_support_vectors,
                 " support vectors out of ",
                 best_cost_number_of_training_points,
                 " training points.  Out of these, ",
                 best_cost_nsv_1,
                 " belong to level ",
                 best_cost_level_1,
                 ", and the remaining ",
                 best_cost_nsv_2,
                 " belong to level ",
                 best_cost_level_2,
                 ".")
print(message)


best_cost_y_pred_train = predict(svmfit_best_cost, OJ_train)
best_cost_confusion_matrix_train = table(predict = best_cost_y_pred_train, truth = OJ_train$Purchase)
best_cost_error_rate_train = round((best_cost_confusion_matrix_train[2] + best_cost_confusion_matrix_train[3])/ sum(best_cost_confusion_matrix_train), 4)

best_cost_y_pred_test = predict(svmfit_best_cost, OJ_test)
best_cost_confusion_matrix_test = table(predict = best_cost_y_pred_test, truth = OJ_test$Purchase)
best_cost_error_rate_test = round((best_cost_confusion_matrix_test[2] + best_cost_confusion_matrix_test[3])/ sum(best_cost_confusion_matrix_test), 4)

if(best_cost_error_rate_test > best_cost_error_rate_train){
  best_cost_error_message = "training error rate is better"
}else {best_cost_error_message = "training error rate is not better"}

message = paste0("The ",
                 best_cost_error_message,
                 ".  The training error rate is ",
                 best_cost_error_rate_train,
                 ", and the test error rate is ",
                 best_cost_error_rate_test,
                 ".")
print(message)

# For comparison later
radial_kernel_results = c(best_cost_parameter, best_cost_error_rate_train, best_cost_error_rate_test)


# (g) Repeat parts (b) through (e) using a support vector machine
# with a polynomial kernel. Set degree=2.

svmfit = svm( Purchase ~ .,
              data = OJ_train,
              kernel = "polynomial",
              degree = 2,
              cost = 0.01)

kernel_type = svmfit$call$kernel
number_of_support_vectors = svmfit['tot.nSV']
number_of_training_points = lengths(svmfit['fitted'])[[1]]
level_1 = svmfit['levels'][[1]][1]
level_2 = svmfit['levels'][[1]][2]
nsv_1 = svmfit['nSV'][[1]][1]
nsv_2 = svmfit['nSV'][[1]][2]

message = paste0("This ",
                 kernel_type,
                 " support vector machine creates ",
                 number_of_support_vectors,
                 " support vectors out of ",
                 number_of_training_points,
                 " training points.  Out of these, ",
                 nsv_1,
                 " belong to level ",
                 level_1,
                 ", and the remaining ",
                 nsv_2,
                 " belong to level ",
                 level_2,
                 ".")
print(message)


# What are the training and test error rates?  (this was (c))
y_pred_train = predict(svmfit, OJ_train)
confusion_matrix_train = table(predict = y_pred_train, truth = OJ_train$Purchase)
error_rate_train = round((confusion_matrix_train[2] + confusion_matrix_train[3])/ sum(confusion_matrix_train), 4)

y_pred_test = predict(svmfit, OJ_test)
confusion_matrix_test = table(predict = y_pred_test, truth = OJ_test$Purchase)
error_rate_test = round((confusion_matrix_test[2] + confusion_matrix_test[3])/ sum(confusion_matrix_test), 4)

if(error_rate_test > error_rate_train){
  error_message = "training error rate is better"
}else {error_message = "training error rate is not better"}

message = paste0("The ",
                 error_message,
                 ".  The training error rate is ",
                 error_rate_train,
                 " and the test error rate is ",
                 error_rate_test,
                 ".")
print(message)


#Use the tune() function to select an optimal cost. Consider values in the range 0.01 to 10.
#  this was (d)

cost_range = seq(.01, 10, .01)
tune_out = tune(svm,
                Purchase ~ .,
                data = OJ_train, 
                kernel = "polynomial",
                degree = 2,
                ranges = list(cost = cost_range))

cost = tune_out$performances$cost
error = tune_out$performances$error
plot(x = cost, y = error, type = "l")

best_cost_parameter = tune_out$best.parameters[[1]]
message = paste0("The best cost parameter for this ",
                 kernel_type,
                 " support vector machine is ",
                 best_cost_parameter, ".")
print(message)

# Compute the training and test error rates using this new value for cost.  (Was (e))

svmfit_best_cost = svm(Purchase ~ .,
                       data = OJ_train,
                       kernel = "polynomial",
                       degree = 2,
                       cost = best_cost_parameter)

best_cost_number_of_support_vectors = svmfit_best_cost['tot.nSV']
best_cost_number_of_training_points = lengths(svmfit_best_cost['fitted'])[[1]]
best_cost_level_1 = svmfit_best_cost['levels'][[1]][1]
best_cost_level_2 = svmfit_best_cost['levels'][[1]][2]
best_cost_nsv_1 = svmfit_best_cost['nSV'][[1]][1]
best_cost_nsv_2 = svmfit_best_cost['nSV'][[1]][2]

message = paste0("This ",
                 kernel_type,
                 " support vector machine creates ",
                 best_cost_number_of_support_vectors,
                 " support vectors out of ",
                 best_cost_number_of_training_points,
                 " training points.  Out of these, ",
                 best_cost_nsv_1,
                 " belong to level ",
                 best_cost_level_1,
                 ", and the remaining ",
                 best_cost_nsv_2,
                 " belong to level ",
                 best_cost_level_2,
                 ".")
print(message)


best_cost_y_pred_train = predict(svmfit_best_cost, OJ_train)
best_cost_confusion_matrix_train = table(predict = best_cost_y_pred_train, truth = OJ_train$Purchase)
best_cost_error_rate_train = round((best_cost_confusion_matrix_train[2] + best_cost_confusion_matrix_train[3])/ sum(best_cost_confusion_matrix_train), 4)

best_cost_y_pred_test = predict(svmfit_best_cost, OJ_test)
best_cost_confusion_matrix_test = table(predict = best_cost_y_pred_test, truth = OJ_test$Purchase)
best_cost_error_rate_test = round((best_cost_confusion_matrix_test[2] + best_cost_confusion_matrix_test[3])/ sum(best_cost_confusion_matrix_test), 4)

if(best_cost_error_rate_test > best_cost_error_rate_train){
  best_cost_error_message = "training error rate is better"
}else {best_cost_error_message = "training error rate is not better"}

message = paste0("The ",
                 best_cost_error_message,
                 ".  The training error rate is ",
                 best_cost_error_rate_train,
                 ", and the test error rate is ",
                 best_cost_error_rate_test,
                 ".")
print(message)

# For comparison later
polynomial_kernel_results = c(best_cost_parameter, best_cost_error_rate_train, best_cost_error_rate_test)
cost = 0.01)

kernel_type = svmfit$call$kernel
number_of_support_vectors = svmfit['tot.nSV']
number_of_training_points = lengths(svmfit['fitted'])[[1]]
level_1 = svmfit['levels'][[1]][1]
level_2 = svmfit['levels'][[1]][2]
nsv_1 = svmfit['nSV'][[1]][1]
nsv_2 = svmfit['nSV'][[1]][2]

message = paste0("This ",
                 kernel_type,
                 " support vector machine creates ",
                 number_of_support_vectors,
                 " support vectors out of ",
                 number_of_training_points,
                 " training points.  Out of these, ",
                 nsv_1,
                 " belong to level ",
                 level_1,
                 ", and the remaining ",
                 nsv_2,
                 " belong to level ",
                 level_2,
                 ".")
print(message)


# What are the training and test error rates?  (this was (c))
y_pred_train = predict(svmfit, OJ_train)
confusion_matrix_train = table(predict = y_pred_train, truth = OJ_train$Purchase)
error_rate_train = round((confusion_matrix_train[2] + confusion_matrix_train[3])/ sum(confusion_matrix_train), 4)

y_pred_test = predict(svmfit, OJ_test)
confusion_matrix_test = table(predict = y_pred_test, truth = OJ_test$Purchase)
error_rate_test = round((confusion_matrix_test[2] + confusion_matrix_test[3])/ sum(confusion_matrix_test), 4)

if(error_rate_test > error_rate_train){
  error_message = "training error rate is better"
}else {error_message = "training error rate is not better"}

message = paste0("The ",
                 error_message,
                 ".  The training error rate is ",
                 error_rate_train,
                 " and the test error rate is ",
                 error_rate_test,
                 ".")
print(message)


#Use the tune() function to select an optimal cost. Consider values in the range 0.01 to 10.
#  this was (d)

cost_range = seq(.01, 10, .01)
tune_out = tune(svm, Purchase ~ ., data = OJ_train, kernel = "polynomial",
                ranges = list(cost = cost_range))

cost = tune_out$performances$cost
error = tune_out$performances$error
plot(x = cost, y = error, type = "l")

best_cost_parameter = tune_out$best.parameters[[1]]
message = paste0("The best cost parameter for this ",
                 kernel_type,
                 " support vector machine is ",
                 best_cost_parameter, ".")
print(message)

# Compute the training and test error rates using this new value for cost.  (Was (e))

svmfit_best_cost = svm(Purchase ~ .,
                       data = OJ_train,
                       kernel = "polynomial",
                       cost = best_cost_parameter)

best_cost_number_of_support_vectors = svmfit_best_cost['tot.nSV']
best_cost_number_of_training_points = lengths(svmfit_best_cost['fitted'])[[1]]
best_cost_level_1 = svmfit_best_cost['levels'][[1]][1]
best_cost_level_2 = svmfit_best_cost['levels'][[1]][2]
best_cost_nsv_1 = svmfit_best_cost['nSV'][[1]][1]
best_cost_nsv_2 = svmfit_best_cost['nSV'][[1]][2]

message = paste0("This support vector classifier creates ",
                 best_cost_number_of_support_vectors,
                 " support vectors out of ",
                 best_cost_number_of_training_points,
                 " training points.  Out of these, ",
                 best_cost_nsv_1,
                 " belong to level ",
                 best_cost_level_1,
                 ", and the remaining ",
                 best_cost_nsv_2,
                 " belong to level ",
                 best_cost_level_2,
                 ".")
print(message)


best_cost_y_pred_train = predict(svmfit_best_cost, OJ_train)
best_cost_confusion_matrix_train = table(predict = best_cost_y_pred_train, truth = OJ_train$Purchase)
best_cost_error_rate_train = round((best_cost_confusion_matrix_train[2] + best_cost_confusion_matrix_train[3])/ sum(best_cost_confusion_matrix_train), 4)

best_cost_y_pred_test = predict(svmfit_best_cost, OJ_test)
best_cost_confusion_matrix_test = table(predict = best_cost_y_pred_test, truth = OJ_test$Purchase)
best_cost_error_rate_test = round((best_cost_confusion_matrix_test[2] + best_cost_confusion_matrix_test[3])/ sum(best_cost_confusion_matrix_test), 4)

if(best_cost_error_rate_test > best_cost_error_rate_train){
  best_cost_error_message = "training error rate is better"
}else {best_cost_error_message = "training error rate is not better"}

message = paste0("The ",
                 best_cost_error_message,
                 ".  The training error rate is ",
                 best_cost_error_rate_train,
                 ", and the test error rate is ",
                 best_cost_error_rate_test,
                 ".")
print(message)

# For comparison later
polynomial_kernel_results = c(best_cost_parameter, best_cost_error_rate_train, best_cost_error_rate_test)


# (h) Overall, which approach seems to give the best results on this data?
results_dataframe = data.frame(linear_kernel_results, radial_kernel_results, polynomial_kernel_results)
row.names(results_dataframe) = c('best_cost', 'train_error_rate', 'test_error_rate')
results_dataframe = t(results_dataframe)

results_dateframe

best_svm_kernel_type_here = names(which.min(results_dataframe[ , 'test_error_rate']))

message = paste0("The best approach for this data is found in the ",
                 best_svm_kernel_type_here,
                 ".")

print(message)



