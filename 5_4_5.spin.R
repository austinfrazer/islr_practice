# 5.4.5
#  The 'Default' data is inside the ISLR package.

library(ISLR)
names(Default)
head(Default)

# a  Fit a logistic regression using income and balance to predict default.
glm_default = glm(default ~ income + balance, data = Default, family = binomial)
print(glm_default)
summary(glm_default)

# b  Using the validation set approach, estimate the test error of this model.
    # i)  Split the sample set into a training set and a test set

train = sample(nrow(Default), round(nrow(Default)/2))

    # ii) Fit the logit to only the training data.

glm_default_train = glm(default ~ income + balance, data = Default, family = binomial, subset = train)
print(glm_default_train)
summary(glm_default_train)

    # iii) Obtain a prediction of default status for each individual in the validation set.

# version given in solutions site.
glm.pred = rep("No", nrow(Default) - length(train))
glm.probs = predict(glm_default_train, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"


  # iv) Compare the validation set error, which is the fraction of the observations
  #    in the validation set that are misclassified

confusion_matrix = table(glm.pred, Default$default[-train])
1 - (confusion_matrix[1, 1] + confusion_matrix[2,2]) / sum(confusion_matrix)

# c) Repeat the process in b three times, each wih a different test - validation split.

# I want to write a function that will do this.

# set seed
set.seed(2)
runif(1)

train = sample(nrow(Default), round(nrow(Default)/2))

# ii) Fit the logit to only the training data.

PROPORTION_OF_TRAIN = 0.5  # (0, 1)
BASE_PROBABILITY    = 0.5  # (0, 1; default is usually .5)
NUM_ITER            = 3



glm_generator <- function(train_proportion = PROPORTION_OF_TRAIN,
                          decision_probability = BASE_PROBABILITY,
                          number_of_iterations = NUM_ITER){
  results = numeric(number_of_iterations)

  for(i in 1:number_of_iterations){
    set.seed(i)
    train = sample(nrow(Default), round(nrow(Default) * train_proportion))
    glm_default_train = glm(default ~ income + balance, data = Default, family = binomial, subset = train)
    glm.pred = rep("No", nrow(Default) - length(train))
    glm.probs = predict(glm_default_train, Default[-train, ], type = "response")
    glm.pred[glm.probs > decision_probability] = "Yes"
    confusion_matrix = table(glm.pred, Default$default[-train])
    results[i] = 1 - (confusion_matrix[1, 1] + confusion_matrix[2,2]) / sum(confusion_matrix)
  }
  return(results)
}

#  manual attempts to find best train split.
glm_generator(.5, .5, 3)
glm_generator(.9, .5, 3)
glm_generator(.1, .5, 3)
glm_generator(.01, .5, 3)
glm_generator(.7, .5, 3)
mean(glm_generator(.7, .5, 3))
mean(glm_generator(.7, .5, 10))
mean(glm_generator(.7, .5, 100))
mean(glm_generator(.7, .5, 1000))


# Loop driven training split parameter tuning.
parameter_tuning_result = numeric(length(seq(.1, .9, .05)))

for (split in seq(.1, .9, .05)){
  iter = 1 + (split - .1) / .05
  #print(paste(split, glm_generator(split, .5, 3)))
  parameter_tuning_result[iter] = as.numeric(mean(glm_generator(split, .5, 100)))
}

print(parameter_tuning_result)

#  checking an abnormal reading (a case of "0" error averaged over 1000 different splits).
#    error appears in results from for loop but does not appear when .25 is checked directly.
mean(glm_generator(.2, .5, 100))
mean(glm_generator(.25, .5, 100))
mean(glm_generator(.30, .5, 100))

# d)  Do this.  Borrowing from (b)
# i)  Split the sample set into a training set and a test set

train = sample(nrow(Default), round(nrow(Default)/2))

# ii) Fit the logit to only the training data.
names(Default)
contrasts(Default$student)[2]
dum_stud = contrasts(Default$student)
dum_stud


Default2 = Default
Default2[dum_stud] = contrasts(Default$student)


glm_default_train = glm(default ~ income + balance + as.factor(student), data = Default, family = binomial, subset = train)
print(glm_default_train)
summary(glm_default_train)

# Obtain a prediction of default status for each individual in the validation set.

glm.pred = rep("No", nrow(Default) - length(train))
glm.probs = predict(glm_default_train, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"


#  Compare the validation set error, which is the fraction of the observations
#    in the validation set that are misclassified

confusion_matrix = table(glm.pred, Default$default[-train])
1 - (confusion_matrix[1, 1] + confusion_matrix[2,2]) / sum(confusion_matrix)

#  This didn't really move the needle in terms of error reduction.

##########################3

############ Extra Credit!
#  If still time before Rachel is done with call, I want to rewrite this into Rmd for easier recall.
#  And update in git.

