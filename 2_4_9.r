# Chapter 2, Problem 9.

# Use Auto data.  Remove missing vales from the data.
input_filepath = file.choose()

auto = read.csv(file = input_filepath, header = TRUE, na.strings = "?")

dim(auto) # 397 9
auto = na.omit(auto)
dim(auto) # 392 9

attach(auto)
# a)  Which predictors are qualitative and which are quantitative?
str(auto)
  # Quantitative:  mpg, displacement, horsepower, weight, acceleration
  # Qualitative:  cylinders, year, origin, name  (cylinder could be quant, and maybe year)

# b)  Range of each quantitative predictior
quant_variables = c("mpg", "displacement", "horsepower", "weight", "acceleration")
apply(auto[quant_variables], 2, range)


# c)  Standard deviation of each quantitative predictor
apply(auto[quant_variables], 2, sd)

# d)  Now remove the 10th through 85th observation.
    # Get range, mean, and sd of each predictor in that subset.
auto_subset <- auto[-c(10:85), ]

apply(auto_subset[quant_variables], 2, range)
apply(auto_subset[quant_variables], 2, mean)
apply(auto_subset[quant_variables], 2, sd)

# e)  Using the full dataset, investigate predictors graphically...
plot(auto[quant_variables])
plot(cylinders, mpg)
plot(as.factor(cylinders), mpg)

par(mfrow = c(2, 2))
   
apply(auto[quant_variables], 2, hist)
hist(mpg)
hist(acceleration)
class(auto[quant_variables])

# f) What variables would help predict mpg?
my_model = lm(mpg ~ displacement + horsepower + weight + acceleration)

my_model
summary(my_model)
my_model2 = lm(mpg ~ horsepower + weight + as.factor(cylinders))

my_model3 = lm(mpg ~ ., data = auto)
summary(my_model3)

summary(lm(mpg ~., data = auto[quant_variables]))

summary(my_model2)
hey = names(auto[-mpg])

length(hey)

all_variables = ""
hey
for (i in 1:length(hey)){
  all_variables = paste()

  
?lm  
}
names(auto)
my_model3 = lm(mpg ~ names(auto[-mpg]))
auto3 = auto[-mpg]
dim(auto3)
dim(auto)
cor(mpg, displacement)
cor(mpg, horsepower)

plot(auto)

