# This question uses the variables dis (the weighted mean of distances
# to five Boston employment centers) and nox (nitrogen oxides concentration
# in parts per 10 million) from the Boston data. We will treat
# dis as the predictor and nox as the response.
library(MASS)



# (a) Use the poly() function to fit a cubic polynomial regression to
# predict nox using dis. Report the regression output, and plot
# the resulting data and polynomial fits.



fit_nox3 = lm(nox ~ poly(dis, 3), data = Boston)

dislims = range(Boston$dis)
dis.grid = seq(from = dislims[1], to = dislims[2])
preds = predict(fit_nox3, newdata = list(dis = dis.grid), se = TRUE)
se.bands = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(Boston$dis, Boston$nox, xlim = dislims, cex = .5, col = "darkgrey")
title("Degree - 3 Polynomial", outer = T)
lines(dis.grid, preds$fit, lwd = 2, col = "blue")
matlines(dis.grid, se.bands, lwd = 1, col = "blue", lty = 3)


# (b) Plot the polynomial fits for a range of different polynomial
# degrees (say, from 1 to 10), and report the associated residual
# sum of squares.


##  RSS generator
all_rss = rep(NA, 10)
for (i in 1:10){
  assign(paste0("fit_nox_", i), lm(nox ~ poly(dis, i), data = Boston))
  all_rss[i] = sum(get(paste0("fit_nox_", i))$residuals ^ 2)
}
round(all_rss, 3)

##  Plot Generator
#### Could get neither mfrow nor layout to work to display more than one
#### plot at a time coming out of the loop.
# par(mfrow = c(4,3))
# layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 5, 2, byrow = TRUE))
for (i in 1:10){
  assign(paste0("fit_nox_", i), lm(nox ~ poly(dis, i), data = Boston))
  dislims = range(Boston$dis)
  dis.grid = seq(from = dislims[1], to = dislims[2])
  preds = predict(get(paste0("fit_nox_", i)), newdata = list(dis = dis.grid), se = TRUE)
  se.bands = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
  
  par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
  plot(Boston$dis, Boston$nox, xlim = dislims, cex = .5, col = "darkgrey")
  title(paste0("Degree - ", i, " Polynomial"), outer = T)
  lines(dis.grid, preds$fit, lwd = 2, col = "blue")
  matlines(dis.grid, se.bands, lwd = 1, col = "blue", lty = 3)
}



#### Combined RSS and plot generator.
all_rss = rep(NA, 10)
for (i in 1:10){
  assign(paste0("fit_nox_", i), lm(nox ~ poly(dis, i), data = Boston))
  all_rss[i] = sum(get(paste0("fit_nox_", i))$residuals ^ 2)

  dislims = range(Boston$dis)
  dis.grid = seq(from = dislims[1], to = dislims[2])
  preds = predict(get(paste0("fit_nox_", i)), newdata = list(dis = dis.grid), se = TRUE)
  se.bands = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
  
  par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
  plot(Boston$dis, Boston$nox, xlim = dislims, cex = .5, col = "darkgrey")
  title(paste0("Degree - ", i, " Polynomial"), outer = T)
  lines(dis.grid, preds$fit, lwd = 2, col = "blue")
  matlines(dis.grid, se.bands, lwd = 1, col = "blue", lty = 3)
}
round(all_rss, 3)

# (c) Perform cross-validation or another approach to select the optimal
# degree for the polynomial, and explain your results.

### Reused the third method I used in 7.9.6
# I could also try the k-fold cross validation technique given on page 193.
#  I went ahead and expanded it by repeating it 10 times with 10 different random seeds.

num_resamples = 10
num_folds     = 10
cv.error = matrix(0, num_resamples, num_folds)
for (i in 1:num_resamples){
  seed_number = round(runif(1) * 2147483647, 0)     #Should be able to get all seeds except probably 0.  There are 2^32 seeds.
  set.seed(seed_number)
  print(paste0("Now using seed number, ", seed_number))
  for (j in 1:num_folds){
    glm.fit = glm(nox ~ poly(dis, i), data = Boston)
    cv.error[i, j] = cv.glm(Boston, glm.fit, K = num_folds)$delta[1]
  }}
cv.error
average_cv.error = apply(cv.error, MARGIN = 2, FUN = mean)
average_cv.error
best_poly_order = which.min(average_cv.error)
print(paste0("I should choose a level ", best_poly_order, " order polynomial."))



# (d) Use the bs() function to fit a regression spline to predict nox
# using dis. Report the output for the fit using four degrees of
# freedom. How did you choose the knots? Plot the resulting fit.





# (e) Now fit a regression spline for a range of degrees of freedom, and
# plot the resulting fits and report the resulting RSS. Describe the
# results obtained.





# (f) Perform cross-validation or another approach in order to select
# the best degrees of freedom for a regression spline on this data.
# Describe your results.
