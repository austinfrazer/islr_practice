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





# (c) Perform cross-validation or another approach to select the optimal
# degree for the polynomial, and explain your results.





# (d) Use the bs() function to fit a regression spline to predict nox
# using dis. Report the output for the fit using four degrees of
# freedom. How did you choose the knots? Plot the resulting fit.





# (e) Now fit a regression spline for a range of degrees of freedom, and
# plot the resulting fits and report the resulting RSS. Describe the
# results obtained.





# (f) Perform cross-validation or another approach in order to select
# the best degrees of freedom for a regression spline on this data.
# Describe your results.
