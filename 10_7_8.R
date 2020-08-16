# In Section 10.2.3, a formula for calculating PVE was given in Equation
# 10.8. We also saw that the PVE can be obtained using the sdev
# output of the prcomp() function.
# On the USArrests data, calculate PVE in two ways:



#   (a) Using the sdev output of the prcomp() function, as was done in
#       Section 10.2.3.



#   (b) By applying Equation 10.8 directly. That is, use the prcomp()
# function to compute the principal component loadings. Then,
# use those loadings in Equation 10.8 to obtain the PVE.
# These two approaches should give the same results.

# Hint: You will only obtain the same results in (a) and (b) if the same
# data is used in both cases. For instance, if in (a) you performed
# prcomp() using centered and scaled variables, then you must center
# and scale the variables before applying Equation 10.3 in (b).