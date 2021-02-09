#set fixed values

# null hypothesis is that p = 0.6
p_null <- 0.6

# testing against what value of p??
# initialize this to p=0.4
p_alt <- 0.4

# n is the number of subjects given headache medicine
n <- 100

# m is the critical value (e.g. #of subject who report improvement, beyond which we will
# reject the null hypothesis and claim success!)
# Curve_one, set m based on an a TypeI error of 0.05
z <- qnorm(p_alt, sqrt(p_alt*(1-p_alt)/n), 0.05)
m <- 