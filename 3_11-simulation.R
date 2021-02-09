#set fixed values

# null hypothesis is that p = 0.6
p_null <- 0.6

# testing against what value of p??
# initialize this to p=0.4
p_alt <- seq(0.4, 1.0, .01)

# n is the number of subjects given headache medicine
n <- 100

# m is the critical value (e.g. #of subject who report improvement, beyond which we will
# reject the null hypothesis and claim success!)
pwr <- rep(0,61)
# Curve_one:
for (i in 1:61) {
  # a) set m (the test's cut-off), based on an a TypeI error of 0.05
  z <- qnorm(.95, p_null, sqrt(p_null*(1-p_null)/n))
  m <- z*n
  
  # b) calculate p-value of the alternative, given the cut-off (the power)
  pwr[i] <- pnorm(z, p_alt[i], sqrt(p_alt[i]*(1-p_alt[i])/n), lower.tail = FALSE)
}

plot(p_alt,pwr)
