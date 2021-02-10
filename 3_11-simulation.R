#initialized values

# null hypothesis is that p = 0.6
p_null <- 0.6

# testing against what value of p??
# initialize this to p=0.4
p_alt <- seq(0.4, 1.0, .001)

# n is the number of subjects given headache medicine
n <- 100

# Find the m that will get the Type I error under .05
for (i in 1:100) {
  p_value <- 1 - pbinom(i, size=n, prob=p_null)
  if p_value < 0.05 {break}
}
m1 <- p_value







# m is the critical value (e.g. #of subject who report improvement, beyond which we will
# reject the null hypothesis and claim success!)
pwr <- rep(0,601)
# Curve_one:
# a) set m (the test's cut-off), based on an a TypeI error of 0.05 (which involves p_null)
z <- qnorm(.95, p_null, sqrt(p_null*(1-p_null)/n))
m1 <- z*n

# b) calculate power against the alternative, given the cut-off
for (i in 1:601) {
  pwr[i] <- pnorm(z, p_alt[i], sqrt(p_alt[i]*(1-p_alt[i])/n), lower.tail = FALSE)
}
plot(p_alt, pwr, col="red",pch=20, cex=.25)

# Curve_two:


for (i in 1:601) {
  # a) set m (the test's cut-off), based on an a TypeII error of 0.05  (which involves p_alt)
  z <- qnorm(.05, p_alt, sqrt(p_alt*(1-p_alt)/n))
  
  # b) calculate p-value of the alternative, given the cut-off (the power)
  pwr[i] <- 1 - pnorm(z, p_alt[i], sqrt(p_alt[i]*(1-p_alt[i])/n), lower.tail = FALSE)
}
plot(p_alt, pwr, col="blue",pch=20)
