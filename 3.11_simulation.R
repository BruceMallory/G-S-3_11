library(ggplot2)
#initialized values

# null hypothesis is that p = 0.6
p_null <- 0.6

# n is the number of subjects given headache medicine
n <- 100

# Find the m that will get the Type I error under .05
# The right tail when p_alt=p_null
for (i in 1:100) {
  p_value <- 1 - pbinom(i, size=n, prob=p_null)
  if (p_value < 0.05) break
}
m1 <- i

# Find the m that will get the Type II error under .05
# The left tail when p_alt=p_null
for (i in 1:100) {
  p_value <- pbinom(i, size=n, prob=.8)
  if (p_value > 0.05) break
}
m2 <- (i)

df <- data.frame("p_alt"=seq(0.4, 1.0, .001), "pwr1"=rep(0,601), "pwr2"=rep(0,601))

for (i in 1:601) {
  df$pwr1[i] <- pnorm(m1/n, df$p_alt[i], sqrt(df$p_alt[i]*(1-df$p_alt[i])/n), lower.tail = FALSE)
  df$pwr2[i] <- pnorm(m2/n, df$p_alt[i], sqrt(df$p_alt[i]*(1-df$p_alt[i])/n), lower.tail = FALSE)
}

box <- data.frame(x1=.6, x2=.8, y1=.05, y2=.95)

ggplot(df, aes(x = p_alt))+
  geom_path(aes(y = pwr1, colour = "69"), col="red") +
  geom_path(aes(y = pwr2, colour = "73"), col="blue") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("") + scale_x_continuous(n.breaks=10) +
  ylab("") + scale_y_continuous(n.breaks=10) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y1, 
                   colour = "segment"), col="grey", data = box) +
  geom_segment(aes(x = x2, y = y1, xend = x2, yend = y2, 
                   colour = "segment"), col="grey", data = box) +
  geom_segment(aes(x = x2, y = y2, xend = x1, yend = y2, 
                   colour = "segment"), col="grey", data = box) +
  geom_segment(aes(x = x1, y = y2, xend = x1, yend = y1, 
                   colour = "segment"), col="grey", data = box)

  
  
 