n.trials <- 1000
n <- 1:100
sample.mean <- numeric(100)
sample.var <- numeric(100)
for (i in n) {
  head.counts <- rbinom(n.trials, i, .5)
  sample.mean[i] <- mean(head.counts)
  sample.var[i] <- var(head.counts)
}
plot(n, sample.mean)
points(n, sample.var, col = 2) #points makes points on existing plot

qnorm(seq(.1, .9, by = .1), lower.tail = TRUE)

qqnorm(rnorm(500))
#mu and sigma parameters don't create line of best fit

x <- rexp(500)
par(mfrow = c(1,2))
hist(x)
qqnorm(x)

library(lattice)
x <- rexp(500)
par(mfrow = c(1,1))
hist(x)
qqmath(x, dist = "qexp")
