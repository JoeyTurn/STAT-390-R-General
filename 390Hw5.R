trans <- read.table("https://www.stat.washington.edu/marzban/390/spring20/transform_dat.txt", header=T)
x <- trans$x
y <- trans$y

par(mfrow = c(1,2))
plot(x, y)
new.x <- sqrt(x)
plot(new.x, y)
lm.1 <- lm(y ~ new.x)
abline(lm.1)
summary(lm.1)

set.seed(123)
n = 100
x1 = runif(n,1,3)
y = 2 + 3*x1 + rnorm(n, 0, 1)
plot(x,y)
lm.1 <- lm(y ~ x)
s <- summary(lm.1)
s$r.squared
x2 <- runif(n, 1, 3)
x3 <- runif(n, 1, 3)
x4 <- runif(n, 1, 3)

lm.2 <- lm(y ~ x1 + x2)
summary(lm.2)$r.squared
lm.3 <- lm(y ~ x1 + x2 + x3)
summary(lm.3)$r.squared
lm.4 <- lm(y ~ x1 + x2 + x3 + x4)
summary(lm.4)$r.squared
plot(lm.4)



#13.2
depth <- c(8.9, 36.6, 36.8, 6.1, 6.9, 6.9, 7.3, 8.4, 6.5, 8.0, 4.5, 9.9, 2.9, 2.0) #x1
content <- c(31.5, 27, 25.9, 39.1, 39.2, 38.3, 33.9, 33.8, 27.9, 33.1, 26.3, 37, 34.6, 36.4) #x2
depth2 <- depth^2
content2 <- content^2
strength <- c(14.7, 48, 25.6, 10, 16, 16.8, 20.7, 38.8, 16.9, 27, 16, 24.9, 7.3, 12.8) #y
# y from x1, x2, x3 = x1^2, x4 = x2^2, and x5 = x1*x2
lm.strength <- lm(strength ~ depth + content + depth2 + content2 + depth*content)
summary(lm.strength)

lm.strength2 <- lm(strength ~ depth + content)
summary(lm.strength2)

#13.3
hw3dat1 <- read.table("https://www.stat.washington.edu/marzban/390/spring20/hw_3_dat1.txt", header=T)
hw3dat2 <- read.table("https://www.stat.washington.edu/marzban/390/spring20/hw_3_dat2.txt", header=T)
plot(hw3dat1$x1, hw3dat1$x2) 
plot(hw3dat1$x2, hw3dat1$y) #interaction

plot(hw3dat2$x1, hw3dat2$x2) #obv colinear
plot(hw3dat2$x1^2, hw3dat2$y) #looks quadratic for both

lm.hw31 <- lm(hw3dat1$y ~ hw3dat1$x1*hw3dat1$x2)
summary(lm.hw31)
hw32x12 <- hw3dat2$x1^2
hw32x22 <- hw3dat2$x2^2
lm.hw32 <- lm(hw3dat2$y ~ hw3dat2$x1 + hw3dat2$x2 + hw32x12 + hw32x22 + hw3dat2$x1*hw3dat2$x2)
summary(lm.hw32)

#13.5
par(mfrow = c(1,2))
n <- 5000
xbars <- numeric(n)
for (i in 1:n) {
  rno <- rnorm(50)
  xbars[i] <- mean(rno)
}
hist(xbars)
xmins <- numeric(n)
for (i in 1:n) {
  rno <- rnorm(50)
  xmins[i] <- min(rno)
}
hist(xmins)

#14.1
par(mfrow = c(1,1))
n <- 5000
expmeans <- numeric(n)
for (i in 1:n) {
  expmeans[i] <- mean(rexp(100, 2))
}
qqnorm(expmeans)


#Week 6

midvals <- c(429, 430, 430, 431, 436, 437)
highvals <- c(440, 441, 445, 446, 447)
boxplot(midvals, highvals)

mean(midvals)
mean(highvals)
(sd(midvals)^2/6)^2
(sd(highvals)^2/5)^2

x1 = c(-0.27, -0.14, 1.61, 0.09, 0.00, 2.07, 0.56, -1.67, -0.51, -0.54)
x2 = c(-0.32, 0.20, 1.93, 0.54, 0.75, 1.77, 0.84, -0.29, -0.33, 0.17)
x12 = x1-x2
mean(x12)
sd(x12)
y1 = c(-0.27, -0.14, 1.61, 0.09, 0.00, 2.07, 0.56, -1.67, -0.51, -0.54)
y2 = c( 0.20 , 0.54, -0.33, 1.93, -0.32, 1.77, 0.75, 0.17, -0.29, 0.84)
y12 <- y1-y2
mean(y1)
s1 <- sd(y1)
mean(y2)
s2 <- sd(y2)
