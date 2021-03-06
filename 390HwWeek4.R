MoE <- c(29.8, 33.2, 33.7, 35.3, 35.5, 36.1, 36.2, 36.3, 37.5, 37.7, 38.7, 38.8, 39.6, 41.0, 42.8, 42.8, 43.5, 45.6, 46.0, 46.9, 48.0, 49.3, 51.7, 62.6, 69.8, 79.5, 80.0)
Strength <- c(5.9, 7.2, 7.3, 6.3, 8.1, 6.8, 7.0, 7.6, 6.8, 6.5, 7.0, 6.3, 7.9, 9.0, 8.2, 8.7, 7.8, 9.7, 7.4, 7.7, 9.7, 7.8, 7.7, 11.6, 11.3, 11.8, 10.7)
par(mfrow = c(3, 2))
plot(MoE, Strength)
boxplot(MoE, ylab = "MoE")
boxplot(Strength, ylab = "Strength")
qqnorm(MoE, main = "MoE QQNorm")
qqnorm(Strength, main = "Strength QQNorm")
cor(MoE, Strength)
zMoE <- (MoE-mean(MoE))/sd(MoE)
zStr <- (Strength-mean(Strength))/sd(Strength)
1/26*(-1.15*-1.35 + -.89*-.56 + -.86*-.51 + -.73*-1.1 + -.72*-.02 + -.68*-.9 + -.67*-.68 + -.66*-.33 + -.57*-.8 + -.55*-.99 + -.48*-.69 + -.47*-1.11 + -.41*-.15 + -.3*.51 + -.17*.04 + -.17*.34 + -.12*-.21 + .03*.94 + .06*-.45 + .13*-.27 + .22*.94 + .31*-.2 + .49*-.27 + 1.31*2.08 + 1.86*1.9 + 2.59*2.2 + 2.63*1.54)
sd(Strength)
sd(MoE)
lm(Strength ~ MoE)
error <- Strength - (3.29 + .107*MoE)
my_error <- c(-.58, .36, .4, -.77, 1, -.35, -.16, .43, -.5, -.82, -.43, -1.14, .37, 1.32, .33, .83, -.14, .33, -.81, -.61, 1.27, -.77, -1.12, 1.61, .54, 0, -1.15)
sum(my_error^2)


#11-3
x <- c(45, 58, 71, 71, 85, 98, 108)
y <- c(3.20, 3.40, 3.47, 3.55, 3.60, 3.70, 3.80)
mean(x)
mean(y)
mean(x*y)
mean(x^2)
lm(y ~ x)
yresid <- 2.84 + .009*x
ypsilon <- y-yresid
anova(lm(y~x))
