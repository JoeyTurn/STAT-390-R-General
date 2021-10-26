data <- read.csv2("390hw1data.csv", header = FALSE)
coin_flip <- c('T', 'H', 'T', 'T', 'H', 'H', 'H', 'T' , 'H' , 'H' , 'T' , 'H' , 'T' , 'T' , 'T' , 'T' , 'T' , 'H' , 'T' , 'H' , 'H' , 'T' , 'T' , 'H' , 'T' , 'H' , 'H' , 'T' , 'T' , 'T')
die_roll <- c(3, 2, 2, 3, 4, 1, 4, 1, 2, 5, 6, 4, 4, 1, 2, 4, 3, 2, 5, 6, 2, 6, 3, 2, 6, 5, 1, 2, 1, 2)
amount_bet <- c(15.73, 8.31, 15.69, 21.79, 6.48, 9.47, 16.10, 26.25, 10.56, 17.71, 3.13, 15.57, 22.66, 8.15, 14.76, 7.85, 21.73, 14.78, 28.32, 21.32, 8.57, 12.61, 21.83, 23.95, 26.62, 17.97, 24.53, 0.83, 16.84, 11.86)
total_lost <- c(67.47, 46.38, 57.66, 73.26, 56.81, 66.26, 74.26, 82.21, 67.72, 59.73, 1.52, 64.86, 75.98, 16.77, 46.62, 74.45, 88.27, 39.94, 98.34, 37.37, 8.53, 73.74, 76.47, 74.47, 92.63, 65.12, 83.27, 12.74, 53.32, 47.73)

coin_flip

### Week 9

n = 10
n.trial = 5000
x = c(1:n)
y_true = 10 + 2*x
sigma_eps = 15
betahats <- numeric(n.trial)
par(mfrow=c(1,1))
set.seed(123)

for(trial in 1:n.trial){
  y_obs = y_true + rnorm(n,0,sigma_eps)
  lm.1 = lm(y_obs ~ x)
  betahats[trial] <- lm.1$coefficients[2]
}
hist(betahats)
#b)
mean(betahats) #meanbetahat = 1.969032, meanbeta = 2
#c)
sd(betahats); sigma_eps/sqrt(sum((x-mean(x))^2)) #about the same
qqnorm(betahats)
abline(mean(betahats), sigma_eps/sqrt(sum((x-mean(x))^2)))

x <-  c(89, 177, 189, 354, 362, 442, 965)
y <- c(.40, .60, .48, .66, .61, .69, .99)
plot(x, y)
lm.1 <- lm(y~x)
lm.1$coeff[2] #0.0006210758
abline(lm.1$coeff[1], lm.1$coeff[2])
betahat <- 0.0006210758
aov.1 <- aov(lm.1)
summary(aov.1) #S_e = .0146
S_e <- .0146
Sxxsqrt <- sqrt(sum((x-mean(x))^2))
tstar <- 2.571
betahat + (tstar * S_e/Sxxsqrt)
betahat - (tstar * S_e/Sxxsqrt)
mean(x)

### Week 8
ab1 <- amount_bet[die_roll == 1]
ab2 <- amount_bet[die_roll == 2]
ab3 <- amount_bet[die_roll == 3]
ab4 <- amount_bet[die_roll == 4]
ab5 <- amount_bet[die_roll == 5]
ab6 <- amount_bet[die_roll == 6]
aov1 <- aov(amount_bet ~ as.factor(die_roll))
aov2 <- aov((amount_bet+2)~as.factor(die_roll))
summary(aov1)
summary(aov2)
qqnorm(amount_bet)
abline(mean(ab1), sd(ab1))
abline(mean(ab2), sd(ab2), col = 2)
abline(mean(ab3), sd(ab3), col = 3)
abline(mean(ab4), sd(ab4), col = 4)
abline(mean(ab5), sd(ab5), col = 5)
abline(mean(ab6), sd(ab6), col = 6)
qqnorm(ab1)


###

par(mfrow = c(1, 4))
plot(as.factor(coin_flip))
hist(die_roll, breaks = 12)
hist(amount_bet, breaks = 10)
hist(total_lost, breaks = 10)

par(mfrow = c(1, 1))
boxplot(split(amount_bet, die_roll), xlab = "Die Roll", ylab = "Amount Bet")

par(mfrow = c(1, 2))
qqnorm(amount_bet)
abline(mean(amount_bet), sd(amount_bet))
qqnorm(total_lost)
abline(mean(total_lost), sd(total_lost))

par(mfrow = c(1,1))
plot(amount_bet, total_lost, main = "Scatterplot of Total Lost vs Amount Bet
     while Gambling (Using Dice & Coins)")
cor(amount_bet, total_lost)
call <- lm(total_lost ~ amount_bet)
alpha_hat <- call$coefficients[1]
beta_hat <- call$coefficients[2]
abline(alpha_hat, beta_hat)
resids <- total_lost - (alpha_hat+beta_hat*amount_bet)
SST <- sum((total_lost-mean(total_lost))^2)
SSE <- sum(resids^2)
SSR <- SST - SSE
r2 <- SSR/SST
Se <- sqrt(1/28*SSE)

## Week 6
abt <- amount_bet[coin_flip == 'T']
abh <- amount_bet[coin_flip == 'H']
mean(abt)
mean(abh)
sd(abt)
sd(abh)
(mean(abt) - mean(abh)) - 1.96*sqrt((8.17^2/17)+(6.04^2/13))

## Week 4
cl <- c(57.9,   35.7,   54.5,   56.8,   51.1,   70.8,   77.3, 51.6, 54.7,   63.6,   59.2,   59.2,   55.8,   38.5)
cl.mean <- mean(cl)
cl.sd <- sd(cl)
zcl <- (cl-cl.mean)/cl.sd
co <- c(44.2,   52.1,   60.2,   52.7,   47.2,   65.6,   71.4, 48.8,   53.1,   66.3,   59.8,   47.5,   64.5,   34.5)
co.mean <- mean(co)
co.sd <- sd(co)
zco <- (co-co.mean)/co.sd
comb <- zcl * zco
r <- 1/13*(sum(comb))
cor(cl, co)

a <- numeric(14)
for (i in 1:14) {
  a[i] <- zcl[i]*zco[i]
}
a
