library(MASS)
library(ISLR)

#Problem 13
set.seed(1)

#Problem 13(a)
x = rnorm(100)

#Problem 13(b)
eps = rnorm(100, 0, sqrt(0.25))

#Problem 13(c)
y = -1 + 0.5*x + eps

#y is obviously length 100
#beta_0 and beta_1 are read off as -1 and .5 respectively
print(length(y))

#Problem 13(d)
plot(x, y)

#Problem 13(e)
lm.fit = lm(y~x)

#Use summary to compare to "natural" beta_0 and beta_1
#Note that we have a decent fit from the estimates
summary(lm.fit)

#Problem 13(f)
plot(x, y)
abline(lm.fit, col=2)
abline(-1, 0.5, col=3)
legend(-1, legend=c("model fit", "pop. regression"), col=2:3)

#Problem 13(g)
lm.fitsq = lm(y~x+I(x^2))

#As shown below, there is evidence that adding x^2 did
#very slightly improve the fit in r^2, but p and F look worse
summary(lm.fitsq)

#Problem 13(h)
set.seed(1)
x1 = rnorm(100)
eps1 = rnorm(100, 0, .25)
y1 = -1 + 0.5*x1 + eps1
plot(x1, y1)
lm.fit1 = lm(y1~x1)

#We can see much better statistical fit data so far
summary(lm.fit1)

#and the drawn lines look much better too!
plot(x1, y1)
abline(lm.fit1, col=2)
abline(-1, 0.5, col=3)
legend(-1, legend=c("model fit", "pop. regression"), col=2:3)

#Problem 13(i)
set.seed(1)
x2 = rnorm(100)
eps2 = rnorm(100, 0, .75)
y2 = -1 + 0.5*x2 + eps2
plot(x2, y2)
lm.fit2 = lm(y2~x2)

#We can see a somewhat worse statistical fit data so far
summary(lm.fit2)

#Drawn lines 
plot(x2, y2)
abline(lm.fit2, col=2)
abline(-1, 0.5, col=3)
legend(-1, legend=c("model fit", "pop. regression"), col=2:3)

#problem 13(j)

#Inspection of the below reveals (not surprisingly) that
#less noise gives us a much tighter bound around the 
#"true" intercepts and slope

#Original set
confint(lm.fit)
#Less noise
confint(lm.fit1)
#More noise
confint(lm.fit2)

#Problem 14

#Problem 14(a)
set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
#beta_0 = 2, beta_1 = 2, beta_2 = 0.3
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)

#Problem 14(b)
plot(x1, x2)
cor(x1, x2)

#Problem 14(c)
lm.fit = lm(y~x1+x2)

#The estimated: beta_0 = 2.13, beta_1 = 1.44, beta_2 = 1.01
#But note that the p value for x2 is HUGE, so we cannot
#reject its null hypothesis
summary(lm.fit)

#Problem 14(d)
lm.fit1 = lm(y~x1)

#We can reject the null based on the p value
summary(lm.fit1)

#Problem 14(e)
lm.fit2 = lm(y~x2)

#We can reject the null based on the p-value
summary(lm.fit2)

#14(f)
#No, they do not.  x1 and x2 fall under the first of the
#"detection of multicollinearity" section of the wikipedia
#article on the subject

#14(g)
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y,6)

lm.fit = lm(y~x1+x2)
lm.fit1 = lm(y~x1)
lm.fit2 = lm(y~x2)

#Note that x1 and x2 are now statistically insignificant
#and significant respectively now
summary(lm.fit)
