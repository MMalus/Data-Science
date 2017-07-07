library(ISLR)
library(leaps)
library(glmnet)
library(caTools)
library(pls)
set.seed(1)

#Problem 8

#Problem 8(a)
X <- rnorm(n=100)
eps <- rnorm(n=100)

#Problem 8(b)
beta0 = rnorm(1)
beta1 = rnorm(1)
beta2 = rnorm(1)
beta3 = rnorm(1)

Y <- beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + eps

#Problem 8(c)
data.full = data.frame(y=Y, x=X)
regfit.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10)
regfit.summary = summary(regfit.full)

#Now to get the best model according to Cp, BIC, and adjR^2
which.min(regfit.summary$cp)
which.min(regfit.summary$bic)
which.max(regfit.summary$adjr2)

#Looks like there's some disagreement among the standards, let's generate plots
#to take a closer look

plot(regfit.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(4, regfit.summary$cp[4], pch = 4, col = "red", lwd = 7)

plot(regfit.summary$bic, xlab = "Subset Size", ylab = "BIC", pch = 20, type = "l")
points(3, regfit.summary$bic[3], pch = 4, col = "red", lwd = 7)

plot(regfit.summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, type = "l")
points(4, regfit.summary$adjr2[4], pch = 4, col = "red", lwd = 7)

#At the end of the day, it's not a huge disagreement (between 3 and 4), but let's check
#the summary coefficients just for more insight

coefficients(regfit.full, id = 3)
coefficients(regfit.full, id = 4)

#So the question is between including x^5 or not

#Problem 8(d)
#Basically now we use regsubsets with "forward" or "backward" set as the method
regfit.fwd = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, method = "forward", nvmax = 10)
summary.fwd = summary(regfit.fwd)
regfit.bwd = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, method = "backward", nvmax = 10)
summary.bwd = summary(regfit.bwd)

#Now for the Cp, BIC, and adjR^2
which.min(summary.fwd$cp)
which.min(summary.fwd$bic)
which.max(summary.fwd$adjr2)
which.min(summary.bwd$cp)
which.min(summary.bwd$bic)
which.max(summary.bwd$adjr2)

#Note that we are now seeing some not insignificant disagreements in the numbers
#Plots for a closer look

par(mfrow = c(3, 2))
plot(summary.fwd$cp, xlab = "Subset Size", ylab = "Forward Cp", pch = 20, type = "l")
points(4, summary.fwd$cp[4], pch = 4, col = "red", lwd = 7)
plot(summary.bwd$cp, xlab = "Subset Size", ylab = "Backward Cp", pch = 20, type = "l")
points(7, summary.bwd$cp[7], pch = 4, col = "red", lwd = 7)
plot(summary.fwd$bic, xlab = "Subset Size", ylab = "Forward BIC", pch = 20, type = "l")
points(4, summary.fwd$bic[4], pch = 4, col = "red", lwd = 7)
plot(summary.bwd$bic, xlab = "Subset Size", ylab = "Backward BIC", pch = 20, type = "l")
points(5, summary.bwd$bic[5], pch = 4, col = "red", lwd = 7)
plot(summary.fwd$adjr2, xlab = "Subset Size", ylab = "Forward Adjusted R2", pch = 20, type = "l")
points(4, summary.fwd$adjr2[4], pch = 4, col = "red", lwd = 7)
plot(summary.bwd$adjr2, xlab = "Subset Size", ylab = "Backward Adjusted R2", pch = 20, type = "l")
points(7, summary.bwd$adjr2[7], pch = 4, col = "red", lwd = 7)

#The forward stepwise selection agrees pretty well, while backwards seems to call for
#more terms

#Problem 8 (e)
xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data.full)[, -1]
#Note that we are using cross-validation!
lasso.mod = cv.glmnet(xmat, Y, alpha = 1)
#Apparently the best min is .002979574
best.lambda = min(lasso.mod$lambda)
plot(lasso.mod)

#Now we redo this, using the best lambda we found above:
lasso.best = glmnet(xmat, Y, alpha=1)
predict(lasso.best, s = best.lambda, type = "coefficients")

#So this method picks X, X^2, X^6, and X^7 as the most important with X^3 conttributing

#Problem 8 (f)
beta7 <- rnorm(1)
Y = beta0 + beta7 * X^7 + eps
#Start with the regsubsets bit:
data.full = data.frame(y=Y, x=X)
regfit.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10)
regfit.summary = summary(regfit.full)

#Now check the Cp, BIC, and adjR^2 again
which.min(regfit.summary$cp)
which.min(regfit.summary$bic)
which.max(regfit.summary$adjr2)

#Find which coefficients are predicted based on the above criteria:
coefficients(regfit.full, id=1)
coefficients(regfit.full, id=2)
coefficients(regfit.full, id=4)

#So it seems like BIC picks the most realistic (correct) model with near matching coefficients

#Now for the lasso
xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data.full)[, -1]
lasso.mod = cv.glmnet(xmat, Y, alpha = 1)
#Apparently the best min is 1.021374
best.lambda = min(lasso.mod$lambda)
plot(lasso.mod)

#Now we redo this, using the best lambda we found above:
lasso.best = glmnet(xmat, Y, alpha=1)
predict(lasso.best, s = best.lambda, type = "coefficients")

#Examining these results, we see that Lasso adds a (very) small X^9 parameter, but that it's beta0 and beta7 are relatively good

#Problem 9

attach(College)

#Problem 9(a)
#We'll use caTools' here with a training size of 70%
split = sample.split(College, SplitRatio = 0.70)

#Split based off of split Boolean Vector
train = subset(College, split == TRUE)
test = subset(College, split == FALSE)

#Problem 9(b)
#We are interested in predicting the number of applications received
lm.fit = lm(Apps~., data=train)
lm.pred = predict(lm.fit, test)

#Recall the test error for least squares is mean((y-pred)^2)
mean((test[,"Apps"]-lm.pred)^2)
#We find a test error of 1246994

#Problem 9(c)
xmattrain = model.matrix(Apps~., data=train)
xmattest = model.matrix(Apps~., data=test)
grid = 10^seq(10, -2, length=100)
ridge.fit = cv.glmnet(xmattrain, train[,"Apps"], alpha = 0, lambda = grid)
#We see we have a best fit lambda of 32.74549
lambda.best = ridge.fit$lambda.min
ridge.pred = predict(ridge.fit, newx = xmattest, s=lambda.best)
mean((test[, "Apps"]-ridge.pred)^2)
#The new error is slightly higher now at 1427451

#Problem 9(d)
lasso.mod = cv.glmnet(xmattrain, train[, "Apps"], alpha = 1)
#We see the best lambda as 3.912414
best.lambda = min(lasso.mod$lambda)
#Now we redo this, using the best lambda we found above:
lasso.pred = predict(lasso.mod, newx = xmattest, s = best.lambda)
mean((test[, "Apps"]-lasso.pred)^2)
#We see a closer error (but still higher) at 1267010

#Now we examine the coefficients as follows, it looks like nothing was zeroed out
lasso.mod = glmnet(model.matrix(Apps~., data=College), College[, "Apps"], alpha=1)
lasso.pred = predict(lasso.mod, s = best.lambda, type="coefficients")

#Problem 9(e)
#Remember to remove missing values before attempting this part of the exercise
pcr.fit = pcr(Apps~., data=train, scale = TRUE, validation="CV")
#Use validationplot() to find M (looks like 10 on my plot)
validationplot(pcr.fit, val.type="MSEP")
pcr.pred = predict(pcr.fit, test, ncomp=10)
mean((test[, "Apps"]-pcr.pred)^2)
#We see an error of 3288706

#Problem 9(f)
pls.fit = plsr(Apps~., data = train, scale = TRUE,validation="CV")
validationplot(pls.fit, val.type="MSEP")
#Again, looks like it bottoms out around 10 or so
pls.pred = predict(pls.fit, test, ncomp=10)
mean((test[, "Apps"]-pls.pred)^2)
#We see an error of 1236705, best so far!

#Problem 9(g)
#It looks like, on the face of it, PLS is the strongest result.  It seems like, in general,
# we are able to predict applications fairly well.  Lasso shrunk the importance
#of many variables, but did not outright delete any of them.  The test error for
#pcr was massive however, and we should probably avoid using it here.

#Probem 10

#Problem 10(a)
p = 20
n = 1000
beta = rnorm(p)
epsilon = rnorm(p)
X = matrix(rnorm(n * p), n, p)
#Let's pick five elements to set to zero: 16, 12, 1, 2, 3 (done by drawing random numbers from 1 to 20)
beta[16] = 0
beta[12] = 0
beta[1] = 0
beta[2] = 0
beta[3] = 0
#Matrix multiplication for R is %*% 
y = X %*% beta + epsilon

#Problem 10(b)
#caTools (to my knowledge) won't work well on matrices, so we do this the old fashioned way
train = sample(seq(1000), 100, replace = FALSE)
X.train = X[train, ]
X.test = X[-train, ]
y.train = y[train,]
y.test = y[-train,]

#Problem 10(c)
regfit.full = regsubsets(y~., data = data.frame(x= X.train, y=y.train), nvmax=p)
summary(regfit.full)
#we create essentially an empty vector p units long. We'll fill this with a for loop
val.errors = rep(NA, p)
#generate same fake "names" for our columns
x_cols = colnames(X, do.NULL = FALSE, prefix = "x.")
for (i in 1:p) {
  coefficients = coef(regfit.full, id = i)
  pred = as.matrix(X.train[, x_cols %in% names(coefficients)]) %*% coefficients[names(coefficients) %in% x_cols]
  val.errors[i] = mean((y.train - pred)^2)
}
#Finally, plot the errors
plot(val.errors, ylab = "Training MSE", pch = 19, type = "b")

#Problem 10(d)
#Copy pasted from 10(c) essentially
#we create essentially an empty vector p units long. We'll fill this with a for loop
val.errors = rep(NA, p)
#generate same fake "names" for our columns
x_cols = colnames(X, do.NULL = FALSE, prefix = "x.")
for (i in 1:p) {
  coefficients = coef(regfit.full, id = i)
  pred = as.matrix(X.test[, x_cols %in% names(coefficients)]) %*% coefficients[names(coefficients) %in% x_cols]
  val.errors[i] = mean((y.test - pred)^2)
}
#Finally, plot the errors
plot(val.errors, ylab = "Test MSE", pch = 19, type = "b")

#Problem 10(e)
#It looks like we want 12 parameter from the graph, but just to check:
which.min(val.errors)
#So 12 parameter is correct!  Since five of the parameters were zero, it makes
#some sense that the number of parameters we want is less than 20.

#Problem 10(f)
#We'd like to see how many of the actually zero coefficients the model caught
#Recall that we zeroed out 1, 2, 3, 12, and 16 by force at the start.
coef(regfit.full, id = 12)
#We actually caught all the forcefully zeroed out coefficients, but it seems
#that we picked up another one at 7.  Can't win them all it seems!

#Problem 10(g)
#again, we mostly copy and paste the code from  part 10(c)
val.errors = rep(NA, p)
a = rep(NA, p)
b = rep(NA, p)
for (i in 1:p) {
  coefficients = coef(regfit.full, id = i)
  a[i] = length(coefficients) - 1
  b[i] = sqrt(sum((beta[x_cols %in% names(coefficients)] - coefficients[names(coefficients) %in% x_cols])^2) + sum(beta[!(x_cols %in% names(coefficients))])^2)
}
#Finally, plot the errors
plot(x = a, y = b, xlab = "number of coefficients", ylab = "error between estimated and true coefficients")
#We find a discrepancy of 1 between our result here and our result in 10(d)
which.min(b)
