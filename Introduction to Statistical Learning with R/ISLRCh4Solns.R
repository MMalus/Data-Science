library(ISLR)
library(MASS)
library(class)
set.seed(1)

#Problem 10

attach(Weekly)

#Problem 10(a)

summary(Weekly)

#Note the results of summary, Direction is not numeric
cor(Weekly[,-9])

pairs(Weekly[,-9])

#Problem 10(b)

glm.fit = glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family=binomial)

summary(glm.fit)

#judging by the output of summary, it seems Lag2 is stat sig at p~.03

#Problem 10(c)

glm.probs = predict(glm.fit, type="response")

glm.pred = rep("Down", 1089)

glm.pred[glm.probs>.5]="Up"

table(glm.pred, Direction)

mean(glm.pred==Direction)

#While this seems good, recall we have not split into training and test sets.  But it's also important to note that
#if the market goes up, the model is right 56.1% of the time but if it goes down the model is right 11.2% of the time.

#Problem 10(d)

train=(Year>=1990&Year<=2008)

Weekly.test = Weekly[!train,]

Direction.test = Direction[!train]

glm.fit1 = glm(Direction~Lag2, family=binomial, subset=train)

glm.probs1 = predict(glm.fit1, Weekly.test, type="response")

glm.pred1 = rep("Down", 104)

glm.pred1[glm.probs1 >.5] = "Up"

table(glm.pred1, Direction.test)

mean(glm.pred1==Direction.test)

mean(glm.pred1!=Direction.test)

#Problem 10(e)

lda.fit = lda(Direction~Lag2, data=Weekly, subset=train)

lda.fit

lda.pred = predict(lda.fit, Weekly.test)

names(lda.pred)

lda.class = lda.pred$class

table(lda.class,Direction.test)

mean(lda.class==Direction.test)

#Problem 10(f)

qda.fit = qda(Direction~Lag2, data=Weekly, subset=train)

qda.fit

qda.pred = predict(qda.fit, Weekly.test)

names(qda.pred)

qda.class = qda.pred$class

table(qda.class,Direction.test)

mean(qda.class==Direction.test)

#It picks "Up" always, but still gets the right answer 58.65% of the time, cute but bad.

#Problem 10(g)

train.X = as.matrix(Lag2[train])

test.X = as.matrix(Lag2[!train])

Direction.train = Direction[train]

knn.pred = knn(train.X, test.X, Direction.train, k=1)

table(knn.pred, Direction.test)

mean(knn.pred == Direction.test)

#Problem 10(h)

#Just by the numbers, qda works best.  But note that it only ever guessed "Up".  Otherwise logreg and lda produce
#similar results

#Problem 10(i)

#Reasonable choices could be squared variables or square roots of absolute values.  But this isn't a very interesting
#topic

#knn with a few different k

kvec = c(1, 5, 10, 25, 100)

for (kval in kvec){
  knn.pred = knn(train.X, test.X, Direction.train, k=kval)
  print(mean(knn.pred == Direction.test))
}

#Problem 12

#Problem 12(a)
#Note I created a general function here, which is technically part d

Power <- function(x, a) {
  return(x^a)
}

print(Power(2,3))

#Problem 12(b)

print(Power(3,8))

#Problem 12(c)

print(Power(10,3))

print(Power(8,17))

print(Power(131,3))

#Problem 12(d)

#See part (a)'s answer

#Problem 12(e)

x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

plot(x, Power(x, 2), xlab="Numbers from 1 to 10 (log)", ylab="Squared numbers from 1 to 10 (log)", main="Demonstrating Power function", log="xy")

#Problem 12(f)

PlotPower <- function(x, a, ...){
  plot(x, Power(x, a), xlab="Integers (log)", ylab=paste0("Integers to the power ", a, " (log)"), main="Demonstrating Power plotting function", log="xy", ...)
}

PlotPower(1:10, 3)
