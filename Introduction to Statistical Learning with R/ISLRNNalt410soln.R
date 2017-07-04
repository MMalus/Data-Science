#Michael Malus

#Alternate solution to ISLR Chaoter 4 Problem 10(b) using the NN library in R

library('neuralnet')
library('caTools')
library('ISLR')
set.seed(1)

attach(Weekly)

#Part a'
summary(Weekly)
str(Weekly)
dim(Weekly)
cor(Weekly[,-9])
pairs(Weekly[,-9])

#Part b'

#We will use Direction as the response and all other variables as predictors
#First we need to normalize the data, start by creating min and max vectors

maxs <- apply(Weekly[,1:8], 2, max)
mins <- apply(Weekly[,1:8], 2, min)

scaled <- as.data.frame(scale(Weekly[,1:8], center = mins, scale = maxs - mins))

#Next we check the results to make sure this looks reasonable
print(head(scaled,2))

#It looks fine, so we can move ahead to changing the Direction column to 1/0
Direction = as.numeric(Weekly$Direction)-1
data = cbind(scaled, Direction)

head(data)

#Now let's set up the training and test data
split = sample.split(Weekly$Direction, SplitRatio = 0.70)

#Split based off of split Boolean Vector
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

#Finally, let's set up the formula we're going to use in our NN
feats <- names(scaled)

#Concatenate strings
f <- paste(feats, collapse=' + ')
f <- paste('Direction ~', f)

#Convert to formula
f <- as.formula(f)

#Finally, define the NN
nn <- neuralnet(f, train, hidden=c(10,10,10), linear.output=FALSE)

#Compute predictions off the test set
predicted.nn.values <- compute(nn, test[,1:8])

#Check out net.result
print(head(predicted.nn.values$net.result))

#The results look more like probabilities, so we should just round them off
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result, round, digits=0)

#Create the confusion matrix
table(test$Direction, predicted.nn.values$net.result)

#So as you can see, even with this incredibly simplistic NN, we have achieved a
#significant improvement in our predictions.  Let's see the architecture of the
#network, just for illustration of complexity.

plot(nn)
