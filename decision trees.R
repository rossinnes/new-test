# classification tree model

install.packages("ISLR")
library(ISLR)
attach(Carseats)
head(Carseats)
range(Sales)

# create a categorial vector 

High = ifelse(Sales >=8,"Yes","No")
length(High)
dim(Carseats)

# lets attatch this new vector to the data frame, basicly overides the current data frame 

Carseats = data.frame(Carseats,High)
names(Carseats)

# split data into training and test sets

Carseats = Carseats[,-1]

set.seed(2) # if you want to reproduce the same results later on (you can use any number)
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
test = -train
training_data = Carseats[train,]
testing_data = Carseats[test,]
testing_High = High[test]

#fit tree model using train data

install.packages("tree")
library(tree)
?tree

tree_model = tree(High ~., training_data)

# then plot (adding text to the dendrogram!)
# note pretty=0 is important for categorical variables otherwise you get a, b and c
plot(tree_model)
text(tree_model, pretty=0, )

# check how the model is doing using the test data


tree_pred = predict(tree_model, testing_data,type ="class")
mean(tree_pred != testing_High) # this should give 0.285 which is 28.5% a little high so we need to prune and for this use cross validation!

# prune the tree and use cross val to see where we should stop pruning

set.seed(3)
cv_tree = cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

# plot the size vs the error rate

plot(cv_tree$size,cv_tree$dev, type="b")

# the cross validation shows us that we should prune the tree at 9

# prune

pruned_model = prune.misclass(tree_model, best=9)
plot(pruned_model)
text(pruned_model, pretty=0)

#check how its doing

tree_pred = predict(pruned_model, testing_data, type ="class")
mean(tree_pred != testing_High)
