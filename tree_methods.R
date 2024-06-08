# Reference: ISLR Chapter 8

library(ISLR2)
library(tree) # ordinary regression / classification tree
library(randomForest) # random forest
library(gbm) # generalised boosting models
library(BART) # Bayesian additive regression tree

### 1. Ordinary regression / classification tree
tree = tree(y ~ x, data=df)
summary(tree) # type of tree, variables used, number of terminal nodes, residuals

plot(tree)
text(tree, pretty=0) # plot tree with labels for visualisation

pred = predict(tree, df.test.X, type="class")
table(tree.pred, df.test.Y)
# pred = predict(tree, df.test.X) # for regression or probability


tree.cv = cv.tree(tree, FUN=prune.misclass)
names(tree.cv)

par(mfrow=c(1, 2))
plot(tree.cv$size, tree.cv$dev, type="b") # plot of deviance vs tree size to select optimal tree size
plot(tree.cv$k, tree.cv$dev, type="b") # plot of deviance vs regularisation parameter (k*|T|) - bijection with tree size

par(mfrow=c(1, 1))
prune.tree = prune.misclass(tree, best=9) # best = optimal tree size
plot(prune.tree)
text(prune.tree, pretty=0)

tree.pred.test = predict(prune.tree, newdata=df.test.X, type="class") # predict with cross-validated tree
# tree.pred.test = predict(prune.tree, newdata=df.test.X) # regression tree
table(tree.pred.test, df.test.Y)
# mean((tree.pred.test-df.test.Y)^2) # regression tree
# plot(tree.pred.test, df.test.Y) # see if close to y =x
# abline(0, 1) # y = x

train = sample(1:nrow(df), 100)
tree = tree(y ~ ., data=df, 
            subset=train, 
            control=tree.control(nobs=length(train), # nobs = number of observations in training set
                                 mincut=4, # nobs = number of observations in training set
                                 minsize=10, # smallest allowed node size
                                 mindev=0 # deviance ratio threshold for node compared to the root to split, 0 means it will always split
                                ) 
           )


### 2. Random Forest
rf = randomForest(y ~ ., 
                  data=df, 
                  subset=train, 
                  mtry=12, # number of variables considered at each split (=p if bagging)
                  importance=T # calculate importance of each covariate
                 )

yhat.rf = predict(rf, newdata=df[-train,])
plot(yhat.rf, y.test)
abline(0, 1)
mean((yhat.rf - y.test)^2)

varImpPlot(rf.boston) # plot variable importance

### 3. Boosting

boost = gbm(y ~ ., data=df[train, ],
            distribution="gaussian", # "gaussian", "bernoulli", "poisson"
            n.trees=5000, # number of trees in the boosting
            interaction.depth=4, # max depth of the tree
            shrinkage=0.2, # how much each boosted tree's prediction is shrink by in the additive model
            verbose=F)

summary(boost)
plot(boost, i="x1") # partial dependence plot - partial effect of covariate x1 on response y

yhat.boost = predict(boost,
                     newdata=df.test.X, n.trees=5000) # can specify when to early cutoff
mean((yhat.boost - df.test.Y)^2)

### 4. Bayesian Additive Regression Tree

bartfit = gbart(xtrain, ytrain, x.test=xtest)
yhat.bart = bartfit$yhat.test.mean
mean((ytest - yhat.bart)^2)

ord = order(bartfit$varcount.mean, decreasing=T) # how many times each variable appeared in the collection of trees i.e. a proxy for importance of variables
bartfit$varcount.mean[ord]




