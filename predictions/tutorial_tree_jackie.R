#tutorial on how to use deciison trees

#install.packages("rpart")

library(rpart)
library(readr)

#tutorial
#grow tree
fit <- rpart(Kyphosis ~ Age + Number + Start,
             method="class", data=kyphosis)
#display results
printcp(fit) 
#visualize cross-validation
plotcp(fit)
summary(fit)

#plot tree
plot(fit, uniform = TRUE, main ="Classification Tree for Kyphosis")
text(fit, use.n = TRUE, all = TRUE, cex= .8)

#create attractive postscrip plot of tree
post(fit, file = "c:/tree.ps",
     title = "Classification Tree for Kyphosis")

#prune the tree
pfit <- prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"])

#plot the pruned tree

plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for Kyphosis")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree.ps",
     title = "Pruned Classification Tree for Kyphosis")


install.packages("rpart.plot")
library(rpart.plot)
data("ptitanic")

#begin with a small cp
set.seed(123)
tree <- rpart(survived ~ ., data = ptitanic, 
              control = rpart.control(cp = 0.0001))

#pick tree size that minimizes misclassification error (prediction error)
#prediction error rate in training data = root node error * rel error * 100%
#prediction error rate in cv = root node error * xerror * 100%

printcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned <- prune(tree, cp = bestcp)




conf_matrix <- table(ptitanic$survived, predict(tree_pruned,type="class"))
rownames(conf_matrix) <- paste("Actual", rownames(conf_matrix), sep = ":")
colnames(conf_matrix) <- paste("Pred", colnames(conf_matrix), sep = ":")
print(conf_matrix)

#plots
plot(tree_pruned)
text(tree_pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree_pruned, faclen = 0, cex = 0.8, extra = 1)
#faclen = 0 : use full names of the factor labels
#extra = 1: adds number of observations at each node; equivalent to using
#use.n = TRUE in plot.rpart

#to plot on the tree how many people died v. survived (not a proportions)
tot_count <- function(x, labs, digits, varlen)
{
  paste(labs, "\n\nn =", x$frame$n)
}

prp(tree_pruned, faclen = 0, cex = 0.8, node.fun=tot_count)

#to incorporate color
only_count <- function(x, labs, digits, varlen)
{
  paste(x$frame$n)
}

boxcols <- c("pink", "palegreen3")[tree_pruned$frame$yval]
#to plot the tree with the colors
par(xpd=TRUE)
prp(tree_pruned, faclen = 0, cex = 0.8, node.fun=only_count, box.col = boxcols)
legend("bottomleft", legend = c("died","survived"), fill = c("pink", "palegreen3"),
       title = "Group")

###END OF TUTORIAL###



