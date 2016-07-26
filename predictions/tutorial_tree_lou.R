library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(rpart)
library(rpart.plot)

listings_history <- read_csv("../raw_data/2015-12-02-listings.csv")

# grow tree
fit <- rpart(Kyphosis ~ Age + Number + Start,
             method="class", data=kyphosis)

printcp(fit) #display results
plotcp(fit) #visualize cross-validation results
summary(fit) #detailed summary of splits

# prune tree
pfit <- prune(fit, cp=
                fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"])

# create attractive postscript plot of tree
post(fit, file = "tree.ps",
     title = "Classification Tree for Kyphosis")

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for Kyphosis")

text(pfit, use.n=TRUE, all=TRUE, cex=.8)

##############################################

# Rpubs, Titanic Data
data(ptitanic)
str(ptitanic)

# 1. Begin with a small complexity parameter (0.0001)
set.seed(123)
tree <- rpart(survived ~ ., data = ptitanic, control = rpart.control(cp = 0.0001))

# 2. Pick tree size that minimizes misclassification
printcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]

# 4. Prune the tree using the best cp
tree.pruned <- prune(tree, cp = bestcp)
printcp(tree)

# confusion matrix (training data)
conf.matrix <- table(ptitanic$survived, predict(tree.pruned, type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep=":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep=":")
print(conf.matrix)

# plotting
tot_count <- function(x, labs, digits, varlen){
  paste(labs, "\n\nn =", x$frame$n)
}

prp(tree.pruned, faclen = 0, cex = 0.8, node.fun=tot_count)

# including colour
only_count <- function(x, labs, digits, varlen)
{
  paste(x$frame$n)
}

boxcols <- c("pink", "palegreen3")[tree.pruned$frame$yval]

par(xpd=TRUE)
prp(tree.pruned, faclen = 0, cex = 0.8, node.fun=only_count, box.col = boxcols)
legend("bottomleft", legend = c("died","survived"), fill = c("pink", "palegreen3"),
       title = "Group")
