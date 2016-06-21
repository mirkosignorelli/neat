# load libraries:
library(neat)
library(pROC)

# load data:
load('data-sim1.Rdata')
# NB: adj is the adjacency matrix, nodes a vector with node names,
# setlist is a list specifying the sets of nodes
# and correct is a vector indicating the ground truth (0 = enrichment, 1 = no enrichment)

# generate network matrix
net = networkmatrix(adj, nodes, nettype = 'directed')

# compute the test
test = neat(alist = setlist, blist = setlist, network = net,
            nodes = nodes, nettype = 'directed', alpha = 0.05)
test = test[test$A!=test$B,]

# behaviour under H0:
summary(test[correct==1,])
# overall behaviour
table(correct, test$conclusion)

plot.roc(correct, test$pvalue,axes=T, percent=TRUE, print.auc=TRUE,
         max.auc.polygon=TRUE, max.auc.polygon.col="white")  
