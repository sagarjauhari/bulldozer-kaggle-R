library(caret)
source(filter.R)

trainDescr <- tm.psb1
trainClass <- trainDescr$SalePrice
trainDescr$SalePrice <- NULL

v <- trainDescr$saledate >= as.numeric(strptime(2011, format="%Y"))
xtest <- trainDescr[v,]
ytest <- trainClass[v]

xtrain <- trainDescr[-v,]
ytrain <- trainClass[-v]

#xtest <-   xtest[,c(3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,25,27,28)]
#xtrain <- xtrain[,c(3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,25,27,28)]

xtest <-   xtest[,c(3,4,5,6,7,9,10)]
xtrain <- xtrain[,c(3,4,5,6,7,9,10)]


rf.fit.psb1.noprox <- randomForest( x=xtrain,
                                      y=ytrain,
                                      xtest=xtest,
                                      ytest=ytest,
                                      proximity=T,
                                      do.trace=T,
                                      #sampsize=100000,
                                      importance=T,
                                      keep.forest=T
                                      
)