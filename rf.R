library(randomForest)

load("train.merged.rf.Rdata")
load("test.merged.rf.Rdata")
load("trainClass.rf.Rdata")
rfbench <- read.csv("../../kaggle/bulldozers/random_forest_benchmark.csv")


v <- train.merged$saledate >= 2011
xtest <- train.merged[v,]
ytest <- trainClass[v]

v <- is.na(test.merged$MfgYear)
meanAge <- mean(test.merged$saledate[!v] - test.merged$MfgYear[!v])
test.merged$MfgYear[v] <- test.merged$saledate[v] - meanAge

rf.fit.100000.noprox <- randomForest( x=train.merged,
                        y=trainClass,
                        na.action=na.omit,
                        xtest=test.merged,
                        ytest=rfbench$SalePrice,
                        proximity=T,
                        do.trace=T,
                        sampsize=100000,
                        importance=T,
                        keep.forest=T
                        
)
rf.fit.10000.noprox <- rf.fit




