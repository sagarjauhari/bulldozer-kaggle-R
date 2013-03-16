library(randomForest)

load("train.merged.Rdata")
load("test.merged.Rdata")
load("trainClass.Rdata")

rf.fit <- randomForest( SalePrice ~ . - MachineID - SalesID,                      
                        data=cbind(train.merged,trainClass),
                        importance=T,
                        na.action=na.omit)