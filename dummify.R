# Expected Input: preprocessed train.merged and test.merged
# dummify factor variables having reasonably less factors

library(caret)

source("util.R")
res <- merge(getEmptyRatio(train.merged),getNumFactors(train.merged),by="attribute")
res <- merge(res,getNumDistinct(train.merged),by="attribute")

# Picking attributes with high-Non.Empty and low NumFactors from res
ff <- ~ auctioneerID + datasource + Enclosure + Hydraulics +
 MfgYear + ProductGroup + saledate + state

# Create dummy vars
levels(test.merged$auctioneerID) <- levels(train.merged$auctioneerID)
levels(test.merged$datasource) <- levels(train.merged$datasource)
levels(test.merged$state) <- levels(train.merged$state)
levels(test.merged$Enclosure) <- levels(train.merged$Enclosure)
levels(test.merged$Hydraulics) <- levels(train.merged$Hydraulics)
levels(test.merged$ProductGroup) <- levels(train.merged$ProductGroup)

dummies <- dummyVars(ff, data=cbind(train.merged,trainClass))
train.dummy <- predict(dummies,newdata=train.merged)
test.dummy <- predict(dummies,newdata=test.merged)

pp <- preProcess(train.dummy)
train.dummy.pp <- predict(pp,train.dummy)
test.dummy.pp <- predict(pp,test.dummy)



# print(Sys.time())
# ctrl <- trainControl(method="cv")
# 
# testGrid <- expand.grid(.cp=c(1:3)*0.0001)
# 
# glmfit2 <- train(SalePrice ~ ., 
#                data=cbind(train.dummy.pp,trainClass), 
#                method="glmnet",
#                trControl = ctrl,
#               preProcess = c("center","scale")
#                #tuneGrid = testGrid
#                )
# print(Sys.time())

# Create dummy vars
# mmTrain <- model.matrix(ff,train.merged)
# mmTrain <- mmTrain[match(rownames(train.merged), rownames(mmTrain)),]
# mmTrain[,"MfgYear"] <- train.merged$MfgYear
# mmTrain[,"saledate"] <- train.merged$saledate
# 
# mmTest <- model.matrix(ff,test.merged)
# mmTest <- mmTest[match(rownames(test.merged), rownames(mmTest)),]
# mmTest[,"MfgYear"] <- test.merged$MfgYear
# mmTest[,"saledate"] <- test.merged$saledate
# 
# mmTrain <- model.matrix(ff, data=train.merged, 
#            contrasts.arg = lapply(sapply(train.merged, is.factor)
#                                   ,contrasts, 
#                                   contrasts=FALSE))

