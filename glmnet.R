# Expects train.dummy.pp and test.dummy.pp

library(glmnet)


glmfit <- glmnet(train.dummy.pp,
                 trainClass[,1],
                 lambda.min.ratio=0.0001,
                 nlambda=10
                 )
glm.pred.train <- predict(glmfit, train.dummy.pp, s=0.1245375)
glm.pred.test <- predict(glmfit, test.dummy.pp,s=0.1245375)

rmse <- NULL
for(i in 1:100){
  rmse[i] <- (mean((trainClass[,1] - glm.pred.train[,i])^2))^0.5
}
plot(rmse)