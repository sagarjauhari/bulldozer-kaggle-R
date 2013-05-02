library(mobForest)

trainDescr <- tm.psb1

v <- (trainDescr$saledate >= as.numeric(strptime(2011, format="%Y"))) & 
  (trainDescr$saledate < as.numeric(strptime(2012, format="%Y")))
train <- trainDescr[v,]
train <- train[1:1000,]

v <- (trainDescr$saledate >= as.numeric(strptime(2012, format="%Y")))
test <- trainDescr[v,]

mob1 <- mobForestAnalysis(as.formula(SalePrice ~ datasource +
                                       saledate +
                                       ProductSize),
                          c("Enclosure", "Ride_Control",
                            "Stick", "Transmission",
                            "Hydraulics"),
                          mobForest.controls = 
                            mobForest_control(ntree = 5,
                                              mtry = 2,
                                              replace = T),
                          data=train,
                          processors=1,
                          model=linearModel,
                          newTestData = test)