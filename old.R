library(rpart)
library(caret)
library(party)

#### Multicore
# library(doMC)
# registerDoMC(cores = 2)

######## Debug variables ###########
do.import = T
do.preprocess = T
do.nom2num = F
do.transformAttr = F
do.visualize = F
do.analysis = F
do.predict = F
do.clean = F
do.saveResults = F


######## Functions ###########
# The lookup function is used to map factors to numbers.
# It creates the mapping using the distinct values across
# the training and test sets, thereby providing a consistent
# mapping everytime. 
lookup <- function(var, target){
  distinct = unique(c(as.character(train.merged[[var]]),
                      as.character((test.merged[[var]]))))
  df <- data.frame(x = distinct, y = c(1:length(distinct)))
  merge(target[[var]], df, all.x=T, by.y='x')[,2]
}

buildModl1 <- function(){
  print("Building modl1", quote = F)
  print(Sys.time())
  ctrl <- trainControl(method="repeatedcv")
  
  testGrid <- expand.grid(.cp=c(1:3)*0.0001)
  
  modl1 <- train(SalePrice ~ . - MachineID - SalesID, 
                 data=train.merged, 
                 method="rpart",
                 trControl = ctrl,
                 tuneGrid = testGrid)
  print(Sys.time())
  #   print("Saving modl1",quote=F)
  #   save(modl1, file="modl1.model")
}

############  Modl2 - Linear modeling ######
# RMSE = 22300
buildModl2 <- function(){
  print("Building modl2", quote = F)
  print(Sys.time())
  ctrl <- trainControl(method="cv")  
  modl2 <- train(SalePrice ~ . - MachineID - SalesID, 
                 data=train.merged, 
                 method="lm",
                 trControl = ctrl,
                 preProc=c("center","scale")
                 #tuneGrid = testGrid
  )
  print(Sys.time())
  return(modl2)
}


############  Modl3 - Linear modeling ######
# RMSE = 22700
buildModl3 <- function(){
  print("Building modl3", quote = F)
  print(Sys.time())
  
  ctrl <- trainControl(method="cv")
  
  testGrid <- expand.grid(.cp=c(1:3)*0.0001)
  
  modl3 <- train(SalePrice ~ fiBaseModel
                 + ProductGroup
                 + Forks
                 + fiSecondaryDesc
                 + Coupler
                 + state
                 + Coupler, 
                 data=train.merged, 
                 method="lm",
                 trControl = ctrl,
                 preProc=c("center","scale")
                 #tuneGrid = testGrid
  )
  print(Sys.time())
  #   print("Saving modl1",quote=F)
  #   save(modl1, file="modl1.model")
  return(modl3)
}


############  Importing data ######
if(do.import){
  source("import.R")  
}

###========================#
###   Preprocessing        #
###========================#

if(do.preprocess){
  print("Preprocessing", quote = F)
  ### Remove unnecessary columns
  machine.appendix$fiProductClassDesc <- NULL # might do text processing later on
  machine.appendix$fiManufacturerDesc <- NULL # since fiManufacturerID is present.
  machine.appendix$fiModelDesc <- NULL            #Redundant
  machine.appendix$ProductGroupDesc <- NULL
  
  trainDescr$fiModelDescriptor <- NULL      #Redundant
  trainDescr$fiModelSeries <- NULL          #Redundant
  trainDescr$fiProductClassDesc <- NULL     #Redundant
  trainDescr$fiSecondaryDesc <- NULL        #Redundant
  trainDescr$ModelID <- NULL                #Redundant
  trainDescr$fiModelDesc <- NULL            #Redundant
  trainDescr$ProductGroup <- NULL           #Redundant
  trainDescr$ProductGroupDesc <- NULL       #Redundant
  trainDescr$fiBaseModel <- NULL            #Redundant
  
  testDescr$fiModelDescriptor <- NULL      #Redundant
  testDescr$fiModelSeries <- NULL          #Redundant
  testDescr$fiProductClassDesc <- NULL     #Redundant
  testDescr$fiSecondaryDesc <- NULL        #Redundant
  testDescr$ModelID <- NULL                #Redundant
  testDescr$fiModelDesc <- NULL            #Redundant
  testDescr$ProductGroup <- NULL           #Redundant
  testDescr$ProductGroupDesc <- NULL       #Redundant 
  testDescr$fiBaseModel <- NULL            #Redundant
  
  ### Handle MfgYear
  trainDescr$YearMade <- NULL
  testDescr$YearMade <- NULL
  
  ### STEP 1 - Remove all attribues which have < 20 % non-empty values. 
  emptyRatio = data.frame(attribute=c(),"Non-Empty"=c())
  threshold = 0.2
  
  ### Find nearly empty columns of machine.appendix
  count = 0;
  for(colname in names(machine.appendix)){
    nonEmptyRatio = prop.table(
      table(machine.appendix[[colname]]=="" | is.na(machine.appendix[[colname]])))[1]
    #     emptyRatio = rbind(emptyRatio,data.frame(attribute=colname, "Non-Empty" = attrEmpty))
    if(nonEmptyRatio < threshold){
      machine.appendix[[colname]] <- NULL
      count = count + 1;
    }
  }
  print(sprintf("Removed %d attributes from machine.appendix",count))
  
  
  ### Remove nearly empty columns of trainDescr; Alro remove the same columns from testDescr
  count = 0
  for(colname in names(trainDescr)){
    nonEmptyRatio = prop.table(table(trainDescr[[colname]]=="" |
                                       is.na(trainDescr[[colname]])))[1]
    #   emptyRatio = rbind(emptyRatio,data.frame(attribute=colname, "Non-Empty" = attrEmpty))
    if(nonEmptyRatio < threshold){
      trainDescr[[colname]] <- NULL
      testDescr[[colname]] <- NULL
      count = count+1
    }
  }
  print(sprintf("Removed %d attributes from trainDescr and testDescr",count), quote = F)
  
  ### STEP 2 - Merge trainDescr with machine.appendix, testDescr with machine.appendix
  train.merged <- merge(trainDescr, machine.appendix, all.x=T)
  test.merged <- merge(testDescr, machine.appendix, all.x=T)
  rm(machine.appendix)
  
  ### STEP 3 - Handle MfgYear
  train.merged$MfgYear[train.merged$MfgYear==1000] = NA
  test.merged$MfgYear[test.merged$MfgYear==1000] = NA
  train.merged$MfgYear[train.merged$MfgYear==9999] = NA
  test.merged$MfgYear[test.merged$MfgYear==9999] = NA
  
  # Replace 'None or Unspecified' by NAs
  train.merged[train.merged=="None or Unspecified"] <- NA;
  test.merged[test.merged=="None or Unspecified"] <- NA;
  
  # Parse $saledate
  train.merged$saledate <- as.numeric(
    strptime(train.merged$saledate, format="%m/%d/%Y %H:%M"))
  test.merged$saledate <- as.numeric(
    strptime(test.merged$saledate, format="%m/%d/%Y %H:%M"))
  
  
  
  # Remove Near Zero Variance predictors
  nrzvar <- nearZeroVar(train.merged)
  train.merged <- train.merged[,!names(train.merged) %in% names(train.merged[nrzvar])]
  test.merged <- test.merged[,!names(test.merged) %in% names(test.merged[nrzvar])]
  
  # Remove high correlation predictors
  #highcorr <- findCorrelation(cor(train.merged), 0.90)
  
} # End if (do.preprocess)

# STEP 4 - Create lookup functions and convert to numeric

if(do.nom2num){
  print("Converting nominal attributes to numeric",quote=F)
  # vars will also be used later, because these are the
  # attributes that look like numbers but have to be treated
  # as nominal. 
  vars <- c(
    "PrimarySizeBasis",
    "ProductSize",
    "fiProductClassDesc",
    "state",
    "ProductGroup",
    "Drive_System",
    "Enclosure",
    "Forks",
    "Ride_Control",
    "Transmission",
    "Hydraulics",
    "Ripper",
    "Tire_Size",
    "Coupler",
    "Track_Type",
    "Undercarriage_Pad_Width",
    "Stick_Length",
    "Thumb",
    "Pattern_Changer",
    "Grouser_Type",
    "fiBaseModel",
    "fiSecondaryDesc"
  )  
  for(var in vars){
    if(is.factor(train.merged[[var]]) & is.factor(test.merged[[var]])){
      train.merged[[var]]<-lookup(var,train.merged)
      test.merged[[var]]<-lookup(var,test.merged)
    }
  }
}

########## Transform Attributes ################
if(do.transformAttr){
  # Centering and Scaling
  print("Transforming attributes: Centering and Scaling",quote=F)
  transVars <- c(
    "saledate",
    "ProductSize",
    "state",
    "Drive_System",
    "Enclosure",
    "Forks",
    "Ride_Control",
    "Transmission",
    "Hydraulics",
    "Coupler",
    "Track_Type",
    "Grouser_Type",
    "MfgYear"
  )
  xTrans <- preProcess(train.merged[,transVars])
  train.processed <- predict(xTrans,train.merged[,transVars])
  train.merged[,transVars] <- train.processed
  test.processed <- predict(xTrans,test.merged[,transVars])
  test.merged[,transVars] <- test.processed
  
  # Save files: ID = S001
  write.csv(train.merged,file="train_S001.csv",row.names=F)
  write.csv(test.merged,file="test_S001.csv",row.names=F)
}

##### Clean up
if(do.clean){
  print("Cleaning up!", quote = F)
  rm(trainDescr)
  rm(testDescr)
  rm(machine.appendix)
  rm(train.processed)
  rm(test.processed)
  rm(emptyRatio)
  rm(test.can.substitute)
  rm(train.can.substitute)
}

########## Analysis ################
if(do.analysis){
  # Add SalePrice to the table
  train.merged <- cbind(train.merged,trainClass);
  colnames(train.merged)[26] <- "SalePrice";
  
  print("Starting data analysis", quote = F)
  
  #   print("Calculating life of machines", quote = F)
  #   life <- train.merged$saledate - as.numeric(strptime(train.merged$YearMade,format="%Y"))
  #   life[train.merged$saledate < as.numeric(strptime(1918,format="%Y"))
  #        | train.merged$saledate > as.numeric(strptime(2013,format="%Y")) ] <- NA
  #   life[train.merged$YearMade < 1919 |
  #          train.merged$YearMade > 2013] <- NA
  #   life[life < 0] <- NA
  
  
  #   print("Building modl0", quote = F)
  #   print(Sys.time())
  #   modl0 <- rpart(SalePrice ~ 
  #           auctioneerID + 
  #           YearMade + 
  #           saledate + 
  #           fiBaseModel +
  #           ProductSize + 
  #           ProductGroup +
  #           Drive_System+
  #           Enclosure+
  #           Ride_Control +
  #           fiProductClassDesc,
  #           , data=train.merged)
  #   print("Saving modl0",quote=F)
  #   save(modl0, file="modl0.model")
  
  buildModl1();
  
}

if(do.visualize){
  plot(train.merged$YearMade[1:100000], 
       train.merged$SalePrice[1:100000],
       xlim=c(1950,2012), xlab="year",ylab="Sale Price")
  
  plot(aggregate(SalePrice ~ YearMade, data=train.merged,mean),
       xlim=c(1950,2012),ylab="Mean Sale Price")
  
  plot(aggregate(SalePrice ~ YearMade, data=train.merged,median),
       xlim=c(1950,2012),ylab="Median Sale Price")
  
  plot(train.merged$MachineHoursCurrentMeter,
       train.merged$SalePrice, xlim=c(1,100000))
  
  plot(life, train.merged$SalePrice)
  
  plot(aggregate(SalePrice ~ fiBaseModel,data=train.merged,mean))
  
}


if(do.predict){
  modlPred2 <- predict(modl1$finalModel,newdata = test.merged)  
}

if(do.saveResults){
  ans <- cbind(test.merged$SalesID,modlPred2)
  write.csv(ans,file="ansS001.csv",row.names=F)
}

