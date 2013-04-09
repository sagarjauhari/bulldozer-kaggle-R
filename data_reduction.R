source("import.R")
source("util.R")

#########################################
# Remove redundant variables
factors <- c(
  "ModelID",
  "YearMade",
  "fiModelDesc",
  "fiBaseModel",
  "fiSecondaryDesc",
  "fiModelSeries",
  "fiModelDescriptor",
  "fiProductClassDesc",
  "ProductGroup",
  "ProductGroupDesc"
)
for(factor in factors){
  trainDescr[[factor]] <- NULL
  testDescr[[factor]] <- NULL
}
rm(factor);
rm(factors)


train.merged <- merge(trainDescr, machine.appendix, by="MachineID",all.x=T)
test.merged <- merge(testDescr, machine.appendix, by="MachineID",all.x=T)
rm(machine.appendix)
rm(trainDescr)
rm(testDescr)

#########################################
# Remove attributes of low importance
factors <- c(
  "ModelID",
  "Pushblock",
  "Enclosure",
  "fiModelDesc",
  "MachineID",
  "Drive_System",
  "fiModelSeries",
  "Transmission",
  "Tire_Size",
  "fiModelDescriptor",
  "Travel_Controls",
  "Ripper",
  "Backhoe_Mounting",
  "ProductGroup",
  "Blade_Type",
  "state",
  "Ride_Control",
  "MachineHoursCurrentMeter",
  "auctioneerID",
  "Undercarriage_Pad_Width",
  "Pattern_Changer",
  "Thumb",
  "Stick",
  "Coupler",
  "Stick_Length",
  "datasource",
  "UsageBand",
  "Forks",
  "Steering_Controls",
  "Track_Type",
  "Grouser_Type",
  "Differential_Type",
  "Enclosure_Type",
  "Pad_Type",
  "Turbocharged",
  "Blade_Extension",
  "Grouser_Tracks",
  "Hydraulics_Flow",
  "ProductGroupDesc"
)
for(factor in factors){
  train.merged[[factor]] <- NULL
  test.merged[[factor]] <- NULL
}
rm(factor);
rm(factors)

#########################################
### Handle MfgYear
train.merged$MfgYear[train.merged$MfgYear==1000] = NA
test.merged$MfgYear[test.merged$MfgYear==1000] = NA
train.merged$MfgYear[train.merged$MfgYear==9999] = NA
test.merged$MfgYear[test.merged$MfgYear==9999] = NA

vec <- is.na(train.merged$MfgYear)
train.merged <- train.merged[!vec,]
trainClass<- trainClass[!vec,]

# Parse $saledate
train.merged$saledate <- as.numeric(format(
  as.Date(train.merged$saledate, "%m/%d/%Y %H:%M"), format = "%Y"))
test.merged$saledate <- as.numeric(format(
  as.Date(test.merged$saledate, "%m/%d/%Y %H:%M"), format = "%Y"))


for(var in colnames(train.merged)){
  if(is.factor(train.merged[[var]]) & is.factor(test.merged[[var]])){
    print(sprintf("Converting factor: %s",var))
    train.merged[[var]]<-lookup(var,train.merged)
    test.merged[[var]]<-lookup(var,test.merged)
  }
}


transVars <- c(
  "ProductSize",      
  "Blade_Width",      
  "Engine_Horsepower",
  "Hydraulics",       
  "Scarifier",        
  "Tip_Control",      
  "Coupler_System",   
  "fiBaseModel",      
  "fiSecondaryDesc",  
  "fiProductClassDesc",
  "fiManufacturerID", 
  "fiManufacturerDesc",
  "PrimarySizeBasis", 
  "PrimaryLower",     
  "PrimaryUpper"
)
xTrans <- preProcess(train.merged[,transVars])
train.processed <- predict(xTrans,train.merged[,transVars])
train.merged[,transVars] <- train.processed
test.processed <- predict(xTrans,test.merged[,transVars])
test.merged[,transVars] <- test.processed
rm(train.processed)
rm(test.processed)

train.merged$PrimaryUpper <- NULL
train.merged$PrimaryLower <- NULL
test.merged$PrimaryUpper <- NULL
test.merged$PrimaryLower <- NULL



#########################################
# Remove all data before 1950
# vec <- (train.merged$MfgYear > 1950)
# train.merged <- train.merged[vec,]
# trainClass <- trainClass[vec,]
# 
# # Remove records having NA in important variables
# vec <- is.na(train.merged$MfgYear) | 
#         is.na(train.merged$ProductSize) |
#         is.na(train.merged$fiBaseModel) |
#         is.na(train.merged$Coupler) |
#         is.na(train.merged$saledate)
# 
# train.merged <- train.merged[!vec,]
# trainClass<- trainClass[!vec,]
