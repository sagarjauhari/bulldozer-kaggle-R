print("Importing Data", quote = F)
trainDescr = read.csv(file="../../kaggle/bulldozers/TrainAndValid.csv",header=T)
machine.appendix = read.csv(file="../../kaggle/bulldozers/Machine_Appendix.csv", header=T)
trainClass <- trainDescr[2]
trainDescr$SalePrice <- NULL
print("Note: Label class present separately in trainClass", quote = F)

### Remove unnecessary columns
print("Remove unnecessary columns", quote = F)
machine.appendix$fiProductClassDesc <- NULL # might do text processing later on
machine.appendix$fiManufacturerDesc <- NULL # fiManufacturerID present.
machine.appendix$ProductGroupDesc <- NULL       #ProductGroup present

machine.appendix$fiModelDesc <- NULL
machine.appendix$ModelID <- NULL          # Because removing fiModelDesc


trainDescr$fiModelDescriptor <- NULL      #Redundant
trainDescr$fiModelSeries <- NULL          #Redundant
trainDescr$fiProductClassDesc <- NULL     #Redundant
trainDescr$fiSecondaryDesc <- NULL        #Redundant
trainDescr$ModelID <- NULL                #Redundant
trainDescr$fiModelDesc <- NULL            #Redundant
trainDescr$ProductGroup <- NULL           #Redundant
trainDescr$ProductGroupDesc <- NULL       #Redundant
trainDescr$fiBaseModel <- NULL            #Redundant

### Merge trainDescr with machine.appendix
print("Merge trainDescr with machine.appendix, testDescr with machine.appendix", quote = F)
train.merged <- merge(trainDescr, machine.appendix, all.x=T)

### Handle MfgYear
print("Handle MfgYear", quote = F)
train.merged$YearMade <- NULL
train.merged$MfgYear[train.merged$MfgYear==1000] = NA
train.merged$MfgYear[train.merged$MfgYear==9999] = NA

### Replace missings by NAs
print("Replace missings by NAs", quote = F)
train.merged[train.merged==""] <- NA;

### Parse $saledate
print("Parse $saledate", quote = F)
train.merged$saledate <- as.numeric(
  strptime(train.merged$saledate, format="%m/%d/%Y %H:%M"))

### Replace 'None or Unspecified' by NAs
print("Replace None or Unspecified by NAs", quote = F)
train.merged[train.merged=="None or Unspecified"] <- NA;

### Specific cases
print("Handle MachineHoursCurrentMeter", quote = F)
attr <- train.merged$MachineHoursCurrentMeter
attr[attr==0]=NA
train.merged$MachineHoursCurrentMeter <- attr

### Convert relevant IDs to factors
print("Convert relevant IDs to factors", quote = F)
factors <- c(
  "auctioneerID",
  "datasource",
  "fiManufacturerID"
)
for(f in factors){
  train.merged[[f]] <- as.factor(train.merged[[f]])
}

### Clean up
rm(machine.appendix)
rm(trainDescr)
rm(attr)
rm(f)
rm(factors)

### Keep only last record in case of resales
print("Trashing previous data for multiple sales", quote = F)
train.merged <- train.merged[order(train.merged$MachineID),]
train.merged<- train.merged[!duplicated(train.merged$MachineID),]

