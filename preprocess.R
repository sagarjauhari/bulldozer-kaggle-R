print("Preprocessing", quote = F)
### Remove unnecessary columns
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

testDescr$fiModelDescriptor <- NULL      #Redundant
testDescr$fiModelSeries <- NULL          #Redundant
testDescr$fiProductClassDesc <- NULL     #Redundant
testDescr$fiSecondaryDesc <- NULL        #Redundant
testDescr$ModelID <- NULL                #Redundant
testDescr$fiModelDesc <- NULL            #Redundant
testDescr$ProductGroup <- NULL           #Redundant
testDescr$ProductGroupDesc <- NULL       #Redundant 
testDescr$fiBaseModel <- NULL            #Redundant

### Merge trainDescr with machine.appendix, testDescr with machine.appendix
train.merged <- merge(trainDescr, machine.appendix, all.x=T)
test.merged <- merge(testDescr, machine.appendix, all.x=T)

### Handle MfgYear
train.merged$YearMade <- NULL
test.merged$YearMade <- NULL
train.merged$MfgYear[train.merged$MfgYear==1000] = NA
test.merged$MfgYear[test.merged$MfgYear==1000] = NA
train.merged$MfgYear[train.merged$MfgYear==9999] = NA
test.merged$MfgYear[test.merged$MfgYear==9999] = NA

### Parse $saledate
train.merged$saledate <- as.numeric(
  strptime(train.merged$saledate, format="%m/%d/%Y %H:%M"))
test.merged$saledate <- as.numeric(
  strptime(test.merged$saledate, format="%m/%d/%Y %H:%M"))

### Replace 'None or Unspecified' by NAs
train.merged[train.merged=="None or Unspecified"] <- NA;
test.merged[test.merged=="None or Unspecified"] <- NA;

### Replace missings by NAs
train.merged[train.merged==""] <- NA;
test.merged[test.merged==""] <- NA;

### Specific cases
attr <- train.merged$MachineHoursCurrentMeter
attr[attr==0]=NA
train.merged$MachineHoursCurrentMeter <- attr

attr <- test.merged$MachineHoursCurrentMeter
attr[attr==0]=NA
test.merged$MachineHoursCurrentMeter <- attr
rm(attr)

### Convert relevant IDs to factors
factors <- c(
  "auctioneerID",
  "datasource",
  "fiManufacturerID"
  )
for(f in factors){
  train.merged[[f]] <- as.factor(train.merged[[f]])
  test.merged[[f]] <- as.factor(test.merged[[f]])
}

