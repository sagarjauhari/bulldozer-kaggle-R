print("Importing Data", quote = F)
trainDescr = read.csv(file="../../kaggle/bulldozers/TrainAndValid.csv",header=T)
machine.appendix = read.csv(file="../../kaggle/bulldozers/Machine_Appendix.csv", header=T)
#trainClass <- trainDescr[2]
#trainDescr$SalePrice <- NULL
print("Note: Label class present separately in trainClass", quote = F)

### Remove unnecessary columns
print("Remove unnecessary columns", quote = F)
machine.appendix$fiProductClassDesc <- NULL # might do text processing later on
machine.appendix$fiManufacturerDesc <- NULL # fiManufacturerID present.
machine.appendix$ProductGroupDesc <- NULL   #ProductGroup present

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
trainDescr$UsageBand <- NULL              #MachineUsageCurrentMeter present; so redundant

### Merge trainDescr with machine.appendix
print("Merge trainDescr with machine.appendix", quote = F)
train.merged <- merge(trainDescr, machine.appendix, all.x=T)

### Handle MfgYear
print("Handle MfgYear", quote = F)
train.merged$YearMade <- NULL
train.merged$MfgYear[train.merged$MfgYear==1000] = NA
train.merged$MfgYear[train.merged$MfgYear==9999] = NA

### Parse $saledate
print("Parse $saledate", quote = F)
train.merged$saledate <- as.numeric(
  strptime(train.merged$saledate, format="%m/%d/%Y %H:%M"))

### Replace missings by NAs
print("Replace missings by NAs", quote = F)
train.merged[train.merged==""] <- NA;

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

### Remove nearly empty columns from train.merged
threshold = 0.01
for(colname in names(train.merged)){
  nonEmptyRatio = prop.table(table(is.na(train.merged[[colname]])))[1]
  if(nonEmptyRatio < threshold){
    train.merged[[colname]] <- NULL
    print(sprintf("Removed %s; Only %f filled",colname,nonEmptyRatio))
  }
}
rm(colname)
rm(nonEmptyRatio)
rm(threshold)

### Handle ambiguous factor values
#Transmission
print("Converting 'Transmission' values to lower case",quote=F)
train.merged$Transmission <- as.factor(tolower(train.merged$Transmission))

#Blade_Width
print("Converting 'Blade_Width' values to numeric",quote=F)
bw <- as.character(train.merged$Blade_Width)
bw[bw=="<12'"]<-11
bw[bw=="12'"] <-12
bw[bw=="13'"] <-13
bw[bw=="14'"] <-14
bw[bw=="16'"] <-16
train.merged$Blade_Width<-as.numeric(bw)
rm(bw)

#Engine_Horsepower
train.merged$Engine_Horsepower <- NULL #Useless

#Tire_Size
print("Converting 'Tire_Size' values to numeric",quote=F)
ts <- as.character(train.merged$Tire_Size)
ts[ts=="20.5"  ] <-20.5    
ts[ts=="23.5"	] <-23.5    
ts[ts=="29.5"	] <-29.5    
ts[ts=="13\""	] <-13    
ts[ts=="15.5"	] <-15.5    
ts[ts=="20.5\""	] <-20.5  
ts[ts=="10\""	] <-10    
ts[ts=="10 inch"] <-10 
ts[ts=="26.5"	] <-26.5    
ts[ts=="14\""	] <-14    
ts[ts=="17.5\""	] <-17.5 
ts[ts=="17.5"	] <-17.5 
ts[ts=="15.5\""	] <-15.5 
ts[ts=="23.5\""	] <-23.5 
ts[ts=="7.0\""	] <-7 
ts[ts=="23.1\""	] <-23.1 
train.merged$Tire_Size <- as.numeric(ts)
rm(ts)

#Undercarriage_Pad_width
print("Converting 'Undercarriage_Pad_width' values to numeric",quote=F)
train.merged$Undercarriage_Pad_Width <- 
  as.numeric(substr(as.character(train.merged$Undercarriage_Pad_Width),1,2))

#Stick_Length
print("Converting 'Stick_Length' values to numeric",quote=F)
sl <- strsplit(as.character(train.merged$Stick_Length),"'")
sl <- sapply(sl,function(item){item[1]})
train.merged$Stick_Length <- as.numeric(sl)
rm(sl)


#Refresh levels of factor variables
train.merged<- droplevels(train.merged)

