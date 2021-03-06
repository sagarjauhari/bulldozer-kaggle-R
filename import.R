library(caret)

print("Importing Data", quote = F)
trainDescr = read.csv(file="../../kaggle/bulldozers/Train.csv",header=T)
testDescr = read.csv(file="../../kaggle/bulldozers/Valid.csv", header=T)
machine.appendix = read.csv(file="../../kaggle/bulldozers/Machine_Appendix.csv", header=T)
trainClass <- read.csv(file="../trainClass.csv", header=T)
trainDescr$SalePrice <- NULL
print("Note: Label class present separately in trainClass", quote = F)
