library(caret)

print("Importing Data", quote = F)
trainDescr = read.csv(file="../../kaggle/bulldozers/Train.csv",header=T)
testDescr = read.csv(file="../../kaggle/bulldozers/Valid.csv", header=T)
machine.appendix = read.csv(file="../../kaggle/bulldozers/Machine_Appendix.csv", header=T)
trainClass <- trainDescr$SalePrice
trainDescr$SalePrice <- NULL
print("Note: Label class present separately in trainClass", quote = F)
print("Attributes in trainDescr:", quote = F)
print(names(trainDescr))
print("Attributes in testDescr:", quote = F)
print(names(testDescr))
