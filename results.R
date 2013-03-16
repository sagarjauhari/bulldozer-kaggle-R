ans <- 

testDescr = read.csv(file="../../kaggle/bulldozers/Valid.csv", header=T)
id <- testDescr$SalesID
rm(testDescr)
write.csv(cbind())