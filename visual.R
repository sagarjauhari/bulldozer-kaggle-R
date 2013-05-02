# Visualize state wise plot
names <- levels(train.merged$state)
for(name in names){
  v <- train.merged$state==name
  print(c(name,
          length(train.merged$SalePrice[v]),
          round(mean(train.merged$SalePrice[v]),2)))
}

names <- names(train.merged)
# % Distinct values in each column
lapply(names,function(name){
  length(unique(train.merged[[name]]))
})

# Machine Hours current meter
vec <- !is.na(train.merged$MachineHoursCurrentMeter)
plot(train.merged$MachineHoursCurrentMeter[vec],train.merged$SalePrice[vec])