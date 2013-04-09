getEmptyRatio <- function(data){
  emptyRatio = data.frame(attribute=c(),"Non-Empty"=c())
  for(colname in names(data)){
    nonEmptyRatio = prop.table(
      table(!is.na(data[[colname]])))["TRUE"]
    emptyRatio = rbind(emptyRatio,data.frame(attribute=colname, "Non-Empty" = round(nonEmptyRatio,digits=3)))
  }
  rownames(emptyRatio) <- NULL
  return(emptyRatio)
}

getNumFactors <- function(data){
  numfac = data.frame(attribute=c(),"NumFactors"=c())
  for(colname in names(data)){
    if(!is.factor(data[[colname]])){
      numfac =rbind(numfac,data.frame(attribute=colname,"NumFactors"=NA))
    }else{
      numfac =rbind(numfac,data.frame(attribute=colname,
                                      "NumFactors"=length((levels(data[[colname]])))))
    }
  }
  return(numfac)
}

getNumDistinct <- function(data){
  numfac = data.frame(attribute=c(),"NumUniq"=c())
  for(colname in names(data)){
    if(is.factor(data[[colname]])){
      numfac =rbind(numfac,data.frame(attribute=colname,"NumUniq"=NA))
    }else{
      numfac =rbind(numfac,data.frame(attribute=colname,
                                      "NumUniq"=length(unique(data[[colname]]))))
    }
  }
  return(numfac)
}

writeResults <- function(data,filename){
  testData = read.csv(file="../../kaggle/bulldozers/Valid.csv", header=T)
  id <- test.merged$SalesID
  rm(testData)
  the_answer <- cbind(id,data);
  colnames(the_answer) <- c("SalesID","SalePrice")
  
  write.csv(the_answer[order(the_answer[,1]),],file=filename,row.names=F)
  rm(the_answer)
}

######## Functions ###########
# The lookup function is used to map factors to numbers.
# It creates the mapping using the distinct values across
# the training and test sets, thereby providing a consistent
# mapping everytime. 
nom2num <- function(){
  for( var in colnames(train.merged)){
    if(is.factor(train.merged[[var]])){ 
      print(sprintf("Converting factor%s",var))
      distinct = unique(c(
        as.character(train.merged[[var]]),
        as.character((test.merged[[var]]))
      ))
      df <- data.frame(x = distinct, y = c(1:length(distinct)))
      
      train.merged[[var]] <- merge(
        train.merged[[var]], df, all.x=T, by.y='x')[,2]
      test.merged[[var]] <- merge(
        test.merged[[var]], df, all.x=T, by.y='x')[,2]
    }
  }
}

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
