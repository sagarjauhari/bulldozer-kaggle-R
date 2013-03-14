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