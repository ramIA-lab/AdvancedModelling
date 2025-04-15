# This code has been created by Karina Gibert, from Universitat Politècnica de Catalunya (Barcelona, Spain)
# Please, in all uses of this code, refer to her and the paper https://doi.org/10.1080/00207160.2013.783209
# Complete reference:
#  Gibert, K. (2014). Mixed intelligent-multivariate missing imputation. International Journal of Computer Mathematics, 91(1), 85-96
#Barcelona May 2019
# Some updates were performed by Dante Conti & Sergi Ramirez

install.packages("StatMatch")
library(cluster)
require(StatMatch)

#assume missings represented with NA
uncompleteVar<-function(vector){any(is.na(vector))}

Mode <- function(x) 
{
  x<-as.factor(x)
  maxV<-which.max(table(x))
  return(levels(x)[maxV])
}



MiMMi <- function(data, priork=-1)
{
  #Identify columns without missings
  colsMiss<-which(sapply(data, uncompleteVar))
  if(length(colsMiss)==0){
    print("Non missing values found")
    out<-dd
    }else{
    K<-dim(data)[2]
    colsNoMiss<-setdiff(c(1:K),as.vector(colsMiss))
  
    #cluster with complete data
    dissimMatrix <- daisy(data[,colsNoMiss], metric = "gower", stand=TRUE)
    distMatrix<-dissimMatrix^2
  
    hcdata<-hclust(distMatrix, method = "ward.D2")
    plot(hcdata)
    nk<-2
    if(priork==-1){
      print("WARNING: See the dendrogramm and ZOOM if required")
      print("and enter a high number of clusters")
      nk<-readline("(must be a positive integer). k: ")
      nk<-as.integer(nk)
    }else{nk<-priork}
  
    partition<-cutree(hcdata, nk)

    CompleteData<-data
    #nomes cal per tenir tra?a de com s'ha fet la substituci?
    newCol<-K+1
    CompleteData[,newCol]<-partition
    names(CompleteData)[newCol]<-"ClassAux"
  
    setOfClasses<-as.numeric(levels(as.factor(partition)))
    imputationTable<-data.frame(row.names=setOfClasses)
    p<-1
  
    for(k in colsMiss)
    {
       #Files amb valors utils
       rowsWithFullValues<-!is.na(CompleteData[,k])
    
       #calcular valors d'imputacio
       if(is.numeric(CompleteData[,k]))
       {
          imputingValues<-aggregate(CompleteData[rowsWithFullValues,k], by=list(partition[rowsWithFullValues]), FUN=mean)
       }else{
          imputingValues<-aggregate(CompleteData[rowsWithFullValues,k], by=list(partition[rowsWithFullValues]), FUN=Mode)
       }
    
       #Impute
    
       for(c in setOfClasses)
       {
          CompleteData[is.na(CompleteData[,k]) & partition==c,k]<-imputingValues[c,2]
       }
    
       #Imputation Table
       imputationTable[,p]<-imputingValues[,2]
       names(imputationTable)[p]<-names(data)[k]
       p<-p+1
    }
    
    rownames(imputationTable)<-paste0("c", 1:nk)
    out<-new.env()
    out$imputedData<-CompleteData
    out$imputation<-imputationTable
  }
  return(out)
}



### Checking

dd<-iris
#Artificially create missings in the matrix to test the function
dd[sample(150, 20),2]<-NA
dd[sample(150, 20), 5]<-NA
#Now dd has missing values and MIMMI can help


#run MIMMI
dimpute<-MiMMi(dd)

#table of imputation values used
dimpute$imputation

#imputed dataset
dimpute$imputedData

plot(density(na.omit(dd[,2])))
lines(density(dimpute$imputedData[,2]),col=2)
prop.table(table(iris[,5]))
prop.table(table(dimpute$imputedData[,5]))
