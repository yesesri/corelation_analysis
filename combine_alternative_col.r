#set the working directory
setwd("C:/Users/cherukuri/Desktop/GA_projects/microbiome/nick_data/")
#two files, whose rows you want to combine alternatively 
#################################################################################
######### EXAMPLE : df1 has columns named A1 , A2 , A3  ###########################
#########         df2 has columns named B1, B2 , B3    ##########################
#########   the out file has columns in following order :  A1 B1 A2 B2 A3 B3 ####
#################################################################################
df1<- read.delim2("speciesVsallfactor_corr.txt")
df2<- read.delim2("speciesVsallfactors_Pval.txt")
# zipFastener for TWO dataframes of unequal length
zipFastener <- function(df1, df2, along=2)
{
  # parameter checking
  if(!is.element(along, c(1,2))){
    stop("along must be 1 or 2 for rows and columns
         respectively")
  }
  # if merged by using zip feeding along the columns, the
  # same no. of rows is required and vice versa
  if(along==1 & (ncol(df1)!= ncol(df2))) {
    stop 
  }("the no. of columns has to be equal to merge
          them by zip feeding")
  if(along==2 & (nrow(df1)!= nrow(df2))) {
    stop ("the no. of rows has to be equal to merge them by
          zip feeding")
  }
  
  # zip fastener preperations
  d1 <- dim(df1)[along]
  d2 <- dim(df2)[along]
  # index vector 1
  i1 <- 1:d1         
   # index vector 2  
  i2 <- 1:d2 + d1     
  
  # set biggest dimension dMax
  ## equalize vector lengths, by filling gaps with NA's 
  if(d1==d2) {
    dMax <- d1
  } else if (d1 > d2) {
    length(i2) <- length(i1)    
    dMax <- d1                 
  } else  if(d1 < d2){
    length(i1) <- length(i2)    
    dMax <- d2                  
  }
  
  # zip fastener operations
  index <- as.vector(matrix(c(i1, i2), ncol=dMax, byrow=T))
  index <- index[!is.na(index)]         # remove NAs
  

  if(along==1){
    colnames(df2) <- colnames(df1)   # keep 1st colnames                  
    res <- rbind(df1,df2)[ index, ]  # reorder data frame
  }
  if(along==2) res <- cbind(df1,df2)[ , index]           
  
  return(res)
  }

###############################################################
 ## column wise
outfile<- zipFastener(df1, df2,2)
write.table(outfile,file="speciesVsallfactors_finalmatrix.txt")
