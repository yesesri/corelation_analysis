#set working Directory
setwd("C:\\Users\\cherukuri\\Desktop\\")
# matrix with columns and rows, for correlaton calculation
mat<-read.delim("otu.txt")
mat.in<-mat[,1:151]
#command to calculate correlation 
corr.matrix<-cor(mat.in)
write.table(corr.matrix,file="otu_corr_matrix.txt")
n <- ncol(corr.matrix)
cmat <- col(corr.matrix)
ind <- order(-cmat, corr.matrix, decreasing = TRUE) - (n * cmat - n) 
dim(ind) <- dim(corr.matrix)
colnames(ind) <- colnames(corr.matrix)
out <- cbind(ID = c(col(ind)), ID2 = c(ind))
Final<- as.data.frame(cbind(out, cor = corr.matrix[out]))
write.csv(Final,file="C:/Users/cherukuri/Desktop/final.csv")
f=Final
for (i in 1:nrow(Final))
{for(j in 1:2) 
{
  f[i,j]=row.names(corr.matrix)[Final[i,j]] 
}
}
#seggregate positive and negative correlation 
positive=f[f[,3]>.5,]
negative=f[f[,3]< -.5,]
write.table(positive,file="positive_corr_matrix.txt")
write.table(negative,file="negative_corr_matrix.txt")
