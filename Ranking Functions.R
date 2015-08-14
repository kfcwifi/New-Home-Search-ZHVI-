sort.data.frame <- function(x, decreasing=FALSE, by=1, ... ){
  f <- function(...) order(...,decreasing=decreasing)
  i <- do.call(f,x[by])
  x[i,,drop=FALSE]
}

Rank_zips <- function(avg_data) {
  D <- t(avg_data)    # Transpose Data
  Zips <- D[1,]       # Pull off the ZipCodes
  CSM <- D[2:4,]      # Pull off City,State,Metro
  D <- D[-1:-4,]      # Take away categorical data
  class(D)<-"numeric"
  
  Yearz <- as.numeric(rownames(D))
  output <- matrix(ncol=3, nrow=length(Zips))
      for (i in seq_len(NCOL(D))) {
        ff <- D[,i] ~ Yearz
        Fit <- lm(ff)
        output[i,1]<-summary(Fit)$coefficients[2]
        PostCrash <- D[-1:-15,i] ~ Yearz[-1:-15]
        PC <- lm(PostCrash)
        output[i,2]<-summary(PC)$coefficients[2]
      }
  
  output[,3]<-t(colMeans(D,na.rm =TRUE))
  output <- data.frame(output)      # Turn matrix into a dataframe
  rownames(output)<-Zips            # Update column names to be Zipcodes
  colnames(output)<- c("slopePRE","slopePOST","avgZHVI")
  output<-cbind(output,t(CSM))      # Add in columns with city,state,metro
  sort(output,by="slopePOST",decreasing=TRUE)
  invisible(output)

}