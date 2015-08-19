#        Ranking Functions
# These functions take in the raw data and 
# determine the best regions to live in

#
# slopeCRASH= slope of ZHVI during crash
# slopePOST = slope of ZHVI after housing crash (2011-present)
#----------------------------------------------------------

sort.data.frame <- function(x, decreasing=FALSE, by=1, ... ){
  f <- function(...) order(...,decreasing=decreasing)
  i <- do.call(f,x[by])
  x[i,,drop=FALSE]
}

#--------------------------------------------------------------------------------------------
Yearly_Avg <- function(Matched_subset) {
  Data <- Matched_subset[,6:236,with=FALSE]             # Data does not have the regions info
  
  # apply opertations to find yearly averages (adapts based on first and last index)
  start <- substring(names(Data)[1],1,4)                # Start year (extracted from string)
  end   <- substring(names(Data)[length(Data)],1,4)     # Latest year (extracted from string)
  yr <- as.character(start:end)
  
  indices <- lapply(yr,grepl,names(Data))               # Find indicies pertaining to yr
  avg_data<-data.table(Matched_subset[,1:4,with=FALSE]) # Pre-allocate avg_data table
  
  for (i in seq_len(NROW(indices))) {
    yy<-data.matrix(Data[,indices[[i]],with=FALSE])  # For each year, grab all the months
    avg_data[,yr[i]]<-rowMeans(yy)                   # Average all the months for that year
  }
  invisible(avg_data)
}

#--------------------------------------------------------------------------------------------
Rank_zips <- function(avg_data) {
  D <- t(avg_data)    # Transpose Data
  Zips <- D[1,]       # Pull off the ZipCodes
  CSM <- D[2:4,]      # Pull off City,State,Metro
  D <- D[-1:-4,]      # Take away categorical data
  class(D)<-"numeric"
  
  Yearz <- as.numeric(rownames(D))
  output <- matrix(ncol=3, nrow=length(Zips))
      for (i in seq_len(NCOL(D))) {
        ff <- D[11:16,i] ~ Yearz[11:16]
        Fit <- lm(ff)
        output[i,1]<-summary(Fit)$coefficients[2]
        PostCrash <- D[-1:-15,i] ~ Yearz[-1:-15]
        PC <- lm(PostCrash)
        output[i,2]<-summary(PC)$coefficients[2]
      }
  
  output[,3]<-t(colMeans(D,na.rm =TRUE))
  output <- data.frame(output)      # Turn matrix into a dataframe
  rownames(output)<-Zips            # Update column names to be Zipcodes
  colnames(output)<- c("slopeCRASH","slopePOST","avgZHVI")
  output<-cbind(output,t(CSM))      # Add in columns with city,state,metro
  output<-sort(output,by="slopePOST",decreasing=TRUE)
  invisible(output)
}

#--------------------------------------------------------------------------------------------
Employment_Crunch <- function(EMPLOY11,EMPLOY12,EMPLOY13,avg_data) {
  Zips <- avg_data$RegionName
  zz<-grepl(paste(Zips,collapse = "|"),EMPLOY11$zip)
  subz <- data.frame(EMPLOY11[zz])
  subz <- subz[,c("zip","est")]         # Subset out the zip and # of establishments
  
  aa12 <-lapply(subz$zip,grepl,EMPLOY12$zip)
  aa13 <-lapply(subz$zip,grepl,EMPLOY13$zip)
  combined <- matrix(ncol=4, nrow=nrow(subz))
  combined[,1:2] = as.matrix(subz)
      for (i in seq_len(nrow(subz))) {
       temp12 <- EMPLOY12[aa12[[i]]]
       temp13 <- EMPLOY13[aa13[[i]]]
       combined[i,3:4] <- data.matrix(cbind(temp12$est,temp13$est))
      }
  cat(sum(zz, na.rm=TRUE),"Matches for Employment Data")
  invisible(combined)
}

#--------------------------------------------------------------------------------------------
Inject_Employment <- function(Output,combined) {
  dd<-lapply(rownames(Output),grepl,combined[,1])
  augment <- matrix(ncol=4, nrow=nrow(Output))    # Pre-allocate matrix for data augment
      for (i in seq_len(nrow(Output))) {
       augment[i,] <- combined[dd[[i]]][1:4]         # Populate augment in for loop
      }
  Output$est11 <- augment[,2]                     # Add components of augment to the ZHVI data
  Output$est12 <- augment[,3]
  Output$est13 <- augment[,4]
  invisible(Output)
}
