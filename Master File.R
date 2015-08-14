#       Zillow Home Value Index Analyzer
#             By: The KFC WIFI
#
#   This code looks at a user-defined metro area
#   and determines the best area for investment
#--------------------------------------------------

# to clear variables rm(list=ls())
library(data.table,lib.loc = "C:/Users/Kelvin/Desktop/R - Programming")
setwd("C:/Users/Kelvin/Desktop/R - Programming")
metro = 'Orlando' # Metro area of Interest
ZHVI <- fread("./Zip_Zhvi_AllHomes.csv")

Matches<-grepl(metro,ZHVI$Metro) # Logical with matches to the Metro
cat(sum(Matches,na.rm = TRUE),"Matches in the Metro",metro)

Matched_subset <- ZHVI[Matches]
Data <- Matched_subset[,6:236,with=FALSE]

# apply opertations to find yearly averages
start <- substring(names(Data)[1],1,4)
end   <- substring(names(Data)[length(Data)],1,4)
yr <- as.character(start:end)

indices <- lapply(yr,grepl,names(Data))               # Find indicies pertaining to yr
avg_data<-data.table(Matched_subset[,1:4,with=FALSE]) # Initialize avg_data table

    for (i in seq_len(NROW(indices))) {
     yy<-data.matrix(Data[,indices[[i]],with=FALSE])  # For each year, grab all the months
     avg_data[,yr[i]]<-rowMeans(yy)                   # Average all the months for that year
    }


# Identify the Best RegionName (Zip-code) to live in 


# Plot
xax <- as.integer(names(avg_data)[5:24])

lines(xax,y=avg_data[4,5:24,with=FALSE],
     xlab='Year',ylab='Zillow HVI ($)')
lines(xax,y=avg_data[5,5:24,with=FALSE],
     xlab='Year',ylab='Zillow HVI ($)')

