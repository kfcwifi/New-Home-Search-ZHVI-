#       Zillow Home Value Index Analyzer
#             By: The KFC WIFI
#
#   This code looks at a user-defined metro area
#   and determines the best area for investment
#--------------------------------------------------
# Data Courtesy of 
# Zillow Real Estate Research (http://www.zillow.com/research/)
# Census.gov 
# http://www.census.gov/econ/cbp/download/noise_layout/ZIP_Totals_Layout10.txt

metro  <- 'Orlando' # Metro area of Interest
# county <- ''
source("Ranking Functions v2.R")

library(data.table)
ZHVI <- fread("http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_AllHomes.csv")
temp <- tempfile()

download.file("ftp://ftp.census.gov/econ2013/CBP_CSV/zbp13totals.zip",temp, mode="wb")
unzip(temp, "zbp13totals.txt")
EMPLOY13 <- fread("zbp13totals.txt", sep=",")

download.file("ftp://ftp.census.gov/econ2012/CBP_CSV/zbp12totals.zip",temp, mode="wb")
unzip(temp, "zbp12totals.txt")
EMPLOY12 <- fread("zbp12totals.txt", sep=",")

download.file("ftp://ftp.census.gov/econ2011/CBP_CSV/zbp11totals.zip",temp, mode="wb")
unzip(temp, "zbp11totals.txt")
EMPLOY11 <- fread("zbp11totals.txt", sep=",")

if (exists("metro")){
  Matches<-grepl(metro,ZHVI$Metro) # Logical with matches to the Metro
  cat(sum(Matches,na.rm = TRUE),"Matches in the Metro",metro)
} else if (exists("county")) {
  Matches<-grepl(county,ZHVI$CountyName) # Logical with matches to the Metro
  cat(sum(Matches,na.rm = TRUE),"Matches in the County",county)
}

# Manipulate the ZHVI data
Matched_subset <- ZHVI[Matches]
avg_data<-Yearly_Avg(Matched_subset)

# Take Zipcodes and apply regressions to observe ZHVI metrics
Output <- Rank_zips(avg_data)

# Dig for Employment data that corresponds to ZHVI data (avg_data) 
combined <- Employment_Crunch(EMPLOY11,EMPLOY12,EMPLOY13,avg_data)

# Combine Employment data with ZHVI data
Result <- Inject_Employment(Output,combined)
