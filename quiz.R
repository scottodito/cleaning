#week 3 quiz

###########################################################
#Q1
#---------------------------------------------------------#

#read in the file

df <- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv")

#logical vector that house > 10 acres 
#sold > $10,000 worth of agriculture

agricultureLogical <- (df$ACR == 3 & df$AGS == 6)

#Apply the which() function
#df rows => logical vector is TRUE.

which(agricultureLogical)

#Answer
#125 238 262


###########################################################
#Q2
#---------------------------------------------------------#

#get jpeg (downloaded locally)
jeffPeg <- readJPEG("getdatajeff.jpg", native=TRUE)

#quantile with 10 divisions
jQuant <- quantile(jeffPeg,  prob = seq(0, 1, length = 11))

#Answer
#jQuant[4]    #jQuant[9]
#-15259150  , -10575416


###########################################################
#Q3
#---------------------------------------------------------#

#GDP for the 190 
#ranked countries in this data set:
df <- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv")

#educational data from set:
df2 <- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv")

#make a copy so when we remove we don't have issue
cleanedCountryList <- df

#create empty vector to store indices for removing
remVector <- vector(mode="numeric", length=0)
for(i in 1:nrow(df)){
  
  #use the ternary style check
  #if false => build array with index
  #if !match key table & "" 
  ifelse((match(df[i,"X"], df2$CountryCode, nomatch=FALSE) & df[i,"X"] != "" ), TRUE, remVector <- append(remVector,i))

}

#remove items at indices
cleanedCountryList <-cleanedCountryList[-(remVector),];

#remove invalid gdp values
cleanedCountryList <- cleanedCountryList[suppressWarnings(na.omit(as.numeric(as.character(cleanedCountryList$Gross.domestic.product.2012)))),]

#order by gdp DESC
cleanedCountryList <- cleanedCountryList[order(na.omit(as.numeric(as.character(cleanedCountryList$Gross.domestic.product.2012))), decreasing = TRUE), ]

#3 answer
nrow(cleanedCountryList) #189 matches
cleanedCountryList[13,] #13th country is St. Kitts and Nevis

###########################################################
#Q4
#---------------------------------------------------------#

#average GDP ranking for "High income: OECD" 
#& "High income: nonOECD" group

#ready frame for merge
#change column name 
colnames(cleanedCountryList)[which(names(cleanedCountryList) == "X")] <- "CountryCode"

#combine frames
combinedFrame <- merge(cleanedCountryList, df2, by="CountryCode")

#cast gdp as numeric
combinedFrame[,"Gross.domestic.product.2012"] <- sapply(combinedFrame[,"Gross.domestic.product.2012"], function(x){as.numeric(as.character(x))})

#order frame
combinedFrame <- combinedFrame[order(combinedFrame$Gross.domestic.product.2012, decreasing = TRUE), ]

#get average for two groups
hiMean <- mean(combinedFrame$Gross.domestic.product.2012[combinedFrame$Income.Group == "High income: OECD"])
hiMeanNOECD <- mean(na.omit(combinedFrame$Gross.domestic.product.2012[combinedFrame$Income.Group == "High income: nonOECD"]))

#answer = 32.96667, 91.91304

###########################################################
#Q4
#---------------------------------------------------------#

#Cut the GDP ranking into 5 separate quantile groups. 
#Make a table versus Income.Group. How many countries
#are Lower middle income but among the 38 nations with highest GDP?

##create new frame with quantiles
newQuarterFrame <- within(combinedFrame, quartile <- as.integer(cut(Gross.domestic.product.2012, quantile(Gross.domestic.product.2012, probs=0:5/5, na.rm = TRUE), include.lowest=TRUE)))

#get the best GDP
checkTop <- newQuarterFrame[newQuarterFrame$quartile == 1,]

#find hpw many exist at the lower income
length(checkTop$Income.Group[checkTop$Income.Group =='Lower middle income'])

#answer = 5