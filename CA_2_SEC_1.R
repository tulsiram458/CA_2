NIPC <- read.csv("NIPostcodes.csv",header = F)
NIPC  # Northern Ireland PostCode == NIPC

# question A
# number of rows 
# structure of the dataframe
# 10 rows of the dataframe

nrow(NIPC)  # number of rows
ncol(NIPC)  # number of columns


# structure of the dataframe 

str (NIPC)

# first and last 10 rows 

head(NIPC, n=10) #head gives first 10 rows


# question B: Adding a suitable name for the attributes of the dataframe 

colnames(NIPC) <- c("Organisation_Name","Sub-building_Name","Building_Name","Number","Primary_Thorfare",
                    "Alt_Thorfare","Secondary_Thorfare","Locality","Townland","Town","County","PostCode",
                    "X-Cordinates","Y-Cordinates","Primary_Key")


# question C: Replace and recode all missing values. 
#visuvalize and disscuss on the missing data.

library(mice) #Multivariate Imputation by Chained Equation library
md.pattern(NIPC) # for displaying missing data patterns 
library(VIM) # new tools for visualization of missing and imputed values
missing_values <- aggr(NIPC, prop = FALSE, number = TRUE) #calculation of the amount of missing values in variables and in combination of variables

NIPC[NIPC==""] <- NA
sum(is.na(NIPC)) # counting of the missing values and Not Applicable values
sum(!complete.cases(NIPC)) # to print rows that are complete and does not have missing values.


#question D
#Missing Data in Columnwise.
Missing_count <- sapply(NIPC, function(y) sum(length(which(is.na(y))))) # takes input in list, vector or data and returns output in vector or matrix
Missing_Count <- data.frame(Missing_Count) # creates a new data frame which has and shares some properties of matrices and lists
Missing_Count


# question E
#Move the Primary Key Identifier to the start of the database.
NIPC <- subset(NIPC, select = c(15,1:14)) # used to return subsets of a vector, matrix or data frame which meets a particular condition


# question F
#Creating of the  Limavady_data dataframe.
Limavady_data <- NIPC[which(NIPC$Locality == "LIMAVADY" | NIPC$Townland == "LIMAVADY" & NIPC$Town == "LIMAVADY"),]
Limavady_data
nrow(Limavady_data)

write.csv(Limavady_data,"Limavady.csv")
write.csv(NIPC,"CleanNIPostcodeData.csv")




#SECTION-2

#question A
getwd()
setwd("C:/Users/tulsi/Documents/week2Demo/NI Crime Data")
list.files()
list.dirs()
files <- list.files(recursive = T)
dataset <- data.frame()
#read all the csv from the directory.
for(file in files)
{
  temp_dataset <- read.csv(file,header = T)
  dataset <- rbind(dataset, temp_dataset)
  rm(temp_dataset)
}
nrow(dataset)
head(dataset, n=10)
setwd("C:/Users/tulsi/Documents/week2Demo/NI Crime Data")
write.csv(dataset,"AIINICrimeData.csv",row.names = F)
rm(dataset)


#question B
Crime_data <- read.csv("AIINICrimeData.csv")
head(Crime_data)
nrow(Crime_data)
Crime_data$Crime.ID <- NULL
Crime_data$Reported.by <- NULL
Crime_data$Falls.within <- NULL
Crime_data$LSOA.code <- NULL
Crime_data$LSOA.name <- NULL
Crime_data$Last.outcome.category <- NULL
Crime_data$Context <- NULL

summary(Crime_data,15)'12'



library(plyr)
Crime_data$Crime.type <- revalue(Crime_data$Crime.type,c("Anti-social behaviour" = "ASBO","Bicycle theft" = "BITH",
                                                         "Burglary" = "BURG","Criminal damage and arson" = "CDAR",
                                                         "Drugs" = "DRUG","Other Theft = OTTH","Public order" = "PUBO",
                                                         "Robbery" = "ROBY", "Shoplifting" = "SHOP","Theft from the person" = "THPR",
                                                         "Vehicle crime" = "VECR", "Violence and sexual offences" = "VISO",
                                                         "Other crime" = "OTCR","Other theft" = "OTTH","Possession of weapons" = "POW"))

summary(Crime_data$Crime.type,15)
write.csv(Crime_data,"AIINICrimeData.csv",row.names = F)
Final_Crime_data <-read.csv("AIINICrimeData.csv")
nrow(Crime_data)
head(Crime_data)
str(Crime_data)

temp_data <- read.csv("AIINICrimeData.csv")

attach(Crime_data)
plot(Crime_data$Crime.type,type='o',col = "Blue")



#question D
#Using the plot() function describe the crime frequency rate.
attach(Final_Crime_data)
plot(Final_Crime_data$Crime.type,ylim=c(0,200000),col = rainbow(14),main = "Crime frequeny rate",
     xlab="Crime Type",ylab="Number of Crimes")
detach(Final_Crime_data)

#question E

# remove the On or near string from the location column.
Final_Crime_data$Location <- sub("On or near ","",Final_Crime_data$Location)
head(Final_Crime_data$Location, n=10)
Final_Crime_data$Location[Final_Crime_data$Location == ""] <- NA

#question F

#Pick the random sample of 5000 entiries using set seed function by omitting location null values.

#Remove Rows With Missing Values On Columns Specified
secondary_Crime_data <- na.omit(Final_Crime_data)

#Set the seed of R's random number generator used for creating simulations or random objects that can be reproduced
set.seed(100)
random_crime_sample <- secondary_Crime_data[sample(nrow(secondary_Crime_data),5000),]





