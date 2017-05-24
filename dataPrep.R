eq_data1= read.csv(file="database.csv", stringsAsFactors = FALSE)
eq_data2 = read.csv(file="ngdc_data.csv", stringsAsFactors = FALSE)

eq_data <- merge(eq_data1, eq_data2, by=c("DAY", "MONTH", "YEAR", "HOUR", "MINUTE"))

eq_data$diffLat <- (eq_data$LATITUDE.x - eq_data$LATITUDE.y)
eq_data$diffLong <- (eq_data$LONGITUDE.x - eq_data$LONGITUDE.y)

write.csv(eq_data, "eq.csv")
eq_data = read.csv(file="eq.csv", stringsAsFactors = FALSE)

subEqData <- subset(eq_data, select = c("DAY", "MONTH", "YEAR", "LATITUDE.x", "LONGITUDE.x", "Depth", "Magnitude", "Magnitude.Type", "FLAG_TSUNAMI", "INTENSITY", "COUNTRY", "LOCATION_NAME", "DEATHS", "INJURIES", "DAMAGE_MILLIONS_DOLLARS", "HOUSES_DESTROYED", "HOUSES_DAMAGED", "TOTAL_MISSING"))
write.csv(subEqData, "subEq.csv")
subEqData = read.csv(file="subEq.csv", stringsAsFactors = FALSE)

# Converting flag for Tsunami into binary numerical values
# Tsunami occured -> 1
# Tsunami did not occur -> 0
subEqData$FLAG_TSUNAMI[which(subEqData$FLAG_TSUNAMI=="")]<-0
subEqData$FLAG_TSUNAMI[which(subEqData$FLAG_TSUNAMI=="Tsu")]<-1
subEqData$FLAG_TSUNAMI<-as.numeric(as.character(subEqData$FLAG_TSUNAMI))

# Imputing missing numerical values(NAs) :
# We are using MICE package which will impute and predict values for missing data 
# depending on the exploratory variables used in equation.
# Here, we have complete data for Location co-ordinates as well as Magnitude and depth of 
# earthquakes occured. So we opt to use these variables for prediction.

# First we will take only significant numerical columns from data set which we think 
# will affect the missing values data prediction.
# Lets focus on people affected -> deaths, injuries
# We are removing row_id, Date, Tusnami flag, intensity, Country, location, magnitude type, 
# missing people and other property related damage.
numerical_data <- subEqData[,-c(1:4, 9:13, 16:19)]

# Install MICE package and display missing data pattern using md.pattern function
install.packages("mice")
library(mice)
md.pattern(subEqData)

# For better visualization of this pattern, we are using VIM package
install.packages("VIM")
library(VIM)
mice_plot <- aggr(numerical_data, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(numerical_data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Let's impute the missing values now
imputed_Data <- mice(numerical_data, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

# Check new calculated imputed values for dependent variables
imputed_Data$imp$DEATHS
imputed_Data$imp$INJURIES

# We got 5 sets of imputed data from above analysis, Choose any one or all to complete dataset
completedData <-complete(imputed_Data,2)
completedData
summary(completedData)

# We can also buid predictiove model using 5 sets in imputed data and using with() command
fit <- with(data = imputed_Data, exp = lm(DEATHS ~ Depth +Magnitude))
combine<-pool(fit)
summary(combine)

numerical_data$DEATHS<-completedData$DEATHS
numerical_data$INJURIES<-completedData$INJURIES

# Now we will impute NAs present for missing people and we will use 
# imputed data of deaths and injuries for that.
numerical_data$MISSING<-subEqData$TOTAL_MISSING
imputed_Data1 <- mice(numerical_data, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data1)
completedData1 <-complete(imputed_Data1,3)
completedData1
summary(completedData1)
numerical_data$MISSING<-completedData1$MISSING

# Also proces Intensity variable from data set to remove NAs
numerical_data$INTENSITY<-subEqData$INTENSITY
imputed_Data1 <- mice(numerical_data, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data1)
completedData1 <-complete(imputed_Data1,3)
completedData1
summary(completedData1)
numerical_data$INTENSITY<-completedData1$INTENSITY

# We will use similar process to impute NAs in variables like damage in million dollars, 
# house damanged and destroyed 
numerical_data$DAMAGE_MILLIONS_DOLLARS<-subEqData$DAMAGE_MILLIONS_DOLLARS
numerical_data$HOUSES_DESTROYED<-subEqData$HOUSES_DESTROYED
numerical_data$HOUSES_DAMAGED<-subEqData$HOUSES_DAMAGED
imputed_Data1 <- mice(numerical_data, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data1)
completedData1 <-complete(imputed_Data1,3)
completedData1
summary(completedData1)
numerical_data$DAMAGE_MILLIONS_DOLLARS<-completedData1$DAMAGE_MILLIONS_DOLLARS
numerical_data$HOUSES_DESTROYED<-completedData1$HOUSES_DESTROYED
numerical_data$HOUSES_DAMAGED<-completedData1$HOUSES_DAMAGED

# This plot shows distribution of variables as individual points on graph
stripplot(imputed_Data1)

# now lets marge this imputed data into our original data frame
subEqData$DEATHS<-numerical_data$DEATHS
subEqData$INJURIES<-numerical_data$INJURIES
subEqData$TOTAL_MISSING<-numerical_data$MISSING
subEqData$INTENSITY<-numerical_data$INTENSITY
subEqData$DAMAGE_MILLIONS_DOLLARS<-numerical_data$DAMAGE_MILLIONS_DOLLARS
subEqData$HOUSES_DESTROYED<-numerical_data$HOUSES_DESTROYED
subEqData$HOUSES_DAMAGED<-numerical_data$HOUSES_DAMAGED

# From summary, we can verify that there no NA's present for mumerical values now
summary(subEqData)

# For the sake of convenience, we will create two new colums which aggreagare damage
# in terms of both humans and property
subEqData$PeopleAffected <- rowSums(subEqData[,c("DEATHS", "INJURIES", "TOTAL_MISSING")], na.rm=T)
subEqData$PropertyDamanged <- rowSums(subEqData[,c("HOUSES_DESTROYED", "HOUSES_DAMAGED")], na.rm=T)

write.csv(subEqData, "subEq.csv")
