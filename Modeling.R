
# KDD Project: Analysis of earthQuake Dataset
# Group: Anal Shah, Ashwini Kadam, Kutika Patil
# Phase 4: Modeling and Evaluation of Data

library(plyr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(devtools)
library(rpart)
library(rpart.plot)
library(sp)
library(rworldmap)
library(arulesViz)
library(arules)

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
summary(numerical_data)
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

# Categorizing countires into continents
AFRICA <- c('ALGERIA','ANGOLA','BENIN','BOTSWANA','BURKINA','BURUNDI','CAMEROON','CAPE VERDE','CENTRAL AFRICAN REPUBLIC','CHAD','COMOROS','CONGO','CONGO DEMOCRATIC REPUBLIC OF', 'DJIBOUTI','EGYPT','EQUATORIAL GUINEA','ERITREA','ETHIOPIA','GABON','GAMBIA','GHANA','GUINEA','GUINEA-BISSAU','IVORY COAST','KENYA','LESOTHO','LIBERIA','LIBYA','MADAGASCAR','MALAWI','MALI','MAURITANIA','MAURITIUS','MOROCCO','MOZAMBIQUE','NAMIBIA','NIGER','NIGERIA','RWANDA','SAO TOME AND PRINCIPE','SENEGAL','SEYCHELLES','SIERRA LEONE','SOMALIA','SOUTH AFRICA','SOUTH SUDAN','SUDAN','SWAZILAND','TANZANIA','TOGO','TUNISIA','UGANDA','ZAMBIA','ZIMBABWE')
ASIA<-c('AFGHANISTAN','BAHRAIN','BANGLADESH','BHUTAN','BRUNEI','CAMBODIA','CHINA','EAST TIMOR','INDIA','INDONESIA','IRAN','IRAQ','ISRAEL','JAPAN','JORDAN','KAZAKHSTAN','NORTH KOREA', 'SOUTH KOREA','KUWAIT','KYRGYZSTAN','LAOS','LEBANON','MALAYSIA','MALDIVES','MONGOLIA','MYANMAR (BURMA)','NEPAL','OMAN','PAKISTAN','PHILIPPINES','QATAR','RUSSIAN FEDERATION','RUSSIA','SAUDI ARABIA','SINGAPORE','SRI LANKA','SYRIA','TAIWAN','TAJIKISTAN','THAILAND','TURKEY','TURKMENISTAN','UNITED ARAB EMIRATES','UZBEKISTAN','VIETNAM','YEMEN')
EUROPE<-c('ALBANIA','ANDORRA','ARMENIA','AUSTRIA','AZERBAIJAN','AZORES (PORTUGAL)','BELARUS','BELGIUM','BOSNIA-HERZEGOVINA','BULGARIA','CROATIA','CYPRUS','CZECH REPUBLIC','DENMARK','ESTONIA','FINLAND','FRANCE','GEORGIA','GERMANY','GREECE','HUNGARY','ICELAND','IRELAND','ITALY','LATVIA','LIECHTENSTEIN','LITHUANIA','LUXEMBOURG','MACEDONIA','MALTA','MOLDOVA','MONACO','MONTENEGRO','NETHERLANDS','NORWAY','POLAND','PORTUGAL','ROMANIA','SAN MARINO','SERBIA AND MONTENEGRO','SLOVAKIA','SLOVENIA','SPAIN','SWEDEN','SWITZERLAND','UKRAINE','UK TERRITORY','UNITED KINGDOM','VATICAN CITY')
NORTHAMERICA<-c('ANTIGUA AND BARBUDA','BAHAMAS','BARBADOS','BELIZE','CANADA','COSTA RICA','CUBA','DOMINICA','DOMINICAN REPUBLIC','EL SALVADOR','GRENADA','GUATEMALA','GUADELOUPE','HAITI','HONDURAS','JAMAICA','MARTINIQUE','MEXICO','NICARAGUA','PANAMA','SAINT KITTS AND NEVIS','SAINT LUCIA','SAINT VINCENT AND THE GRENADINES','TRINIDAD AND TOBAGO','UNITED STATES', 'USA', 'USA TERRITORY')
SOUTHAMERICA<-c('ARGENTINA','BOLIVIA','BRAZIL','CHILE','COLOMBIA','ECUADOR','GUYANA','PARAGUAY','PERU','SURINAME','URUGUAY','VENEZUELA')
OCEANIA<-c('AUSTRALIA','FIJI','KERMADEC ISLANDS (NEW ZEALAND)','KIRIBATI','MARSHALL ISLANDS','MICRONESIA','NAURU','NEW CALEDONIA','NEW ZEALAND','PALAU','PAPUA NEW GUINEA','SAMOA','SOLOMON ISLANDS','TONGA','TUVALU','VANUATU', 'WALLIS AND FUTUNA (FRENCH TERRITORY)')
ANTARCTICA <-c('ANTARCTICA', 'SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS')
OCEAN_SEA <- c('BERING SEA', 'ATLANTIC OCEAN', 'SOLOMON SEA','INDIAN SEA', 'INDIAN OCEAN')

subEqData$CONTINENT <- subEqData$COUNTRY
subEqData$CONTINENT<-factor(subEqData$CONTINENT)
levels(subEqData$CONTINENT)
levels(subEqData$CONTINENT) <- list('AFRICA' = AFRICA, 'ASIA' = ASIA, 'EUROPE' = EUROPE,
                                    'N_AMERICA' = NORTHAMERICA, 'S_AMERICA'= SOUTHAMERICA,
                                    'OCEANIA' = OCEANIA, 'ANTARCTICA' = ANTARCTICA, 'OCEAN_SEA' = OCEAN_SEA)

subEqData$Magnitude.Cat = cut(subEqData$Magnitude, br=c(5,5.9,6.9,7.9,9.9), labels=c("Moderate","Strong","Major","Great"))

subEqData$YEAR.Cat = cut(subEqData$YEAR, br=c(1960,1970,1980,1990,2000,2010,2020),
                         labels=c("1970","1980","1990","2000","2010","2016"))

library(ggmap)
world<-map_data("world")

## Remove the antartica region from the world map
world <- world[world$region != "Antarctica",]

# Mapping earthquake locations on map
map<-ggplot()+geom_map(data=world,map=world,aes(x=world$long,y=world$lat,map_id=world$region))
year_fact<-factor(subEqData$YEAR)
p <- map + geom_point(data = subEqData, aes(x = LONGITUDE.x, y = LATITUDE.x, 
                            frame = subEqData$YEAR,
                            cumulative = TRUE,size=subEqData$Magnitude), alpha = 0.3, 
                            size = 2.5,color="yellow")+
                             geom_jitter(width = 0.1) +theme_void()


# Constructing decision tree for predicting magnitude category
cont_fact<-as.factor(subEqData$CONTINENT)
output.tree<- rpart(Magnitude.Cat ~ YEAR.Cat + cont_fact,data = subEqData,
                    method = "class", control=rpart.control(cp=0.001))

# plotting tree 
prp(output.tree, faclen=4, cex=0.7)


# Converting numerical data into numerical data as knn does not accept
# missing values and factor variables
subEqData1<-subEqData
subEqData1$Magnitude.Cat<-as.numeric(subEqData1$Magnitude.Cat)
subEqData1$CONTINENT<-as.numeric(subEqData1$CONTINENT)
#subEqData1$COUNTRY<-as.numeric(subEqData1$COUNTRY)

# spliting data into training and test data set
# Taking  Magnitude, Depth, Intensity, Continent and Decade as predictors
trainingSet <- subEqData1[1:1100,c(7, 8, 11, 22,24)] # c(5,6,7,8, 10,11,22)]
testSet <- subEqData1[1101:1493, c(7, 8,11, 22,24)] #c(5,6,7,8,10,11,22)]
trainingOutcomes <- subEqData1[1:1100, 23]
testOutcomes <- subEqData1[1101:1493, 23]

# Predicting outcomes for test data using knn
library(class)
predictions <- knn(train = trainingSet, cl = trainingOutcomes, k = 3,
                   test = testSet)

## Display table of false positives and false negetive
table(testOutcomes, predictions)

# Create and display treemap for earthquake based on Continents, Countries and magnitude
library(treemap)
library(RColorBrewer)
treemap(subEqData, #Your data frame object
        index=c("CONTINENT","COUNTRY"),  #A list of your categorical variables
        vSize = "Magnitude",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        palette=brewer.pal(n=8,"RdYlGn"), #Select your color palette from the RColorBrewer presets or make your own.
        title="Earthquake Analysis", #Customize your title
        algorithm = "pivotSize",
        fontsize.title = 14 #Change the font size of the title
)

#Linear Modelling
alli.mod1 = lm(subEqData$Magnitude ~ subEqData$DEATHS)
with(subEqData, plot(Magnitude, INTENSITY, main="Linear Regression"))
abline(lm(subEqData$Magnitude ~ subEqData$DEATHS),col="red",lwd=1.5)
summary(alli.mod1)

#Logistic Regression
#Logistic regression is the appropriate regression analysis to conduct when the dependent variable is dichotomous (binary).

model <- glm(subEqData$FLAG_TSUNAMI ~ subEqData$INTENSITY + subEqData$Magnitude,family=binomial)

summary(model)
plot(INTENSITY/Magnitude ~ FLAG_TSUNAMI, data=subEqData)
lines(subEqData$FLAG_TSUNAMI, model$fitted, type="l", col="red")


### Categorize data
subEqData$Magnitude.Cat = cut(subEqData$Magnitude, br=c(5,5.9,6.9,7.9,9.9), labels=c("Moderate","Strong","Major","Great"))

subEqData$Magnitude.Cat = cut(subEqData$Magnitude, br=c(5,5.9,6.9,7.9,9.9), labels=c("Moderate", "Strong","Major","Great"))

subEqData$Depth.Cat = cut(subEqData$Depth, br=c(-0.1,70,300,700), labels=c("Shallow","Intermediate","Deep"))

subEqData$YEAR.Cat = cut(subEqData$YEAR, br=c(1960,1979,1994,2019), labels = c("More than 35 years ago", "Near past", "Recent"))

subEqData$Magnitude.Cat <- factor(subEqData$Magnitude.Cat)
subEqData$REGION <- factor(subEqData$REGION)

### get association rules

rules <- apriori(subset(subEqData, select = c("Magnitude.Cat", "YEAR.Cat", "REGION")), parameter= list(supp=0.005, conf=0.6))
rules.sorted <- sort(rules, by="lift")

inspect(rules)

plot(rules)
plot(rules, method="graph", control=list(type="items"))

### Create map

region <- joinCountryData2Map(subEqData, joinCode = "NAME", nameJoinColumn = "COUNTRY")
mapCountryData(region, nameColumnToPlot = "Magnitude.Cat")

# Association Rule
### Categorize data
subEqData$Magnitude.Cat = cut(subEqData$Magnitude, br=c(5,5.9,6.9,7.9,9.9), labels=c("Moderate","Strong","Major","Great"))

subEq$Magnitude.Cat = cut(subEq$Magnitude, br=c(5,5.9,6.9,7.9,9.9), labels=c("Moderate", "Strong","Major","Great"))

subEq$Depth.Cat = cut(subEq$Depth, br=c(-0.1,70,300,700), labels=c("Shallow","Intermediate","Deep"))

subEq$YEAR.Cat = cut(subEq$YEAR, br=c(1960,1979,1994,2019), labels = c("More than 35 years ago", "Near past", "Recent"))

subEq$Magnitude.Cat <- factor(subEq$Magnitude.Cat)
subEq$REGION <- factor(subEq$REGION)

### get association rules

install.packages('arules')
library(arules)
# find association rules with default settings
rules <- apriori(subset(subEq, select = c("Magnitude.Cat", "YEAR.Cat", "REGION")), parameter= list(supp=0.005, conf=0.6))
rules.sorted <- sort(rules, by="lift")

inspect(rules)

install.packages("digest")
install.packages("dplyr")
install.packages("arulesViz", dependencies = TRUE)
library(arulesViz)

plot(rules)
plot(rules, method="graph", control=list(type="items"))

### Create map
install.packages('rworldmap',dependencies=TRUE)
library(sp)
library(rworldmap)

region <- joinCountryData2Map(subEqData, joinCode = "NAME", nameJoinColumn = "COUNTRY")
mapCountryData(region, nameColumnToPlot = "Magnitude.Cat")

