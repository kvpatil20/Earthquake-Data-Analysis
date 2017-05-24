eq_data1= read.csv(file="C:\\Users\\kruti\\Desktop\\Sem 2\\KDD\\database.csv", 
                   na.strings="", 
                   stringsAsFactors = FALSE)
eq_data2 = read.csv(file="C:\\Users\\kruti\\Desktop\\Sem 2\\KDD\\database2.csv", 
                    na.strings="", 
                    stringsAsFactors = FALSE)

eq_data <- merge(eq_data1, eq_data2, by=c("DAY", "MONTH", "YEAR", "HOUR", "MINUTE"))

write.csv(eq_data, "C:\\Users\\kruti\\Desktop\\Sem 2\\KDD\\eq.csv")

subEqData <- subset(eq_data, select = c("DAY", "MONTH", "YEAR", "LATITUDE.x", "LONGITUDE.x", "Depth", "Magnitude", "Magnitude.Type", "FLAG_TSUNAMI", "INTENSITY", "COUNTRY", "LOCATION_NAME", "DEATHS", "INJURIES", "DAMAGE_MILLIONS_DOLLARS", "HOUSES_DESTROYED", "HOUSES_DAMAGED", "TOTAL_MISSING"))

write.csv(subEqData, "C:\\Users\\kruti\\Desktop\\Sem 2\\KDD\\subEqData.csv")
#Density plot. Only works for numerical values. 
plot(density(subEqData$Depth))
plot(density(subEqData$Magnitude))


table(subEqData$Magnitude)



#Pie chart with values rounded off to nearest 0.5 value.
pie(table(ceiling(subEqData$Magnitude*2)/2))


pie(table(ceiling(subEqData$Depth*2)/2))

#x<-subEqData$Depth
#y<-subEqData$Magnitude
#df<-data.frame(x,y)
#rm(x,y) # remove objects
#head(df)
#attach(df)
#index of the outlier from x
#(a<-which(x%in%boxplot.stats(x)$out))
#index of the outlier from y
#(b<-which(y%in%boxplot.stats(y)$out))
#detach(df)
#outlier.list1<- intersect(a,b)
#plot(df)
#points(df[outlier.list1],col="red",pch="+",cex=2.5)

#plot(subEqData$Depth, subEqData$Magnitude, main="Outliers", xlab="Magnitude", ylab="Depth", pch="*", col="red", cex=2)
#abline(lm(Depth ~ Magnitude, data=subEqData), col="blue", lwd=3, lty=2)

#Univariate
outlier_mag <- boxplot.stats(subEqData$Magnitude)$out  # outlier values.
outlier_mag
boxplot(subEqData$Magnitude, main="Magnitude", boxwex=0.1)#plots the boxplot 
mtext(paste("Outliers: ", paste(outlier_mag, collapse=", ")), cex=0.6)#displays outliers

boxplot(subEqData$Magnitude)#comparing it with the above graph. No difference

outlier_depth <- boxplot.stats(subEqData$Depth)$out  # outlier values.
outlier_depth
#count(outlier_depth)- does not work without installing and calling the package. Does not give the total count. Just the frequency.
#summary(outlier_depth)- of no use
boxplot(subEqData$Depth, main="Depth", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_depth, collapse=", ")), cex=0.6)

install.packages("plyr")
library(plyr)

#No use
#count(table(outlier_depth))
#count.fields(outlier_depth)

#nrow(table(outlier_depth))#total number of rows

#Gives total number of outliers
length(outlier_depth)

length(outlier_mag)

cor(subEqData$Depth, subEqData$Magnitude)

