eq_data1= read.csv(file="database.csv", stringsAsFactors = FALSE)
eq_data2 = read.csv(file="ngdc_data.csv", stringsAsFactors = FALSE)

eq_data <- merge(eq_data1, eq_data2, by=c("DAY", "MONTH", "YEAR", "LATITUDE", "LONGITUDE"))

write.csv(eq_data, "eq.csv")

subEqData <- subset(eq_data, select = c("DAY", "MONTH", "YEAR", "LATITUDE", "LONGITUDE", "Depth", "Magnitude", "Magnitude.Type", "FLAG_TSUNAMI", "INTENSITY", "COUNTRY", "LOCATION_NAME", "DEATHS", "INJURIES", "DAMAGE_MILLIONS_DOLLARS", "HOUSES_DESTROYED", "HOUSES_DAMAGED", "TOTAL_MISSING"))
