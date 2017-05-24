eq_data1= read.csv(file="database.csv", stringsAsFactors = FALSE)
eq_data2 = read.csv(file="ngdc_data.csv", stringsAsFactors = FALSE)

eq_data <- merge(eq_data1, eq_data2, by=c("DAY", "MONTH", "YEAR", "HOUR", "MINUTE"))

eq_data$diffLat <- (eq_data$LATITUDE.x - eq_data$LATITUDE.y)
eq_data$diffLong <- (eq_data$LONGITUDE.x - eq_data$LONGITUDE.y)

write.csv(eq_data, "eq.csv")

eq_data = read.csv(file="eq.csv", stringsAsFactors = FALSE)

subEqData <- subset(eq_data, select = c("DAY", "MONTH", "YEAR", "LATITUDE.x", "LONGITUDE.x", "Depth", "Magnitude", "Magnitude.Type", "FLAG_TSUNAMI", "INTENSITY", "COUNTRY", "LOCATION_NAME", "DEATHS", "INJURIES", "DAMAGE_MILLIONS_DOLLARS", "HOUSES_DESTROYED", "HOUSES_DAMAGED", "TOTAL_MISSING"))
