#Outliers of Missing, Deaths, Injury and damage


outlier_missing <- boxplot.stats(subEq$TOTAL_MISSING)$out  # outlier values.
outlier_missing
boxplot(subEq$TOTAL_MISSING, main="Missing", boxwex=0.1)#plots the boxplot 
mtext(paste("Outliers: ", paste(outlier_missing, collapse=", ")), cex=0.6)#displays outliers
length(outlier_missing)

outlier_deaths <- boxplot.stats(subEq$DEATHS)$out  # outlier values.
outlier_deaths
boxplot(subEq$DEATHS, main="Deaths", boxwex=0.1)#plots the boxplot 
mtext(paste("Outliers: ", paste(outlier_deaths, collapse=", ")), cex=0.6)#displays outliers
length(outlier_deaths)

outlier_injury <- boxplot.stats(subEq$INJURIES)$out  # outlier values.
outlier_injury
boxplot(subEq$INJURIES, main="Injury", boxwex=0.1)#plots the boxplot 
mtext(paste("Outliers: ", paste(outlier_injury, collapse=", ")), cex=0.6)#displays outliers
length(outlier_injury)

outlier_damage <- boxplot.stats(subEq$DAMAGE_MILLIONS_DOLLARS)$out  # outlier values.
outlier_damage
boxplot(subEq$DAMAGE_MILLIONS_DOLLARS, main="Damage", boxwex=0.1)#plots the boxplot 
mtext(paste("Outliers: ", paste(outlier_damage, collapse=", ")), cex=0.6)#displays outliers
length(outlier_damage)