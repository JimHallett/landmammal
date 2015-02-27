library(RODBC)
library(tidyr)
library(dplyr)

# Connect to external database

channel <- odbcConnect("discountasp", uid = "SQL2008_508574_uwmep_user", pwd = "Manis9")

sm <-sqlFetch(channel, "landMammal")  # read data from SQL database

close(channel)

glimpse(sm)


dim(sm)

sm$Year <- year(sm$Date)

smSubset <- subset(sm, select = c("Site", "LandNumber", "Year", "Station", "Species", "Sex", "Weight", "TotalBody"))



table(smSubset$Species)
Species.freq <- table(smSubset$Species)



Species.Dates <- table(smSubset$Year)
Species.Dates.Species <- table(smSubset$Date, smSubset$Species)
dim(Species.Dates)


# table.obj <- table(smSubset$Species, smSubset$Year)
# 
# barplot(table.obj, xlab="Species", ylab="Year")
# 
# Species.TotalBody.obj<- table(smSubset$TotalBody)
# 
# colSums (Species.TotalBody, na.rm = FALSE, dims = 1)



Species.obj <- table(smSubset$Species, smSubset$Year)


arrange(smSubset, Year, Species, Sex, Weight, TotalBody)

smSubset[ order(smSubset$Year, smSubset$Species, smSubset$Sex, smSubset$Weight, smSubset$TotalBody), ]



filter(smSubset, Species==SOVA, Site==MFC REG 4, Year==1993,sex==M)








