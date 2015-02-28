library(RODBC)
library(tidyr)
library(dplyr)
library(lubridate)

# Connect to external database

channel <- odbcConnect("discountasp", uid = "SQL2008_508574_uwmep_user", pwd = "Manis9")

sm <-sqlFetch(channel, "landMammal")  # read data from SQL database

close(channel)

glimpse(sm)


#dim(sm)

sm$Year <- year(sm$Date)

smSubset <- subset(sm, select = c("Site", "LandNumber", "Year", "Station", "Species", "Sex", "Weight", "TotalBody"))

smSubset$TotalBody <- as.numeric(smSubset$TotalBody)

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


#########################################################################################
# Means Weight and TotalBody
#########################################################################################

smSubsetMeans <- smSubset %>%
  filter(Year==1993) %>%                                 
  arrange(Species, Sex) %>%
  group_by(Species, Sex) %>%
  summarise(meanWeight = mean(Weight))
  

arrange(smSubset, Year, Species, Sex, Weight, TotalBody)

smSubset[ order(smSubset$Year, smSubset$Species, smSubset$Sex, smSubset$Weight, smSubset$TotalBody), ]

options(digits=1)

#filter(smSubset, Species=="SOVA", Site=="MFC REG 4", Year==1993,Sex=="M")


firstmeantest <- group_by(smSubset, Species, Site, Year, Sex, mean(Weight, TotalBody))




