library(RODBC)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Connect to external database

channel <- odbcConnect("discountasp", uid = "SQL2008_508574_uwmep_user", pwd = "Manis9")

sm <-sqlFetch(channel, "landMammal")  # read data from SQL database

close(channel)

glimpse(sm)


#dim(sm)

sm$Year <- year(sm$Date)

# Creates new data set based on year

smSubset <- subset(sm, select = c("Site", "LandNumber", "Year", "Station", "Species", "Sex", "Weight", "TotalBody")) %>%
  filter(Sex=="M" | Sex=="F")
# Creates new data set that contains Site, LandNumber, Year, Station, Species, Sex, Weight, and Totalbody.
# Only uses male and female entries. 
  smSubset$TotalBody <- as.numeric(smSubset$TotalBody)
  
  table(smSubset$Species)
# Shows the number of entries for each species.  
  Species.freq <- table(smSubset$Species) 
# Creates a Table object that shows the number of entries for each species.
  Species.Dates <- table(smSubset$Year)
# Creates a Table object that shows the number of entries for each of the years.
  Species.Dates.Species <- table(smSubset$Date, smSubset$Species)

  dim(Species.Dates)
# Gives the dimensions of "Species.Dates". 

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
  
# Creates data subset that is composed of all the entries from 1993 and isolates the columns "Species" and "Sex".
# Then it groups the entries by "Species" and "Sex". And finaly it averages the weight of each species male and female.

arrange(smSubset, Year, Species, Sex, Weight, TotalBody)

smSubset[ order(smSubset$Year, smSubset$Species, smSubset$Sex, smSubset$Weight, smSubset$TotalBody), ]

options(digits=1)

#filter(smSubset, Species=="SOVA", Site=="MFC REG 4", Year==1993,Sex=="M")


firstmeantest <- group_by(smSubset, Species, Site, Year, Sex, mean(Weight, TotalBody))



smSubsetMeans1 <- smSubset %>%
 filter(Year==1993) %>%   

 arrange(Species, Sex) %>%
 group_by(Species, Sex) %>% 
 summarise(meanWeight = mean(Weight), n = n())



smSubsetMeansall <- smSubset %>% 
    arrange(Species, Sex, Year) %>%
  group_by(Species, Sex, Year) %>% 
  #summarise(meanWeight = mean(Weight), n = n()) %>%
  
  
  
ggplot(smSubset, aes(x=Weight, y=TotalBody)) + geom_point()+geom_smooth()+facet_wrap(Species~Sex)



###############################################################################################################
#Log Tansformations
###############################################################################################################



smSubsetPEMA <- subset(sm, select = c("Site", "LandNumber", "Year", "Station", "Species", "Sex", "Weight", "TotalBody")) %>%
  filter(Sex=="M" | Sex=="F") %>%
  filter(Species=="PEMA")


<<<<<<< HEAD
=======

  ggplot(smSubsetPEMA, aes(x=Weight, y=TotalBody)) + geom_point()+geom_smooth()+facet_wrap(Species~Sex)+ scale_y_log10() + scale_x_log10()



#log10 for all Species

>>>>>>> ff23f9319afec83483c700b21accbef1d86605fe

  ggplot(smSubsetPEMA, aes(x=Weight, y=TotalBody)) + geom_point()+geom_smooth()+facet_wrap(Species~Sex)+ scale_y_log10() + scale_x_log10()

smSubsetFinal <- subset(sm, select = c("Site", "LandNumber", "Year", "Station", "Species", "Sex", "Weight", "TotalBody")) %>%
  filter(Sex=="M" | Sex=="F") %>%
  filter(Species=="PEMA" | Species=="MILO" | Species=="MIMO" | Species=="MIPE" | Species=="MYGA" | Species=="PHIN" | Species=="THTA" | Species=="ZAPR")


<<<<<<< HEAD
#log10 for all Species
=======
ggplot(smSubsetFinal, aes(x=Weight, y=TotalBody)) + 
  geom_point() + geom_smooth() + facet_wrap(Species~Sex) + 
  scale_y_log10() + scale_x_log10() +
  xlab("Weight (g)") + ylab("Total Body Length (mm)") 
>>>>>>> ff23f9319afec83483c700b21accbef1d86605fe

  #expand_limits(x = 150, y = 50)

smSubsetFinal <- subset(sm, select = c("Site", "LandNumber", "Year", "Station", "Species", "Sex", "Weight", "TotalBody")) %>%
  filter(Sex=="M" | Sex=="F") %>%
  filter(Species=="PEMA" | Species=="MILO" | Species=="MIMO" | Species=="MIPE" | Species=="MYGA" | Species=="PHIN" | Species=="THTA" | Species=="ZAPR")


ggplot(smSubsetFinal, aes(x=Weight, y=TotalBody)) + 
  geom_point() + geom_smooth() + facet_wrap(Species~Sex) + 
  scale_y_log10() + scale_x_log10() +
  xlab("Weight (g)") + ylab("Total Body Length (mm)") +
  
  
 

#############################################################
  #PEMA Subset
#############################################################


filterPEMA <- subset(sm, select = c("Site", "LandNumber", "Year", "Station", "Species", "Sex", "Weight", "TotalBody")) %>%
  filter(Year== 1993 | 1994 | 1995) %>%
  filter(Sex=="M" | Sex=="F") %>%
  filter(Species=="1")

  
ggplot(filterPEMA, aes(x=Weight, y=TotalBody)) + 
  geom_point() + geom_smooth() + facet_wrap(Sex~Year) + 
  scale_y_log10() + scale_x_log10() +
  xlab("Weight (g)") + ylab("Total Body Length (mm)")

#####################################################################
#Final loop for Spieces
#####################################################################


SpeciesNames <- c("PEMA", "MILO", "MIMO", "MIPE","MYGA","PHIN","THTA","ZAPR")
  

SpeciesNames2 <- smSubset %>% 
  select(Species) %>%
  distinct()

specieslist <- unique(smSubset$Species)
specieslist



for(i in 1:12){
  
  SpeciesPlotAll <- subset(sm, select = c("Site", "LandNumber", "Year", "Station", "Species", "Sex", "Weight", "TotalBody")) %>%
    filter(Year== 1993 | 1994 | 1995) %>%
    filter(Sex=="M" | Sex=="F") %>%
    filter(Species==specieslist[i]) 
  print(SpeciesPlotAll)
  print(ggplot(SpeciesPlotAll, aes(x=Weight, y=TotalBody, main=i)) + 
    geom_point() + geom_smooth() + facet_wrap(Sex~Year) + 
    scale_y_log10() + scale_x_log10() + ggtitle(specieslist[i]) +
    xlab("Weight (g)") + ylab("Total Body Length (mm)")) %>%
    

}


# Creates a scatter plot of the weight and length for each species separated by sex and year.