library(dplyr)
library(ggplot2)
library(rgdal)
library(rgeos)

hp <- read.csv("data/london-house-prices-pp-2016.csv")
pc <- read.csv("data/london-postcode-bng-lookup.csv")

df <- merge(x = pc, y = hp, by = "Postcode", all.y = TRUE)

dropList <- c("X.x","X.y")
df <- df[ , !(names(df) %in% dropList)]

dim(df) #number of rows
str(df) #col types

summary(df$Price) #mean and median, one house at £1 - worth mentioning!



### Plot median price map ###

boroughMedians <- df %>%
  group_by(District) %>%
  dplyr::summarize(Median = median(Price, na.rm=TRUE))

boroughs <- readOGR(dsn = "data/stats-boundaries-london/ESRI/", layer = "London_Borough_Excluding_MHW")
boroughs$District<-sapply(boroughs$NAME, toupper)
boroughs <- merge(boroughs, boroughMedians, by = "District")
boroughs$Median[25] <- 990000   #source London DataStore

borough_centres <- SpatialPointsDataFrame(gCentroid(boroughs, byid=TRUE), boroughs@data, match.ID=FALSE)

l1 = list("sp.text", borough_centres@coords,as.character(borough_centres$NAME),col="grey", cex=0.75,font=2)
spplot(boroughs, "Median", sub = "Median House Price / £", col = "transparent", sp.layout=l1)




df$Month <- substr(df$Date, 6, 7)

monthlyTotal <- df %>%
  group_by(Month) %>%
  dplyr::summarize(Median_Total = median(Price, na.rm=TRUE),Mean_Total = mean(Price, na.rm=TRUE),Transactions_Total = n())

monthlyFlats <- df %>%
  filter(Type == "F") %>%
  group_by(Month) %>%
  dplyr::summarize(Median_Flats = median(Price, na.rm=TRUE),Mean_Flats = mean(Price, na.rm=TRUE),Transactions_Flats = n())

monthlyHouses <- df %>%
  filter(Type == "S" | Type == "D" | Type == "T") %>%
  group_by(Month) %>%
  dplyr::summarize(Median_Houses = median(Price, na.rm=TRUE),Mean_Houses = mean(Price, na.rm=TRUE),Transactions_Houses = n())

monthlyOther <- df %>%
  filter(Type == "O") %>%
  group_by(Month) %>%
  dplyr::summarize(Median_Other = median(Price, na.rm=TRUE),Mean_Other = mean(Price, na.rm=TRUE),Transactions_Other = n())

monthly <- merge(x = monthlyFlats, y = monthlyHouses, by = "Month")
monthly <- merge(x = monthly, y = monthlyOther, by = "Month")
monthly <- merge(x = monthly, y = monthlyTotal, by = "Month")



ggplot(data=monthly, aes(Month, Transactions, group = 1)) + 
        geom_line(aes(y = Transactions_Flats, colour = "Flats")) +
        geom_point(aes(y = Transactions_Flats)) +
        geom_line(aes(y = Transactions_Houses, colour = "Houses")) +
        geom_point(aes(y = Transactions_Houses)) +
        geom_line(aes(y = Transactions_Other, colour = "Other")) +
        geom_point(aes(y = Transactions_Other))




