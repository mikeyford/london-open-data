library(dplyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(ggmap)
require(maptools)

hp <- read.csv("data/london-house-prices-pp-2016.csv")
hp$Postcode <- gsub(' ','',hp$Postcode)
pc <- read.csv("data/london-postcode-bng-lookup.csv")
pc$Postcode <- gsub(' ','',pc$Postcode)

df <- merge(hp, pc, by = "Postcode")
df$Northings <- df$Nothings

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


#### plotting March sales

#transform to WGS84
uk_bng <- df[,c('Postcode','Eastings','Northings')]

bng <- "+init=epsg:27700"
wgs84 <- "+init=epsg:4326"

coordinates(uk_bng) <- c("Eastings", "Northings")
proj4string(uk_bng) <- CRS(bng)
uk_wgs84 = spTransform(uk_bng, CRS(wgs84))

converted_coordinates <- as.data.frame(uk_wgs84)
names(converted_coordinates) <- c("pc2","longitude", "latitude")

df <- cbind(df, converted_coordinates)

dfKC <- df %>%
  filter(District == "KENSINGTON AND CHELSEA")

# getting the map
mapgilbert <- get_map(location = c(lon = mean(dfKC$longitude), lat = mean(dfKC$latitude))-0.005, zoom = 14,
                      maptype = "hybrid", scale = 2)


df03 <- df %>%
  filter(Month == "03")

#plot montht of march
#plot random sample not march, the same size as month of march

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = df03, aes(x = longitude, y = latitude, fill = "red"), size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)


df00 <- df %>%
  filter(Month != "03")

ggmap(mapgilbert) +
  geom_point(data = sample_n(df00, 9022), aes(x = longitude, y = latitude, fill = "red"), size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)



df03_KC <- df %>%
  filter(Month == "03",District == "KENSINGTON AND CHELSEA")

df00_KC <- df %>%
  filter(Month != 03, District == "KENSINGTON AND CHELSEA")

insideKCTypical = nrow(df00_KC)/11
insideKC03 = nrow(df03_KC)
insideKCIncrease = insideKC03/insideKCTypical
sum(df03_KC$Price)

df03_notKC <- df %>%
  filter(Month == "03", District != "KENSINGTON AND CHELSEA")

df00_notKC <- df %>%
  filter(Month != 03, District != "KENSINGTON AND CHELSEA")

outsideKCTypical = nrow(df00_notKC)/11
outsideKC03 = nrow(df03_notKC)
outsideKCInrease = outsideKC03/outsideKCTypical


df03_BD <- df %>%
  filter(Month == "03",District == "BARKING AND DAGENHAM")

df00_BD <- df %>%
  filter(Month != 03, District == "BARKING AND DAGENHAM")

insideBDTypical = nrow(df00_BD)/11
insideBD03 = nrow(df03_BD)
insideBDInrease = insideBD03/insideBDTypical
sum(df03_BD$Price)

