library(dplyr)
library(ggplot2)
library(rgdal)
library(corrplot)

pol <- read.csv("data/ward_pollution.csv")
health <- read.csv("data/wards_health.csv")
age <- read.csv("data/wards_age_rate.csv")
wb <- read.csv("data/wards_well_being.csv")

wards <- readOGR(dsn = "data/stats-boundaries-london/ESRI/", layer = "London_Ward")

wards <- merge(wards, pol, by.x = "GSS_CODE", by.y = "New_Code")
wards <- merge(wards, health, by.x = "GSS_CODE", by.y = "GEO_CODE")
wards <- merge(wards, age, by.x = "GSS_CODE", by.y = "New_Code")
wards <- merge(wards, wb, by.x = "GSS_CODE", by.y = "New_ward_code")

wards$pop_density <- wards$population/wards$HECTARES

spplot(wards, "bad_health_rate", sub = "Rate of bad health (self-reported", col = "transparent")
spplot(wards, "Aged_Over65", sub = "Over 65 rate", col = "transparent")


correlation <- round(cor(select_if(na.omit(wards@data), is.numeric)), 2)
corrplot(correlation, method = "square")
