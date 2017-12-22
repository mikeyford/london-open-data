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

spplot(wards, "bad_health_rate", sub = "Rate of bad health (self-reported)", col = "transparent")
spplot(wards, "Aged_Over65", sub = "Over 65 rate", col = "transparent")

#names(wards@data) <- abbreviate(names(wards@data), minlength=8) 

data <- wards@data

keepList <- c("pop_density",
              "Aged_0_15",
              "Aged_16_64",
              "Aged_Over65",
              "Mean_NOx",
              "bad_very_bad_health",
              "Life_Expectancy_200913",
              "Childhood_Obesity_2013",
              "Incapacity_Benefit_rate__2013",
              "Unemployment_rate_2013",
              "Crime_rate__2013",
              "Deliberate_Fires__2013",
              "Subjective_wellbeing_average_score_2013",
              "Public_Transport_Accessibility__2013")
subData <- data[, colnames(data) %in% keepList]
C <- round(cor(na.omit(subData)), 3)

rownames(C)<- c("Mean NOx",
                "Bad or V. Bad Health",
                "Pop. Under 16 %",
                "Pop. Working Age %",
                "Pop. Over 65 %",
                "Life Expectancy",
                "Child Obesity Rate",
                "Disabled Benefit Rate",
                "Unemployment Rate",
                "Crime Rate",
                "Deliberate Fire Rate",
                "Public Transit Access",
                "Wellbeing Score",
                "Pop. Density")

colnames(C) <- c("NOx",
                 "BVBH",
                 "PU16",
                 "PWA",
                 "PO65",
                 "LE", 
                 "COR",
                 "DBR",
                 "UR",
                 "CR",
                 "DFR",
                 "PTA",
                 "WS",
                 "PD")





#C <- round(cor(select_if(na.omit(wards@data), is.numeric)), 2)

corrplot(C, method = "color",  tl.col = "black")
