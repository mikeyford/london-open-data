library(dplyr)
library(imputeTS)
library(ggplot2)
library(rgdal)


data <- read.csv("data/london-borough-profiles.csv")
data <- subset(data, (Inner_Outer_London=="Inner_London")|(Inner_Outer_London=="Outer_London"))
data <- na.mean(data, option = "mean")  #impute NA values with mean


dropList = c("GLA_Population_Estimate_2015",
             "GLA_Household_Estimate_2015",
             "Inland_Area_Hectares",
             "Net_internal_migration_2014",
             "Net_international_migration_2014",
             "Net_natural_change_2014",
             "pc_of_second_largest_migrant_population_2011",
             "pc_of_third_largest_migrant_population_2011",
             "Overseas_nationals_entering_the_UK_NINo_201415",
             "Number_of_jobs_by_workplace_2013",
             "Jobs_Density_2013", #dominated by city of london 
             "Number_of_active_businesses-_2013",
             "New_Homes_net_201314",
             "Total_carbon_emissions_2013",
             "Number_of_cars-_2011_Census")

subData <- data[, !colnames(data) %in% dropList]

C <- round(cor(select_if(subData, is.numeric)), 2)
cTab <- as.data.frame(as.table(C))
cLarge <- subset(cTab, (abs(Freq) > 0.75)&(Freq) != 1)
cLarge <- unique(as.data.frame(t(apply(cLarge, 1, sort))))


boroughs <- readOGR(dsn = "data/stats-boundaries-london/ESRI/", layer = "London_Borough_Excluding_MHW")



