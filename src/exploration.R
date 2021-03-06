library(dplyr)
library(imputeTS)
library(ggplot2)
library(rgdal)


data <- read.csv("data/london-borough-profiles.csv")
data <- subset(data, (Inner_Outer_London=="Inner_London")|(Inner_Outer_London=="Outer_London"))
data <- na.mean(data, option = "mean")  #impute NA values with mean

ascData <- read.csv("data/boroughs_ASC.csv")
data <- merge(data, ascData, by.x = "Code", by.y = "ONS_Code")

data$working_ASC_spend_per_pop <- round(data$X18_64_total/(data$Proportion_of_population_of_workingage_2015*data$GLA_Population_Estimate_2015),1)
data$over_65_ASC_spend_per_pop <- round(data$X65_and_over_total/(data$Proportion_of_population_aged_65_and_over_2015*data$GLA_Population_Estimate_2015),1)
data$pop_over_65 <- data$Proportion_of_population_aged_65_and_over_2015*data$GLA_Population_Estimate_2015

data$asc_per_pop <- round(data$asc_total/data$GLA_Population_Estimate_2015,1)
data$mental_health_spend_per_pop <- round((data$X65_and_over_Mental_Health_Support+data$X18_to_64_Mental_Health_Support)/data$GLA_Population_Estimate_2015,1)
data$physical_support_spend_per_pop <- round((data$X18_to_64_Physical_Support+data$X65_and_over_Physical_Support)/data$GLA_Population_Estimate_2015,1)
data$sensory_support_spend_per_pop <- round((data$X18_to_64_Sensory_Support+data$X65_and_over_Sensory_Support)/data$GLA_Population_Estimate_2015,1)
data$memcog_support_spend_per_pop <- round((data$X18_to_64_Support_for_Memory_and_cognition+data$X65_and_over_Support_for_Memory_and_Cognition)/data$GLA_Population_Estimate_2015,1)
data$learning_support_spend_per_pop <- round((data$X18_to_64_Learning_Disability_Support+data$X65_and_over_Learning_Disability_Support)/data$GLA_Population_Estimate_2015,1)


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
cLarge <- subset(cTab, (abs(Freq) > 0.70)&(Freq) != 1)
cLarge <- unique(as.data.frame(t(apply(cLarge, 1, sort))))



boroughs <- readOGR(dsn = "data/stats-boundaries-london/ESRI/", layer = "London_Borough_Excluding_MHW")
boroughs <- merge(boroughs, data, by.x = "GSS_CODE", by.y = "Code" )


#selection <- boroughs$mental_health_spend_per_pop>mean(boroughs$mental_health_spend_per_pop)
#plot(boroughs, col = "lightgrey")
#plot(boroughs[selection, ], col = "blue", add = TRUE)
spplot(boroughs[-33,], "Anxiety_score_201114_out_of_10", sub = "Anxiety score (self-rated)", col = "transparent")


spplot(boroughs[-33,], "asc_per_pop", sub = "Total adult social care spending per person / £", col = "transparent")
spplot(boroughs[-33,], "Proportion_of_population_aged_65_and_over_2015", sub = "Proportion of population aged over 65 / %", col = "transparent")
spplot(boroughs[-33,], "mental_health_spend_per_pop", sub = "Mental health ASC spending per person / £", col = "transparent")
spplot(boroughs[-33,], "working_ASC_spend_per_pop", sub = "Working age ASC spending per person working age / £", col = "transparent")
spplot(boroughs[-33,], "over_65_ASC_spend_per_pop", sub = "Over 65 ASC spending per person over 65 / £", col = "transparent")

#look into how where over 65 ASC spending does not increase with proportion of over 65s
#map 18-15 spending per capita, nd 65+ spending per capita


#produce correlation headmap of 20 interesting values
keepList <- c("Population_density_per_hectare_2015",
              "Proportion_of_population_of_workingage_2015",
              "Homes_Owned_outright_2014_pc",
              "over_65_ASC_spend_per_pop",
              "Fires_per_thousand_population_2014",
              "Anxiety_score_201114_out_of_10",
              "Happiness_score_201114_out_of_10",
              "mental_health_spend_per_pop",
              "Mortality_rate_from_causes_considered_preventable",
              "Rented_from_Local_Authority_or_Housing_Association_2014_pc",
              "Employment_rate_pc_2014",
              "Proportion_of_population_aged_65_and_over_2015",
              "Being_bought_with_mortgage_or_loan_2014_pc",
              "Number_of_cars_per_household_2011_Census",
              "asc_per_pop")

subData2 <- data[, colnames(data) %in% keepList]

C <- round(cor(subData2), 3)
colnames(C) <- c("PD",
                 "WAP",
                 "O65P",
                 "ER",
                 "F",
                 "HOO", 
                 "MH",
                 "RRLA",
                 "CPH",
                 "HS",
                 "AS",
                 "PD",
                 "O65S",
                 "TASC",
                 "MHS")

rownames(C)<- c("Pop. Density",
                "Working Age Pop.",
                "Over 65 Pop.",
                "Employment Rate",
                "Fires Rate",
                "Homes Owned Outright", 
                "Mortgaged Homes",
                "Rate Renting from LA",
                "Cars Per Household",
                "Happiness Score",
                "Anxiety Score",
                "Peventable Deaths",
                "Over 65 Care Spend",
                "Total ASC Spend",
                "Mental Health Spend")
                 
corrplot(C, method = "color", tl.col = "black")
