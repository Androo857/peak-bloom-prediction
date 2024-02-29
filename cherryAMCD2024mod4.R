rm(list = ls())

library(tidyverse)
library(mgcv)
library(gratia)
library(MuMIn)
library(dplyr)
#read the data
cherry <- read.csv("washingtondc.csv") %>% 
  bind_rows(read.csv("liestal.csv")) %>% 
  bind_rows(read.csv("kyoto.csv"))

#Visualize as time series with linear trend
#Note that Liestal and Washington d.c. observations begin well after Kyoto
cherry %>% 
  filter(year >= 1880) %>%
  ggplot(aes(x = year, y = bloom_doy)) +
  geom_smooth(method = lm) +
  scale_x_continuous(breaks = seq(1880, 2020, by = 20)) +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)")+
  theme(panel.grid = element_blank())

#Visualize as time series with gam smoothing
#Note any deviation from linearity - e.g. Liestal, or Washington, D.C.
cherry %>% 
  filter(year >= 1880) %>%
  ggplot(aes(x = year, y = bloom_doy)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  scale_x_continuous(breaks = seq(1880, 2020, by = 20)) +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)")+
  theme(panel.grid = element_blank())

#Non-linear models seems appropriate

#read climate data file with Japan, Kyoto, Liestal, and Washington bloom_doy, and all other Japan sites
Wdata <- read.csv("siteclimatedata.csv", header = TRUE, sep = ",")


# Fit gam model for climate
gam_fit <- gam(bloom_doy ~ s(Feb_max) + s(Jan_max, k = 3), data = Wdata, subset = year >= 1880, method = "REML")

#Model summary
summary(gam_fit)

appraise(gam_fit)

draw(gam_fit)


#Read Vancouver climate data
Vdata <- read.csv("vancouver.csv", header = TRUE, sep = ",")
#predict (hindcast) for the Vancouver climate data
vancouver_doy <- predict(gam_fit, newdata = Vdata)
#round estimates
vc_doy <- as.integer(vancouver_doy)
#view it
vc_doy
#attach the estimates to the data
Vdata1 <- Vdata %>% 
  mutate(vc_doy)

Vdata2 <- Vdata1 %>%
  rename(bloom_doy = vc_doy)

Wdata1 <- Wdata %>%
  bind_rows(Vdata2)
#Read the New York data
NYdata<- read.csv("new_york.csv", header = TRUE, sep = ",")

ny_doy <- predict(gam_fit, newdata = NYdata)
#round estimates
ny_doy <- as.integer(ny_doy)
#view it
ny_doy
#attach the estimates to the data
NYdata1 <- NYdata %>% 
  mutate(ny_doy)

NYdata2 <- NYdata1 %>%
  rename(bloom_doy = ny_doy)

Wdata2 <- Wdata1 %>%
  bind_rows(NYdata2)

#Climate data from NOAA, WeatherUnderground for January 2024 and February 2024 last checked 27th February
# + 'topped-up' with forecasts, e.g. BBC weather, meteoblue etc.
#Full references in methods

CL24 <- read.csv("Climate2024predprecip.csv", header = TRUE, sep = ",")

#Read worldwide bloom_doy WITH hindcast Vancouver and New York data added on
#W1data <- read.csv("siteclimatedatawv.csv", header = TRUE, sep = ",")

#FACTORS
#Make location a factor
CL24$location <- as.factor(CL24$location)
#Check
class(CL24$location)

#Make location a factor
Wdata2$location <- as.factor(Wdata2$location)
#Check
class(Wdata2$location)

#fit model for prediction

gam_fit_I <- gam(bloom_doy ~ s(Feb_max, by = location) + s(Jan_max, k = 3), data = Wdata2)
summary(gam_fit_I)
appraise(gam_fit_I)

#predict fort he 5 sites for 2024
predictions_gam1 <- expand_grid(location = unique(CL24$location),
                               year = 2024) %>% 
  bind_cols(predicted_doy1 = predict(gam_fit_I, newdata = CL24))

predictions_gam1

#view 2024 prediction for each site
gampred1 <- predictions_gam1 %>% 
  group_by(year,location) %>% 
  slice_tail(n = 1)

gampred1

print(gampred1 [1:5, ])

#round the predictions
submission_predictions <- gampred1 %>%
  filter(year > 2023) %>%
  mutate(predicted_doy1 = round(predicted_doy1))

submission_predictions

#subtract values from columns to bring estimates to the regionally appropriate phase
#Replace estimated Washington D.C. and Vancouver and New York 2024 values (80% flowers open) with
#regionally used value (70% flowers open) by indexing with
#bloom_doy - 1
#Explanation: the 70% value is estimated at one(1) day prior
#to the estimate value, i.e. 80% of flowers that are possible to
#be open concurrently

submission_predictions <- submission_predictions %>%
  mutate(predicted_doy1 = ifelse(location %in% c("washingtondc",
                                                 "vancouver",
                                                 "newyorkcity"), predicted_doy1 - 1, predicted_doy1))

submission_predictions
#Replace Liestal 2024  estimate value (80% flowers open) with
#regionally used value (25% flowers open) by indexing with
#bloom_doy - 5
#Explanation: the 25% value is estimated at five (5) days prior
#to the estimate value, i.e. 80% of flowers that are possible to
#be open concurrently
submission_predictions <- submission_predictions %>%
  mutate(predicted_doy1 = ifelse(location %in% c("liestal"
                    ), predicted_doy1 - 5, predicted_doy1))
#view

submission_predictions
#Place the estimates in the order required for the competition

submission_predictions <- submission_predictions %>%
  filter(year > 2023) %>%
  arrange(
    case_when(
      location == "washingtondc" ~ 1,
      location == "liestal" ~ 2,
      location == "kyoto" ~ 3,
      location == "vancouver" ~ 4,
      location == "newyorkcity" ~ 5
    )
  )

#view to check
submission_predictions

# Create a data frame with the reordered predictions
output_data <- data.frame(
  location = c("washingtondc", "liestal", "kyoto", "vancouver", "newyorkcity"),
  prediction = submission_predictions$predicted_doy1
)

# Write the data frame to a CSV file
write.csv(output_data, file = "predictions.csv", row.names = FALSE)
