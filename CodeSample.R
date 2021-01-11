#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#
#   Packages
#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# import needed packages, uncomment below lines and run if you need to install them

# install.packages("tidyverse")
# install.packages("skimr")
# install.packages("ggcorrplot")
# install.packages("ggridges")
# install.packages("cowplot")
# install.packages("RColorBrewer")

library(tidyverse)
library(skimr)
library(ggcorrplot)
library(ggridges)
library(cowplot)
library(RColorBrewer)
library(car)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#
#   Datasets
#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# import needed .csv datasets using tidyverse::read_csv()

# Life expectancy at birth by sex/country
lifeExp <- read_csv("data/lifeExpectancyAtBirth.csv")

# Medical doctors per 10000 population
docPer10k <- read_csv("data/medicalDoctors.csv")

# Nursing and Midwife personnel per 10000 population
nurseMidwifePer10k <- read_csv("data/nursingAndMidwife.csv")

# Dentists per 10000 population
dentistPer10k <- read_csv("data/Dentists.csv")

# Pharmacists per 10000 population
pharmacistPer10k <- read_csv("data/Pharmacists.csv")

# UHC index pf service Coverage (SCI)
uhcCoverage <- read_csv("data/uhcCoverage.csv")

# I handmade this .csv file myself from the information at https://www.who.int/about/who-we-are/regional-offices
whoRegionCodes <- read_csv('data/whoRegionCodes.csv')

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#
#   Data Wrangling/Cleaning
#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# Check for missing data/gauge how clean the datasets are

###################################################
# Life expectancy at birth by sex/country
###################################################
# Create a dataset to see when was the last year there is life expectancy data by country
lifeExpLatestYear <- lifeExp %>% 
  group_by(Location) %>%
  filter(Dim1 == "Both sexes") %>%
  summarize(latestYear = max(Period))

# Check to see what the latest years are
lifeExpLatestYear %>% 
  count(latestYear)

# Investigate the one country that doesn't have 2019 data
lifeExpLatestYear %>%
  filter(latestYear == 2010)
# Sudan is contained within 2 values, "Sudan" and "Sudan (until 2019)"

# Other datasets do not have 2019 data, so will have to find a common latest year
lifeExp %>% 
  group_by(Location) %>%
  filter(Dim1 == "Both sexes") %>%
  ungroup() %>%
  count(Period)
# Life Expectancy was recorded in 2000, 2010, 2015, and 2019

# Create a dataset that will only contain data from 2015 (for overlap with data from other datasets)
# This renames the Life Expectancy (First Tooltip) and Sex (Dim1) columns; also removes the Indicator column
# This will also remove "Sudan (until 2011)" but retain "Sudan"
lifeExpFinal <- lifeExp %>%
  filter(Period == 2015) %>%
  mutate(LifeExp = `First Tooltip`,
         Sex = Dim1) %>%
  select(-c(Indicator, `First Tooltip`, Dim1))

###################################################
# Doctors per 10k population
###################################################
# Check how many countries have data by the latest year it is available 
docPer10k %>%
  group_by(Location) %>%
  summarize(latestYear = max(Period)) %>%
  count(latestYear) %>%
  arrange(desc(.))
# 2018 is the latest year there is data for this, fall back on 2015 for overlap with life exp data

# Check how many countries are captured by data in 2015 (to match with life expectancy data)
docPer10k %>%
  group_by(Location) %>%
  filter(Period == 2015) %>%
  nrow()

# Create a dataset for Doctors per 10000 population in 2015 by country
# This renames the DocPer10k (First Tooltip) column; also removes the Indicator column
docPer10kFinal <- docPer10k %>%
  group_by(Location) %>%
  filter(Period == 2015) %>%
  mutate(DocPer10k = `First Tooltip`) %>%
  select(-c(Indicator, `First Tooltip`))

###################################################
# Nurses and Midwives per 10k population
###################################################
# Check how many countries have data by the latest year it is available 
nurseMidwifePer10k %>%
  group_by(Location) %>%
  summarize(latestYear = max(Period)) %>%
  count(latestYear) %>%
  arrange(desc(.))
# 2018 is the latest year there is data for this, fall back on 2015 for overlap with life exp data

# Check how many countries are captured by data in 2015 (to match with life expectancy data)
nurseMidwifePer10k %>%
  group_by(Location) %>%
  filter(Period == 2015) %>%
  nrow()

# Create a dataset for Nurses and Midwives per 10000 population in 2015 by country
# This renames the NurseMidwifePer10k (First Tooltip) column; also removes the Indicator column
nurseMidwifePer10kFinal <- nurseMidwifePer10k %>%
  group_by(Location) %>%
  filter(Period == 2015) %>%
  mutate(NurseMidwifePer10k = `First Tooltip`) %>%
  select(-c(Indicator, `First Tooltip`))

###################################################
# Dentists per 10k population
###################################################
# Check how many countries have data by the latest year it is available 
dentistPer10k %>%
  group_by(Location) %>%
  summarize(latestYear = max(Period)) %>%
  count(latestYear) %>%
  arrange(desc(.))
# 2019 is the latest year there is data for this, fall back on 2015 for overlap with life exp data

# Check how many countries are captured by data in 2015 (to match with life expectancy data)
dentistPer10k %>%
  group_by(Location) %>%
  filter(Period == 2015) %>%
  nrow()

# Create a dataset for Dentists per 10000 population in 2015 by country
# This renames the DentistPer10k (First Tooltip) column; also removes the Indicator column
dentistPer10kFinal <- dentistPer10k %>%
  group_by(Location) %>%
  filter(Period == 2015) %>%
  mutate(DentistPer10k = `First Tooltip`) %>%
  select(-c(Indicator, `First Tooltip`))

###################################################
# Pharmacists per 10k population
###################################################
# Check how many countries have data by the latest year it is available 
pharmacistPer10k %>%
  group_by(Location) %>%
  summarize(latestYear = max(Period)) %>%
  count(latestYear) %>%
  arrange(desc(.))
# 2018 is the latest year there is data for this, fall back on 2015 for overlap with life exp data

# Check how many countries are captured by data in 2015 (to match with life expectancy data)
pharmacistPer10k %>%
  group_by(Location) %>%
  filter(Period == 2015) %>%
  nrow()

# Create a dataset for Pharmacists per 10000 population in 2015 by country
# This renames the PharmacistPer10k (First Tooltip) column; also removes the Indicator column
pharmacistPer10kFinal <- pharmacistPer10k %>%
  group_by(Location) %>%
  filter(Period == 2015) %>% 
  mutate(PharmacistPer10k = `First Tooltip`) %>%
  select(-c(Indicator, `First Tooltip`))

###################################################
# UHC index of service Coverage (SCI)
###################################################
# Check how many countries have data by the latest year it is available 
uhcCoverage %>%
  group_by(Location) %>%
  summarize(latestYear = max(Period)) %>%
  count(latestYear) %>%
  arrange(desc(.))
# 2017 is the latest year there is data for this, fall back on 2015 for overlap with life exp data

# Check how many countries are captured by data in 2015 (to match with life expectancy data)
uhcCoverage %>%
  group_by(Location) %>%
  filter(Period == 2015) %>%
  nrow()

# Create a dataset for UHC index of service Coverage (SCI) in 2015 by country
# This renames the UhcCoverage (First Tooltip) column; also removes the Indicator column
uhcCoverageFinal <- uhcCoverage %>%
  group_by(Location) %>%
  filter(Period == 2015) %>% 
  mutate(UhcCoverage = `First Tooltip`) %>%
  select(-c(Indicator, `First Tooltip`))

###################################################
# WHO Region Codes
###################################################
# Remove empty columns from the dataset
whoRegionCodesFinal <- whoRegionCodes %>%
  select(c(Country, Region))

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#
#   Data Joining/Wrangling
#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# Join the datasets together
worldHealthStatTemp <- lifeExpFinal %>%
  left_join(whoRegionCodesFinal, by = c('Location' = 'Country')) %>%
  left_join(docPer10kFinal, by = c('Location', 'Period')) %>%
  left_join(nurseMidwifePer10kFinal, by = c('Location', 'Period')) %>%
  left_join(dentistPer10kFinal, by = c('Location', 'Period')) %>%
  left_join(pharmacistPer10kFinal, by = c('Location', 'Period')) %>%
  left_join(uhcCoverageFinal, by = c('Location', 'Period'))

# Drop rows with na's from the dataframe
worldHealthStat <- worldHealthStatTemp %>% drop_na()

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#
#   Summary Statistics
#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# Create summary statistics/exploring the data

# Skim the numeric variables to see mean/sd/quantiles/distribution
worldHealthStat %>%
  select(-c(Period, Sex, Location, Region)) %>%
  skim()

# Create dataset for correlation plot
corrData <- worldHealthStat %>%
  select(c(LifeExp, DocPer10k, NurseMidwifePer10k,
           DentistPer10k, PharmacistPer10k, UhcCoverage))

# Calculate the correlation values
HealthCorr <- round(cor(corrData),2)

# Display the correlation plot
ggcorrplot(HealthCorr, hc.order = TRUE, lab = TRUE) +
  ggtitle("Correlation Plot for WHO World Health Statistics Variables")

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#
#   Plotting Data
#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# Create plots to explore the data visually

# Scatterplot of life exp by uhc coverage (color coded by region)
# Hard-coded the colors in for colorblind readability 
ggplot(worldHealthStat, aes(UhcCoverage, LifeExp, color=Region)) +
  geom_point() +
  xlab("UHC Index of Service Coverage (SCI)") +
  ylab("Life Expectancy at Birth (Years)") +
  ggtitle("UHC Index of Service Coverage and Life Expectancy at Birth in 2015") +
  labs(color='Region') +
  scale_y_continuous(breaks=seq(50,90,5)) +
  scale_x_continuous(breaks=seq(20,100,10)) +
  scale_color_manual(values=c("#E69F00", "#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00")) +
  theme_light()

# Function that creates a ggridge plot when given a dataframe, numeric X column, and categorical Y column for group
ggridgeFunction <- function(df, x, y) {
  
  ggplot(df, aes_string(x, y)) +
    geom_density_ridges() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(expand = expand_scale(mult = c(0, .4))) +
    coord_cartesian(clip = "off") +
    theme_light()
  
}

# Creates a character vector of the numeric columns within the worldHealthStat dataframe
worldHealthStatNumericColumns <- worldHealthStat %>%
  select(-c(Location, Period, Sex, Region)) %>%
  colnames()

# Loop that cycles through the numeric column vector, creating a list of output ridgeline graphs
ridgelineOutput <- list()
for(i in worldHealthStatNumericColumns) {
  ridgelineOutput[[paste0(i,'plot')]] <- ggridgeFunction(worldHealthStat, i, 'Region')
}

# Creating the final set of ridgeline plots arranged in a 3 x 2 grid with A through F labels
# If you have a small monitor, this will look better if you export to .png image file (look at the toolbar on the view pane)
# Set Width to 1000, check the "Maintain aspect ratio" box, and then click "Update Preview"
plot_grid(ridgelineOutput$LifeExpplot, ridgelineOutput$DocPer10kplot, ridgelineOutput$NurseMidwifePer10kplot,
          ridgelineOutput$DentistPer10kplot, ridgelineOutput$PharmacistPer10kplot, ridgelineOutput$UhcCoverageplot,
          labels = "AUTO")

# Reorder the factors in the column "Sex", so that "Both sexes" is the middle factor for the boxplot
worldHealthStat$Sex <- factor(worldHealthStat$Sex, levels = c('Female', 'Both sexes', 'Male'))

# Create boxplot of sexes by life expectancy
# PiYG is a great colorblind friendly 3-color diverging set in RColorBrewer
ggplot(worldHealthStat, aes(x = Sex, y = LifeExp, fill = Sex)) +
  geom_boxplot() +
  xlab("Biological Sex") +
  ylab("Life Expectancy at Birth (Years)") +
  ggtitle("Biologial Sex and Life Expectancy at Birth in 2015") +
  scale_fill_brewer(palette="PiYG") +
  theme_light()

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#
#   Regression Model
#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# Create a regression model to predict Life Expectancy

# Remove the factor "Both sexes" from column "Sex" as it is just a calculated average between "Female" and "Male"
worldHealthStatModel <- worldHealthStat %>%
  filter(Sex == "Female" | Sex == "Male")

# Run a Multiple Linear Regression to predict Life Expectancy
modelLifeExp <- lm(LifeExp ~ Sex + DocPer10k + NurseMidwifePer10k +
             DentistPer10k + PharmacistPer10k + UhcCoverage, data = worldHealthStatModel)

# Check regression model performance
summary(modelLifeExp)

# Want to check if assumptions were violated
# Check if there was any multicollinearity
vif(modelLifeExp)

# Pull out the residuals and fitted values from the model
resids<-modelLifeExp$residuals
fitvals<-modelLifeExp$fitted.values

# Residuals vs Fitted, checking assumption of linearity and homoskedasticity
ggplot() + 
  geom_point(aes(fitvals,resids))+
  geom_hline(yintercept=0, col="red") +
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Fitted")
  

# Check assumption of Normality
# Histogram
ggplot() +
  geom_histogram(aes(resids),bins=20) +
  xlab("Residuals") +
  ylab("Count") +
  ggtitle("Histogram of Residuals")
# Q-Q Plot
ggplot() + 
  geom_qq(aes(sample=resids)) + 
  geom_qq_line(aes(sample=resids), color='red') +
  xlab("Theorectical Quantiles") +
  ylab("Sample Quantiles") +
  ggtitle("Q-Q Plot")

