#life expectancy at birth by sex/region 
lifeExpRegion <- read_csv("data/WHOregionlifeExpectancyAtBirth.csv")

# Population with household expenditures on health greater than 10% of total household expenditure or income (SDG indicator 3.8.2) (%)
popHealthExpend10Perc <- read_csv("data/population10SDG3.8.2.csv")

# Population with household expenditures on health greater than 25% of total household expenditure or income (SDG indicator 3.8.2) (%)
popHealthExpend25Perc <- read_csv("data/population25SDG3.8.2.csv")

###################################################
# Population with household expenditures on health greater than 10% of total household expenditure or income
###################################################
# Checking how many countries have data by the latest year it is available 
popHealthExpend10Perc %>%
  group_by(Location) %>%
  summarize(latestYear = max(Period)) %>%
  count(latestYear) %>%
  arrange(desc(.))
# 2018 is the latest year there is data for this, fall back on 2015 for overlap with life exp data

# Checking how many countries are captured by data in 2015 (to match with life expectancy data)
popHealthExpend10Perc %>%
  group_by(Location) %>%
  filter(Period == 2015) %>%
  nrow()

# Creating a dataset for UHC index of service Coverage (SCI) in 2015 by country
popHealthExpend10PercFinal <- popHealthExpend10Perc %>%
  group_by(Location) %>%
  filter(Period == 2015)

###################################################
# Population with household expenditures on health greater than 25% of total household expenditure or income
###################################################
# Checking how many countries have data by the latest year it is available 
popHealthExpend25Perc %>%
  group_by(Location) %>%
  summarize(latestYear = max(Period)) %>%
  count(latestYear) %>%
  arrange(desc(.))
# 2017 is the latest year there is data for this, fall back on 2015 for overlap with life exp data

# Checking how many countries are captured by data in 2015 (to match with life expectancy data)
popHealthExpend25Perc %>%
  group_by(Location) %>%
  filter(Period == 2015) %>%
  nrow()

# Creating a dataset for UHC index of service Coverage (SCI) in 2015 by country
popHealthExpend25PercFinal <- popHealthExpend25Perc %>%
  group_by(Location) %>%
  filter(Period == 2015)

ggplot(worldHealthStat, aes(Location, UhcCoverage, fill=Region))+
  geom_bar(stat="summary") + xlab("Country") + ylab("UHC Index of Service Coverage (SCI)") +
  ggtitle("UHC Index of Service Coverage by Country From in 2015") + coord_flip() +
  scale_fill_manual(values=c("#E69F00", "#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00")) +
  labs(fill='Region') +
  theme(axis.text.y = element_text(size = 5.0, margin = margin(r = -8)),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0, "pt"))
