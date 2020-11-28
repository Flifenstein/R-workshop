# 1. Download and import dataset
setwd("C:/users/yourname/downloads") #Putting the path in the code
setwd(choose.dir()) #Selecting the working directory by hand
netflix <- read.csv("", sep=";")
View(netflix)

# 2. Import Packages
install.packages("tidyverse")
library(tidyverse)

# 3. Explore the dataset  {.tabset  .tabset-pills}
glimpse(netflix)
head(netflix)
dim(netflix)
summary(netflix)
names(netflix)
class(netflix$Date_file)

# 4. Clean the dataset
## 4.1 Select relevant variables 
netflixclean <- netflix %>%
  select(!topNodeId, bookmark)
glimpse(netflixlean)

## 4.2 Recode values  {.tabset  .tabset-pills}
netflixclean <- netflixclean %>% mutate_all(na_if,"")
glimpse(netflixlean)

# 5. Visualize data

## REMOVE OCCURRENCES WHERE SEASON AND EPISODE ARE EMPTY (BECAUSE THEY ARE NOT TV SERIES)

netflixcleanseries<- na.omit(netflixclean)
netflixcleanseries <- netflixcleanseries %>%
  count(seriesTitle,dateStr)
## LET'S CONSIDER "BINGE-WATCHING" 6 OR MORE EPISODES PER DAY AND SORT BY DATE
netflixcleanseries<- netflixcleanseries[netflixcleanseries$n >= 6,]
netflixcleanseries
netflixcleanseries<- netflixcleanseries[order(netflixcleanseries$Date),]
netflixcleanseries

## GROUPING DATA BY TV SERIES TITLE AND SORTING BY NUMBER OF EPISODES VIEWED
netflixcleanseries<- netflixcleanseries %>% 
  group_by(seriesTitle) %>% 
  summarise(n = sum(n)) %>%
  arrange(desc(n))

## PLOTTING TOP 10 OF BINGE-WATCHING TV SERIES
netflixclean_top <- netflixcleanseries %>% 
  top_n(10) %>%
  ggplot(aes(x = reorder(seriesTitle, n), y = n)) +
  geom_col(fill = "#0097d6") +
  coord_flip() +
  ggtitle("Top 10 tvshows") +
  labs(x = "Series Netflix", y = "Total episodes") +
  theme_minimal()

netflixclean_top

netflix %>%
  select(title,duration,date,series) %>%
  mutate(
    date = as.POSIXct(date/1000, origin='1970-01-01'),
    type = ifelse(is.na(netflix$series), "Film", "Series")
  ) %>%
  ggplot(aes(x = date, weight = duration/60/60, fill = type)) +
  geom_histogram(binwidth=60*60*24*7)+
  labs(y = "Hours per week", x = "Date", title = "Netflix Watchtime", color="Type", fill="Type")
