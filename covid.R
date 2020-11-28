# 1. Download and import dataset
setwd("C:/users/yourname/downloads") #Putting the path in the code
setwd(choose.dir()) #Selecting the working directory by hand
COVID_cases <- read.csv("COVID-19_casus_landelijk.csv", sep=";")
View(COVID_cases)

# 2. Import Packages
install.packages("tidyverse")
library(tidyverse)

# 3. Explore the dataset
glimpse(COVID_cases)
head(COVID_cases)
dim(COVID_cases)
summary(COVID_cases)
names(COVID_cases)
class(COVID_cases$Date_file)

# 4. Clean the dataset
## 4.1 Select relevant variables
COVID_cases_clean <- COVID_cases %>%
  select(!Date_file)
glimpse(COVID_cases_clean)

## 4.2 Recode values
COVID_cases_clean <- COVID_cases %>%
  select(!Date_file) %>%
  mutate(
    Deceased = Deceased %>% recode(Yes = TRUE, No = FALSE, .default = NA),
    Hospital_admission = Hospital_admission %>% recode(Yes = T, No = F, .default = NA)
  )
glimpse(COVID_cases_clean)

## 4.3 Convert to correct datatype
COVID_cases_clean <- COVID_cases %>%
  select(!Date_file) %>%
  mutate(
    Deceased = Deceased %>% recode(Yes = T, No = F, .default = NA),
    Hospital_admission = Hospital_admission %>% recode(Yes = T, No = F, .default = NA),
    Date_statistics = Date_statistics %>% as.Date(),
    Week_of_death = Week_of_death %>% as.character() %>% as.Date("%Y%W")
  )
glimpse(COVID_cases_clean)

# 5. Visualize data
## 5.1 Create barchart
COVID_cases_clean %>%
  ggplot(aes(x = Agegroup)) +
  geom_bar()

## 5.2 Filter province {.tabset  .tabset-pills}
prov <- "Overijssel" #Instead of "Overijssel, you could use a different province of course"
COVID_cases_clean %>%
  filter(Province == prov) %>%
  ggplot(aes(x = Agegroup)) +
  geom_bar()

## 5.3 Styling your graphs {.tabset  .tabset-pills}
COVID_cases_clean %>%
  filter(Province == prov) %>%
  ggplot(aes(x = Agegroup)) +
  geom_bar(fill="Purple") + # Instead of purple, you could use a different color
  labs(title = "Age distribuation of corona patients in Overijssel", x = "Age group", y = "Positive cases")

## 5.4 Creating a piechart {.tabset  .tabset-pills}
COVID_cases_clean %>%
  filter(Province == prov) %>%
  ggplot(aes(x = "",fill = Sex)) +
  geom_bar() +
  coord_polar("y") +
  labs(title = "Gender of positive COVID-19 cases")

## 5.5 Create histogram
COVID_cases_clean %>%
  filter(Province == prov) %>%
  ggplot(aes(x = Date_statistics)) +
  geom_histogram(fill = "blue") +
  labs(title = "Positive corona cases over time in Overijssel", x = "Date", y = "Positive cases")

## 5.6 Set bins
COVID_cases_clean %>%
  ggplot(aes(x = Date_statistics)) +
  geom_histogram(binwidth = 7, fill = "blue") +
  labs(title = "Positive corona cases over time in Overijssel", x = "Date", y = "Positive cases per week")

## 6. Be creative