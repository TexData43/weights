library(tidyverse)
library(lubridate)

### read master weight file into R
weights <- read_csv("share_weight.csv")
### Gather the data from columns into a single row for tidy data
wt_gather <- weights %>% gather(date, weight, 5:117)

### Convert the dates into machine-readable month-day-year dates
wt_gather$date <- mdy(wt_gather$date)
wt_gather$DOB <- mdy(wt_gather$DOB)

### Calculate the difference in days between the date of birth (DOB)
### and the specific date the weight was taken
### convert weight to numeric instead of character
wt_age <- wt_gather %>% 
  mutate(age_wt = date - DOB) %>% 
  filter(age_wt > 0)
wt_age$weight <- as.numeric(wt_age$weight)

### start the summarize pipe, exclude unknown genotypes, NA weights or outliers (> 55)
### group by SEX, GENOTYPE, and by the age in days then calc the mean
wt_sum <- wt_age %>% 
filter(GENOTYPE != "NA", GENOTYPE != "?", weight != "NA", weight < 55) %>% 
  group_by(SEX, GENOTYPE, age_wt) %>% 
  summarise(n = n(), weight = mean(weight, na.rm = T))

### view the end results
wt_sum

### Faceted graph by genotype and sex
(g1 <- ggplot(data = wt_sum, aes(x = age_wt, y = weight, color = GENOTYPE)) + 
    geom_line() + geom_point() + facet_wrap(~SEX, nrow = 2) + labs(x = "Age in Days", y = "Body weight in grams"))

### save the plot
ggsave("weight_age.png", width = 30, height = 20, units = "cm")

###  Save table to CSV file
write.csv(wt_age, "wt_age.csv")
