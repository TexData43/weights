library(tidyverse)
library(lubridate)

### read master file into R and confirm data
### Convert age, sex, gt into factors for graphing
weights <- read_csv("share_weight.csv")

wt_gather <- weights %>% gather(date, weight, 5:117)

wt_gather$date <- mdy(wt_gather$date)
wt_gather$DOB <- mdy(wt_gather$DOB)

wt_age <- wt_gather %>% 
  mutate(age_wt = date - DOB, age_wt = age_wt) %>% 
  filter(age_wt > 0)
wt_age$weight <- as.numeric(wt_age$weight)

wt_sum <- wt_age %>% select("DOB", "SEX", "GENOTYPE", "date", "weight", "age_wt") %>% 
  filter(GENOTYPE != "NA", GENOTYPE != "?", weight != "NA", weight < 55) %>% 
  group_by(SEX, GENOTYPE, age_wt) %>% 
  summarise(n = n(), weight = mean(weight, na.rm = T))

wt_sum

### Faceted graph
(g1 <- ggplot(data = wt_sum, aes(x = age_wt, y = weight, color = GENOTYPE)) + 
    geom_line() + geom_point() + facet_wrap(~SEX, nrow = 2) + labs(x = "Age in Days", y = "Body weight in grams"))

ggsave("weight.png", width = 20, height = 20, units = "cm")

###  Save table to CSV file
write.csv(wt_age, "wt3.csv")
