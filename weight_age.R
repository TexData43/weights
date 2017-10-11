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

### create a vector and then bin
vec <- wt_age$age_wt
bin_x <- seq(0, 1530, 30)
vec2 <- .bincode(vec, bin_x, TRUE, TRUE)

wt_age["bin"] <- vec2

### start the summarize pipe, exclude unknown genotypes, NA weights or outliers (> 55)
### group by SEX, GENOTYPE, and by the age in days then calc the mean
wt_sum <- wt_age %>% 
  filter(GENOTYPE != "NA", GENOTYPE != "?", weight != "NA", weight < 55) %>% 
  group_by(SEX, GENOTYPE, bin) %>% 
  summarise(n = n(), weight = mean(weight, na.rm = T))

### view the end results
wt_sum

### Faceted graph by genotype and sex
(g1 <- ggplot(data = wt_sum, aes(x = bin, y = weight, color = GENOTYPE)) + 
    geom_point(size = 6, alpha = 0.2) + 
    geom_smooth(alpha = .5, size = 1.5) + 
    facet_wrap(~SEX, nrow = 2) + 
    labs(x = "Age in months", y = "Body weight in grams") + 
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)))
### save the plot
ggsave("weight_age.png", width = 30, height = 20, units = "cm")

###  Save table to CSV file
write.csv(wt_age, "wt_age.csv")
