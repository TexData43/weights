library(tidyverse)
library(lubridate)

### read master weight file into R
weights <- read_csv("share_weight.csv")

### Gather the data from columns into a single row for tidy data
wt_gather <- weights %>% 
    gather(date, weight, 5:117)

### Convert the dates into machine-readable month-day-year dates
wt_gather$date <- mdy(wt_gather$date)
wt_gather$DOB <- mdy(wt_gather$DOB)

### Calculate the difference in days between the date of birth (DOB)
### and the specific date the weight was taken
### convert weight to numeric instead of character
wt_age <- wt_gather %>% 
  mutate(age_wt = date - DOB,
         weight = as.numeric(weight)) %>% 
  filter(age_wt > 0)

#wt_age$weight <- as.numeric(wt_age$weight)

### create a vector and then bin
age_vec <- wt_age$age_wt

bin_x <- seq(0, 1530, 30)

bin_vec <- .bincode(age_vec, bin_x, TRUE, TRUE)

wt_age["bin"] <- bin_vec

### start the summarize pipe, exclude unknown genotypes, NA weights or outliers (> 55)
### group by SEX, GENOTYPE, and by the age in days then calc the mean

wt_sum <- wt_age %>% 
    filter(GENOTYPE != "NA", GENOTYPE != "?", weight != "NA", weight < 55) %>%
    mutate(SEX = factor(SEX, levels = c("male", "female")),
           GENOTYPE = factor(GENOTYPE, levels = c("wt", "het", "ko"))) %>% 
    group_by(SEX, GENOTYPE, bin) %>% 
    summarise(n = n(), 
              weight = mean(weight, na.rm = T))

### view the end results
wt_sum

### Faceted graph by genotype and sex
(g1 <- ggplot(data = wt_sum, aes(x = bin, y = weight, fill = GENOTYPE, group = GENOTYPE)) + 
        geom_line(size = 2) +
        geom_point(shape = 21, size = 8, color = "black") + 
        #geom_smooth(alpha = .5, size = 1.5) + 
        facet_grid(~SEX) + 
        scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
        theme_bw() +
        scale_fill_manual(values = c("white", "grey", "black")) +
        theme(legend.position = c(0.03, .92),
              legend.title = element_blank(),
              axis.title = element_text(size = 40),
              legend.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.text = element_text(size = 30),
              legend.text = element_text(size = 20),
              strip.text.x = element_text(size = 30)) +
        labs(x = "\nAGE (MONTHS)", 
             y = "BODY WEIGHT (G)\n"))

### save the plot
ggsave("weight_age.png", g1, width = 20, height = 10, units = "in", dpi = 600)

### Faceted graph by genotype and sex

head(wt_sum)

# recreating a dataset from a graph
forster <- tribble(
    ~SEX,  ~bin,  ~GENOTYPE,  ~weight,  
    "male",   3 , "wt",   27,
    "male",   5 , "wt",   29,
    "male",   7,  "wt",   32,
    "male",   9, "wt",    34,
    "male",   11, "wt",   36,
    "male",   13, "wt",   37.5,
    "male",   15, "wt",   38,
    "male",   17 , "wt",  37,
    "male",   3 , "ko",   28,
    "male",   5 , "ko",   25,
    "male",   7, "ko",    24,    
    "male",   9, "ko",    23,
    "male",   11, "ko",   23.5,
    "male",   13, "ko",   24.5,
    "male",   15, "ko",   25,
    "male",   17 , "ko",  24
)

forster <- forster %>% 
    mutate(SEX = factor(SEX),
           GENOTYPE = factor(GENOTYPE, levels = c("wt", "ko")))

(g2 <- ggplot() + 
        geom_line(data = forster, 
                  aes(x = bin, y = weight, group = GENOTYPE), 
                  size = 2, color = "grey", alpha = 0.8) +
        geom_line(data = filter(wt_sum, SEX == "male" & GENOTYPE == "wt"), 
                  aes(x = bin, y = weight), 
                  size = 2, color = "black") +
        geom_point(data = filter(wt_sum, SEX == "male" & GENOTYPE == "ko"), 
                   aes(x = bin, y = weight), 
                   size = 6, color = "black") + 
        #scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40)) +
        theme_minimal() +
        scale_fill_manual(values = c("white", "grey", "black")) +
        theme(legend.position = c(0.03, .92),
              legend.title = element_blank(),
              axis.title = element_text(size = 40),
              legend.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.text = element_text(size = 30),
              legend.text = element_text(size = 20),
              strip.text.x = element_text(size = 30),
              axis.line = element_line(colour = 'black', size = 2),
              axis.ticks = element_line(colour = "black", size = 2)
              ) +
        labs(x = "\nAGE (MONTHS)", 
             y = "BODY WEIGHT (G)\n") +
        ylim(10, 40) +
        xlim(0, 30)
    )

# unused code for making transparent graphs
#png('wt_gt.png',width=1000,height=1000,units="px",bg = "transparent")
#print(g2)
#dev.off()
#panel.background = element_rect(fill = "transparent",colour = NA),
#plot.background = element_rect(fill = "transparent",colour = NA)

### save the plot at high resolution
ggsave("weight_genotype.png", g2, width = 10, height = 10, units = "in", dpi = 600)


###  Save table to CSV file
#write.csv(wt_age, "wt_age.csv")
