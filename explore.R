library(tidyverse)
library(lubridate)

dat <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>% 
    rename(Province = `Province/State`,
           Country = `Country/Region`) %>% 
    pivot_longer(-c(Province:Long), names_to = "Date", values_to = "Cases") %>% 
    select(Country, Province, Date, Cases) %>% 
    mutate(Date = mdy(Date)) %>% 
    print()

dat2 <- dat %>% 
    group_by(Country, Date) %>% 
    summarise(Cases = sum(Cases, na.rm = TRUE)) %>% 
    print()

country <- c("Austria", "France", "Germany", "Italy", "Spain", "US")
dat2 %>% 
    filter(Country %in% country) %>% 
    filter(Cases > 0) %>% 
    ggplot(aes(x = Date, y = Cases, col = Country)) +
    geom_line(size = 1, alpha = 1) +
    scale_y_log10("Cumulative cases") +
    scale_x_date(breaks = "weeks") +
    scale_color_brewer(type = "qual") +
    theme_gray(base_size = 20) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90))

dat2 %>% filter(Country == "Taiwan*") %>% print(n = 100)
