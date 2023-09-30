library(readr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ggforce)
library(rockthemes)

formatted <- read_csv("ratings/formatted.csv")
ratings <- read_csv("ratings/ratings.csv")

ratings2 <- ratings %>%
  mutate(week = week(`Date Rated`),
         wday = wday(`Date Rated`, label = TRUE),
         year = year(`Date Rated`),
         month = month(`Date Rated`, label = TRUE))

cal.pivot <- ratings2 %>%
  filter(`Title Type` != "tvSeries") %>%
  group_by(year, week) %>%
  reframe(
    ratings = n(),
    shorts = sum(ifelse(`Runtime (mins)` < 60, 1, 0), na.rm=T),
    features = sum(ifelse(`Runtime (mins)` < 60, 0, 1), na.rm=T)
  ) %>%
  mutate(features = ifelse(features > 42, 42, features)) %>%
  arrange(desc(year), desc(week)) %>%
  left_join(
    ratings2 %>%
    filter(`Title Type` != "tvSeries") %>%
    count(year, week, wday) %>%
    mutate(n = ifelse(n > 6, 6, n)) %>%
    spread(wday, n),
    by = join_by(year, week))

p<-
  ggplot(cal.pivot,aes(week,year,fill=log(features+shorts))) +
  geom_tile(size=1,color="white") +
  scale_fill_gradient(low = "#86ebc9",
                      high = "#09855c") +
  coord_equal() +
  theme_void() +
  theme(legend.position="none")
p

