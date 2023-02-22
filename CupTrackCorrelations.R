library(tidyverse)
library(purrr)
library(curl)
library(httr)
library(rvest)
library(ggplot2)
library(readxl)
library(tidymodels)
library(ranger)


Cup = read.csv("Cup2005_2020.csv",sep = ";")
RC21 = read.csv("Cup21.csv", sep = ";")
Updated = bind_rows(Cup, RC21)
Updated$Fastest.Lap.Pct = ((Updated$Fastest.Lap / Updated$Total.Laps) *100)
round(Updated$Fastest.Lap.Pct, digits = 2)
Updated$race_ID = paste(substr(Updated$raceID, 4, 7), "_", substr(Updated$raceID, 0, 2), sep = "")


Schedule = read.csv("CupSchedule.csv")

CupData <- merge(Updated, Schedule)

CupData <- CupData %>%
  group_by(Driver, race_ID) %>%
  arrange(race_ID, Finish)%>%
  select(Driver, year, race, race_ID,Track,TrackType,Start,Finish, Avg..Pos.,Pass.Diff.,
         Pct..Quality.Passes,Pct..Top.15.Laps,Pct..Laps.Led,Fastest.Lap.Pct,
         Total.Laps,Driver.Rating)%>%
  filter(Total.Laps != 0)

Sonoma <- CupData %>%
  group_by(Driver, year)%>%
  select(Driver, year, race_ID, Track, Finish, Avg..Pos., Driver.Rating)%>%
  filter(Track == "Darlington")%>%
  select(Driver, year, race_ID, Track, Driver.Rating)%>%
  summarize(Driver.Rating = mean(Driver.Rating), Track = "Darlington")

times_visited <- CupData %>%
  group_by(Track, race_ID)%>%
  summarize(race_ID = last(race_ID), Track = last(Track))%>%
  ungroup()%>%
  group_by(Track)%>%
  select(Track)%>%
  mutate(races = n())%>%
  summarize(Track = last(Track), races = last(races))

other_tracks <- CupData%>%
  group_by(Driver, year)%>%
  select(Driver, year, race_ID, Track, Finish, Avg..Pos., Driver.Rating)%>%
  left_join(Sonoma, by = c("Driver" = "Driver", "year" = "year"))%>%
  filter(year >= "2010")%>%
  left_join(times_visited, by = c("Track.x" = "Track"))%>%
  filter(races >= 3)

track_correlations <- other_tracks %>%
  group_by(Track.x)%>%
  select(Driver, year, Track.x, Track.y, Driver.Rating.x, Driver.Rating.y)%>%
  filter(Track.x != "Darlington")

regression_table <- do(track_correlations, glance(lm(Driver.Rating.x ~ Driver.Rating.y, data = .)))%>%
  arrange(-adj.r.squared)