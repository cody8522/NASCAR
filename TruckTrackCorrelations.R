library(tidyverse)
library(purrr)
library(curl)
library(httr)
library(rvest)
library(ggplot2)
library(readxl)
library(tidymodels)
library(ranger)


truck = read.csv("Trucks2007_2020.csv",sep = ";")
RC21t = read.csv("Truck21.csv", sep = ";")
Updatedt = bind_rows(truck, RC21t)
Updatedt$Fastest.Lap.Pct = ((Updatedt$Fastest.Lap / Updatedt$Total.Laps) *100)
round(Updatedt$Fastest.Lap.Pct, digits = 2)
Updatedt$race_ID = paste(substr(Updatedt$raceID, 4, 7), "_", substr(Updatedt$raceID, 0, 2), sep = "")


tSchedule = read.csv("TruckSchedule.csv")

tData <- merge(Updatedt, tSchedule)

tData <- tData %>%
  group_by(Driver, race_ID) %>%
  arrange(race_ID, Finish)%>%
  select(Driver, year, race, race_ID,Track,TrackType,Start,Finish, Avg..Pos.,Pass.Diff.,
         Pct..Quality.Passes,Pct..Top.15.Laps,Pct..Laps.Led,Fastest.Lap.Pct,
         Total.Laps,Driver.Rating)%>%
  filter(Total.Laps != 0)

gateway <- tData %>%
  group_by(Driver, year)%>%
  select(Driver, year, race_ID, Track, Finish, Avg..Pos., Driver.Rating)%>%
  filter(Track == "Nashville")

times_visited <- tData %>%
  group_by(Track, race_ID)%>%
  summarize(race_ID = last(race_ID), Track = last(Track))%>%
  ungroup()%>%
  group_by(Track)%>%
  select(Track)%>%
  mutate(races = n())%>%
  summarize(Track = last(Track), races = last(races))

other_tracks <- tData%>%
  group_by(Driver, year)%>%
  select(Driver, year, race_ID, Track, Finish, Avg..Pos., Driver.Rating)%>%
  left_join(gateway, by = c("Driver" = "Driver", "year" = "year"))%>%
  filter(!is.na(Finish.y))%>%
  left_join(times_visited, by = c("Track.x" = "Track"))%>%
  filter(races >= 3)

track_correlations <- other_tracks %>%
  group_by(Track.x)%>%
  select(Driver, year, Track.x, Track.y, Driver.Rating.x, Driver.Rating.y)

regression_table <- do(track_correlations, glance(lm(Driver.Rating.x ~ Driver.Rating.y, data = .)))%>%
  arrange(-adj.r.squared)