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
  mutate(series = "Trucks")%>%
  filter(Total.Laps != 0)
####################################################################################################
## import xfinity data

xfin = read.csv("Xfinity2007_2020.csv",sep = ";")
S221x = read.csv("Xfinity21.csv", sep = ";")
s222x = read.csv("Xfinity2022.csv")
Updatedx = bind_rows(xfin, S221x, s222x)
Updatedx$Fastest.Lap.Pct = ((Updatedx$Fastest.Lap / Updatedx$Total.Laps) *100)
round(Updatedx$Fastest.Lap.Pct, digits = 2)
Updatedx$race_ID = paste(substr(Updatedx$raceID, 4, 7), "_", substr(Updatedx$raceID, 0, 2), sep = "")

xSchedule = read.csv("XfinitySchedule.csv")

xData <- merge(Updatedx, xSchedule)

xData <- xData %>%
  group_by(Driver, race_ID) %>%
  arrange(race_ID, Finish)%>%
  select(Driver, year, race, race_ID,Track,TrackType,Start,Finish, Avg..Pos.,Pass.Diff.,
         Pct..Quality.Passes,Pct..Top.15.Laps,Pct..Laps.Led,Fastest.Lap.Pct,
         Total.Laps,Driver.Rating)%>%
  mutate(series = "Xfinity")%>%
  filter(Total.Laps != 0)
#####################################################################################################
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
  mutate(series = "Cup")%>%
  filter(Total.Laps != 0)

####################################
all_series <- tData %>%
  bind_rows(CupData, xData)

###################

Track_Data <- all_series %>%
  group_by(Driver, year, series)%>%
  select(Driver, year, race_ID, Track, Finish, Avg..Pos., Driver.Rating)%>%
  filter(Track == "Phoenix")%>%
  select(Driver, year, race_ID, Track, Driver.Rating)%>%
  summarize(Driver.Rating = mean(Driver.Rating), Track = "Phoenix")

times_visited <- all_series %>%
  group_by(Track, race_ID, series)%>%
  summarize(race_ID = last(race_ID), Track = last(Track), series = last(series))%>%
  ungroup()%>%
  group_by(Track, series)%>%
  select(Track, series)%>%
  mutate(races = n())%>%
  summarize(Track = last(Track), races = last(races))

other_tracks <- all_series %>%
  group_by(Driver, year, series)%>%
  select(Driver, year, series, race_ID, Track, Finish, Avg..Pos., Driver.Rating)%>%
  left_join(Track_Data, by = c("Driver" = "Driver", "year" = "year", "series" = "series"))%>%
  left_join(times_visited, by = c("Track.x" = "Track", "series" = "series"))

track_correlations <- other_tracks %>%
  group_by(Track.x)%>%
  select(Driver, year, series, Track.x, Track.y, Driver.Rating.x, Driver.Rating.y)%>%
  filter(Track.x != "Phoenix")%>%
  filter(!is.na(Driver.Rating.y))

regression_table <- do(track_correlations, glance(lm(Driver.Rating.x ~ Driver.Rating.y, data = .)))%>%
  arrange(-r.squared)