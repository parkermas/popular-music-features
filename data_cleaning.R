library(RSQLite)
library(tidyverse)

billboard <- "raw-data/CopyOfbillboard-200.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = billboard)

dbListTables(db)
mytable <- dbReadTable(db, "acoustic_features")

mytable2 <- dbReadTable(db, "albums")


billboard <- read_csv("https://query.data.world/s/irbttf7p2dbspdoivd2mrdy67nosle", col_types = cols(
  url = col_character(),
  WeekID = col_character(),
  `Week Position` = col_double(),
  Song = col_character(),
  Performer = col_character(),
  SongID = col_character(),
  Instance = col_double(),
  `Previous Week Position` = col_double(),
  `Peak Position` = col_double(),
  `Weeks on Chart` = col_double()
))

betterDates1 <- as.Date(billboard$WeekID,
                        format = "%m/%d/%Y") 

billboard$WeekID <- betterDates1

billboard$date <- billboard$WeekID

billboard <- billboard %>%
  select(-WeekID)

billboard <- billboard %>%
  select(Song, Performer, `Week Position`, Instance, `Previous Week Position`, `Peak Position`, `Weeks on Chart`, date)

billboard$song <- billboard$Song

billboard <- billboard %>%
  select(-Song)

betterDates2 <- as.Date(mytable$date)

mytable$date <- betterDates2

mytable$album_date <- mytable$date

mytable <- mytable %>%
  select(-date)

billboard$artist <- billboard$Performer

combined <- mytable %>%
  full_join(billboard, by = c("song", "artist"))

combined <- combined %>%
  mutate(not_hot100 = is.na(Performer))

file.remove("raw-data/CopyOfbillboard-200.db")