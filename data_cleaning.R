library(RSQLite)
library(tidyverse)
library(readr)

billboard <- "raw-data/CopyOfbillboard-200.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = billboard)

# I don't really know how SQL files work. However, I installed an SQL
# package and used dbConnect() to access the data and download it. I set
# the dbname to billboard.

dbListTables(db)
mytable <- dbReadTable(db, "acoustic_features")

mytable2 <- dbReadTable(db, "albums")

# While I didn't really anticipate needing the 'albums' file, I saved
# both the acoustic features amd the album data into mytable and
# mytable2, respectively. I used the dbReadTable() command to do this.

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

# Here, I read in the Billboard chart data from the .csv file I had
# previously downloaded. Since it was giving me a weird message for the column
# specifications, I set them myself by copying and pasting the message as the input
# for col_types. I saved it as billboard.

betterDates1 <- as.Date(billboard$WeekID,
                        format = "%m/%d/%Y") 

billboard$WeekID <- betterDates1

billboard$date <- billboard$WeekID

billboard <- billboard %>%
  select(-WeekID)

# Here, since I knew I wanted to get the dates from the Billboard chart data
# into the same format as the audio attributes data to join the two, I used
# as.Date to convert the dates from each 'WeekID' entry to a  more standard
# format in the new 'date' column. After, I used select to remove the WeekID 
# column altogether, as it was no longer needed.

billboard <- billboard %>%
  select(Song, Performer, `Week Position`, Instance, `Previous Week Position`, `Peak Position`, `Weeks on Chart`, date)

# Here, I selected only the columns for the data that I thought would be relevant,
# which included all of the information about chart position for each song as well
# as information about the song title, performer, and date.

billboard$song <- billboard$Song

billboard <- billboard %>%
  select(-Song)

# I made the 'Song' column into a new 'song' column so it would be able
# to be joined with the audio features data from the other
# data set. I then used select() to get rid of the old column.

betterDates2 <- as.Date(mytable$date)

mytable$date <- betterDates2

mytable$album_date <- mytable$date

mytable <- mytable %>%
  select(-date)

# Here, I used as.Date() again to get the dates in a
# standard format before joining. I then saved 'date' back
# into the table as album_date and used select() to remove
# the old date column.

billboard$artist <- billboard$Performer

# I changed the "Performer" column from the billboard column into "artist"
# for joining with the audio features data.

combined <- mytable %>%
  full_join(billboard, by = c("song", "artist"))

# I used full_join() to join mytable and billboard,
# as I knew I wanted to attach the billboard data by both song and artist
# to ensure no songs with the same title were being paired to the wrong artists
# and full join as opposed to the others to still include data for songs
# that didn't chart on the Hot 100 but were still included in the Billboard 200 
# Albums acoustic features dataset.

write_rds(combined, "raw-data/combined.rds")

# I used write_rds() to write my new cleaned date into an .rds
# file so that I can continue to organize it from another file.
