library(tidyverse)
library(readr)
library(broom)
library(coefplot)
library(readr)

combined <- read_rds("raw-data/combined.rds")

# To start, I read in the .rds file that I 
# had previously created using another R script. I saved
# it as combined, which is what I had been calling it previously.

combined <- combined %>%
  mutate(Hot100 = case_when(is.na(Performer) ~ "No",
                            is_character(Performer) ~ "Yes"))

combined$Danceability <- combined$danceability

combined$Energy <- combined$energy

combined$Valence <- combined$valence

combined$Speechiness <- combined$speechiness

# Here, I made several edits to the combined dataset. I added
# a new column 'Hot100' using case_when() which indicated whether
# a given song had charted on the Billboard Hot 100 chart or not.
# I also changed the various audio attribute columns so that they
# were capitalized (so they are easier to use in my Shiny coding).

grouping1 <- combined %>% 
  group_by(Hot100) %>%
  filter(!duplicated(id))

# I wanted to make different groupings of data depending on what I was trying
# to analyze. I decided my first grouping would include both Hot 100 and non-Hot 100 
# songs so I could compare them. To do this, I grouped by Hot100 and then filtered out duplicated songs from each
# group based on their unique id number.

grouping1$date_year <- format(as.Date(grouping1$album_date, format="%d/%m/%Y"),"%Y")

grouping1$date_year <- as.double(grouping1$date_year)

# Here I created a new date_year column for grouping1 that gave
# only the year of the date and set the column type to double in case
# I had to analyze it like a number rather than a date at some point.

floor_decade <- function(value){ return(value - value %% 10) }

grouping1 <- grouping1 %>% 
  mutate(decade = floor_decade(date_year))

# I created the floor_decade function with some help from the internet,
# which returns the appropriate decade for a given date input. I then
# used this to create a new "decade" column so that I could analyze the data
# I had based on decade as well as year.

grouping2 <- combined %>%
  filter(Hot100 == "Yes") %>%
  filter(!duplicated(id))

# For my 2nd grouping, I wanted to analyze only songs that had charted on the
# Billboard Hot 100 chart, so I filtered out all songs that weren't and removed
# duplicates to preserve the accuracy of my analysis.

grouping2$date_year <- format(as.Date(grouping2$date, format="%d/%m/%Y"),"%Y")

grouping2$date_year <- as.double(grouping2$date_year)

grouping2 <- grouping2 %>% 
  mutate(decade = floor_decade(date_year))

# Here, I also created a date_year column and decade column
# for grouping 2 using the same techniques as I had for grouping 1.

decades <- grouping2 %>%
  distinct(decade) %>%
  filter(decade != "1950")

# I created a decades object which was a list of the decades for use in my shiny app.
# Because there weren't enough entries in the two years of data  from the 1950s, I decided
# to discard all data from that decade entirely and start my analysis in 1960.

grouping2$song <- as.factor(grouping2$song)

# Set the song column in grouping2 as a factor after I was getting an error 
# telling me it should be a factor column.

grouping3 <- combined %>% 
  filter(Hot100 == "Yes")

grouping3$date_year <- format(as.Date(grouping3$date, format="%d/%m/%Y"),"%Y")

grouping3$date_year <- as.double(grouping3$date_year)

# Created a grouping3 which has the same data as grouping2 but without duplicates
# removed. Because I wanted to look at how many weeks certain songs spent on the charts, each
# entry represents a week on the charts, I did not want each song to show up only once.

write_rds(decades, "Behind-Billboard/decades.rds")

write_rds(grouping1, "Behind-Billboard/grouping1.rds")

write_rds(grouping2, "Behind-Billboard/grouping2.rds")

write_rds(grouping3, "Behind-Billboard/grouping3.rds")

# I used write_rds to write all my data tables to .rds files within my Shiny
# app so the data could easily be read into the Shiny app.
