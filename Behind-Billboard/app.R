#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinythemes)
library(ggplot2)
library(coefplot)
library(shiny)
library(plotly)
library(readr)
library(gt)

grouping1 <- read_rds("grouping1.rds")
grouping2 <- read_rds("grouping2.rds")
grouping3 <- read_rds("grouping3.rds")
decades <- read_rds("decades.rds")



ui <- navbarPage(theme = shinytheme("flatly"),"Behind Billboard: Exploring the Audio Features of Popular Music",
                 tabPanel(
                     title = "Artist and Song Data",
                     h3("Exploring the Popularity and Audio Features of Pop Music"),
                     tabsetPanel(
                         tabPanel(
                             h6("Artist & Song Popularity"),
                             br(),
                             sidebarPanel(
                                 sliderInput("year3",
                                             "Year", min = 1960, max = 2018, value = 2018, animate = TRUE),
                                 radioButtons("artist_song",
                                              "Song/Artist", c("Song", "Artist"))
                                 
                             ),
                             mainPanel(
                                 plotlyOutput("popularityPlotly")
                             ),
                             br(),
                             tags$div("The above plot shows the most popular songs or artists (based on viewer selection) for each year during 1960-2018. 
                                      For songs, their popularity is represented here by their number of weeks they spent on the Billboard Hot 100 chart for the given year,
                                      and for artists, popularity is measured by the number of their songs which charted during the given year."),
                             br(),
                             tags$div("Do you notice any songs or artists you recognize in the graph? Are there any songs or artists which you think are surprising? Do the most popular songs seem to be by the most popular artists?")
                             
                             
                             
                         ),
                         tabPanel(
                             h6("Artists with Extreme Mean Features"),
                             br(),
                             sidebarPanel(
                                 sliderInput("year",
                                             "Year", min = 1960, max = 2018, value = 2018, animate = TRUE),
                                 selectInput("attribute2",
                                             "Feature:", c("Danceability", "Energy", "Valence", "Speechiness"))
                                 ,
                                 radioButtons("most_least1",
                                              "Highest/Lowest Extreme Features", c("Highest", "Lowest"))
                             ),
                             br(),
                             br(),
                             mainPanel(
                                 gt_output("artistsGt"))),
                         
                         tabPanel(
                             h6("Songs with Extreme Features"),
                             br(),
                             sidebarPanel(
                                 sliderInput("year2",
                                             "Year", min = 1960, max = 2018, value = 2018, animate = TRUE),
                                 selectInput("attribute6",
                                             "Feature:", c("Danceability", "Energy", "Valence", "Speechiness")),
                                 radioButtons("most_least2",
                                              "Highest/Lowest Extreme Features", c("Highest", "Lowest"))
                             ),
                             br(),
                             br(),
                             mainPanel(
                                 gt_output("songsGt"))),
                         
                         tabPanel(
                             h6("Search By Artist"),
                             br(),
                             br(),
                             sidebarPanel(
                                 textInput("name",
                                           "Artist Name:"),
                                 selectInput("attribute3",
                                             "Feature:", c("Danceability", "Energy", "Valence", "Speechiness")),
                                 radioButtons("most_least3",
                                              "Most/Least Popular Songs", c("Most", "Least")),
                                 p("Type in the name of an artist, select a feature, and choose whether to see the artist's 10 most or least popular songs based on peak chart position.")
                             ),
                             br(),
                             mainPanel(
                                 plotlyOutput("artistdata")),
                             br(),
                             tags$div("Note: Songs are arranged in the graph from most to least popular (left to right), both for the artist's ten most and ten least popular songs."),
                         )
                     )),
                 tabPanel("Audio Features Over Time",
                          h3("Visualizing Changes in Audio Features"),
                    tabsetPanel(
                        tabPanel(
                    h6("Audio Features By Year"),
                    sidebarLayout(
                    sidebarPanel(
                        radioButtons("attribute",
                                     "Feature:", c("Danceability", "Energy", "Valence", "Speechiness"))
                                          ),
                    mainPanel(
                        plotlyOutput("chartingPlotly"))),
                    br(),
                    tags$div("Grouped by year, the plot shows the mean for a given feature over the span of a year for both songs that were on the Hot 100 charts and those that weren't."),
                    br(),
                    tags$div("Based on these visualizations, we can see clearly that there are differences in the features Hot 100 songs vs. non-Hot 100 songs, and a significant variation in some of the attributes over time that align with real-world trends."),
                    br(),
                    tags$div("For example, perhaps the spike in mean 'Speechiness' in Hot 100 songs for 2018 makes sense given that rap music was more popular than rock music as a genre for the first time in the United States.")
                    ),
                    tabPanel(
                        h6("Audio Features By Decade"),
                        sidebarLayout(
                            sidebarPanel(radioButtons("attribute5",
                                                       "Feature:", c("Danceability", "Energy", "Valence", "Speechiness"))),
                    
                    mainPanel(
                        plotlyOutput("decadesPlotly"),
                        
                    )
                    )
                    ,
                    br(),
                    tags$div("Grouped by decade, the plot shows the mean for a given feature over the span of a decade for both songs that were on the Hot 100 charts and those that weren't."),
                    br(),
                    tags$div("Based on these visualizations, we can see clearly that there are differences in the features Hot 100 songs vs. non-Hot 100 songs, and a significant variation in some of the attributes over time that align with real-world trends."),
                    br(),
                    tags$div("For example, the steady rise in mean 'Energy' with every subsequent decade through 2000 seems to correlate with the rise of electronic sounds in music and the dominance of electronic sounds in all genres of popular music today."),
                    
                    ))),

                 tabPanel(
                     title = "Predictive Model",
                     h3("What Makes a Popular Song? How Audio Features Predict Peak Chart Position"),
                     br(),
                     sidebarPanel(
                         selectInput("decade",
                                     "Decade:", c("1960", "1970", "1980", "1990", "2000", "2010")),
                         p("Select a decade from the drop-down menu to run a linear regression on the data from that decade. The plot represents the coefficients for the variables included in the model."),
                     ),
                     mainPanel(
                         plotlyOutput("peakPlotly")),
                     br(),
                     tags$div("To explore the relationship between various audio features of music and the peak popularity of Billboard Hot 100 songs, I ran a multiple linear regression that uses the audio features of Danceability, Energy, Valence, and Speechiness to explore the effect on peak chart position of any given song for each decade. The results are displayed in the above interactive coefficient plot."),
                     br(), 
                     tags$div("Originally, I had planned to run a regression on the entire set of data, but given the enormous amount of change in popular music from 1960-2018 and within each decade alone, it seemed more appropriate to run a regression on each decade's worth of data instead.
                              Because of this, interesting conclusions can be drawn from correlations between audio features and popularity for each decade, with the 2010 model giving the best representation of the current pop music environment today."),
                     br(),
                     tags$div("Each 'value' (scroll over the point on the plot to see) represents the effect that a one-unit increase of the feature in a song would have on its chart position. While lower numbers traditionally represent higher chart position in commercial music trackers, I reverse coded the data so that a +5 value in the plot equals moving 5 spots up on the Billboard Hot 100 chart." ),
                     br(),
                     tags$div("For example, analysis of the model for the 2010s yields some interesting results. The intercept is ~ +23, indicating where the model predicts
                              a song with 0 of everything to be on the charts (this actually translates to #77, but the reverse coding complicates the analysis). The coefficient with the highest absolute value - 'Danceability' (~ +29) - indicates the most strongly positive correlation between an'Danceability' and peak chart position. It seems highly probable that songs which are better to dance to based on the 'Danceability' metric would tend to chart better.
                              The coefficient with the lowest absolute value - 'Valence' (~ -1) - shows a weak negative correlation between valence and peak position, possibly suggesting that happy-sounding songs tend to do slightly worse than sad-sounding songs. The positive correlation between 'Speechiness' (~ +4) and peak position may suggest that songs with rapping tend to have a higher peak position. Finally, 
                              the negative correlation between 'Energy' and peak position (~ -5) seems to suggest that high energy songs tend to do worse than songs with a more mellow feel. Note that causality is not necessarily implied, and this is a simplistic model. All analysis does not presume causality and simply notes the interesting correlations found in the data.")
                 ),                   
                         
                         
                  tabPanel(
                      title = "About",
                      h3("About the Project"),
                      tags$div("Music is changing. In recent years, both", a(href = "https://www.npr.org/sections/therecord/2015/06/01/411119372/how-streaming-is-changing-music", "technology"), "and",
                               a(href = "https://www.independent.co.uk/arts-entertainment/music/news/rap-music-rock-most-popular-genre-us-nielsen-music-report-2017-kendrick-lamar-ed-sheeran-drake-top-a8141086.html", "culture"),
                               "have proven to be powerful forces driving change in the music industry, sparking increased discussion about the evolution of popular music and interest in what features make a song popular.
                               In this project, I examine audio and chart data from American popular music from the past five decades, including a number of visualizations that illuminate interesting patterns in the data as well as a predictive model analysis
                               of the variables that drive popularity of a song for each decade."),
                      h3("Data Sources"),
                      tags$div("The first set of data I used comes from Sean Miller at",
                               a(href = "https://data.world/kcmillersean/billboard-hot-100-1958-2017", "data.world,"),
                               "which includes every entry on the Billboard Hot 100 weekly charts from 08/02/1958 to 06/22/2019 as well as descriptive attributes including song name,
                               artist, and information about their chart position and history."),
                      br(),
                      tags$div("The second set of data I used comes from Andrew Thompson at", 
                               a(href = "https://components.one/datasets/billboard-200/", "components.one,"),
                               "an experimental data publication, and it includes", a(href = "https://developer.spotify.com/documentation/web-api/", "Spotify"), 
                               "acoustic data for tracks from Billboard 200 albums from 01/05/1953 to 01/19/2019. Features recorded include danceability, energy, valence, and speechiness, among others.
                               Information about the artist, album, and date were also included in the dataset."),
                      br(),
                      tags$div("In cleaning the data, I combined the two datasets in order to be able to compare Hot 100 songs included in the Billboard 200 albums with the songs that were not Hot 100 but still in the Billboard 200 albums (representing the most popular songs as opposed to many songs which were all relatively successful but not of the same popularity).
                               I also chose to limit the dates from 1960-2018 in order to make sure I had no years with incomplete data and enough entries per decade to create an model with. Check out my", a(href = "https://github.com/parkermas/popular-music-features", "Github code"), "here."),
                      h3("Audio Features"),
                      tags$div("Danceability - describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."),
                      br(),
                      tags$div("Energy - Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy."),
                      br(),
                      tags$div("Valence - A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)."),
                      br(),
                      tags$div("Speechiness - Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks. "),
                      h3("About Me"),
                      tags$div("My name is Parker Mas and I am currently a sophomore at Harvard College studying Government with interests in American politics and data science.
                               Check out my", a(href = "https://github.com/parkermas", "Github"), "here!")
                  ))
                 
                     
          
server <- function(input, output) {
    output$peakPlotly <- renderPlotly({
        group_decade <- grouping2 %>% 
            filter(decade == input$decade)
        peak <- lm(`Peak Position` ~ Danceability + Energy + Speechiness + Valence, data = group_decade) %>%
            coefplot.lm(color = "black", 
                        pointSize = 1.8, 
                        alpha = .7,
                        xlab = "Effect on Peak Position",
                        ylab = "Coefficient") +
            labs(title = "Audio Features as Predictors of Peak Position by Decade") +
            theme(plot.title = element_text(hjust = 0.5))
        
    })
    
    
    output$chartingPlotly <- renderPlotly({
        charting <- grouping1 %>%
            filter(date_year >= "1960") %>%
            filter(date_year < "2019") %>%
            group_by(Hot100, date_year) %>%
            summarize(mean_attribute = mean(get(input$attribute), na.rm = TRUE)) %>%
            ungroup() %>%
            ggplot(aes(group = Hot100, x = date_year, y = mean_attribute)) +
            geom_line(aes(color = Hot100)) +
            geom_point(aes(color = Hot100)) +
            labs(title = "Change in Mean Audio Feature Over Time, 1960-2018",
                 subtitle = "Billboard Hot 100 and non-Hot 100 Charting Songs",
                 x = "Year",
                 y = "Mean Value of the Selected Feature")
    })
    
    output$decadesPlotly <- renderPlotly({
        decades <- grouping1 %>%
            filter(date_year >= "1960") %>%
            group_by(decade, Hot100) %>%
            summarize(mean_attribute = mean(get(input$attribute5), na.rm = TRUE)) %>%
            ggplot(aes(fill = Hot100, x = decade, y = mean_attribute)) +
            geom_col(position = "dodge") +
            labs(title = "Mean Audio Feature by Decade, 1960-2010",
                 subtitle = "Billboard Hot 100 and non-Hot 100 Charting Songs",
                 x = "Decade",
                 y = "Mean Value of Selected Feature")
            
    })
        
    output$popularityPlotly <- renderPlotly({
        popularityPlotly <- popularityPlotly_eventReactive()
    })
    
    popularityPlotly_eventReactive <- eventReactive(c(input$year3, input$artist_song),
                                                    {
                                                        if (input$artist_song == "Artist") {
                                                            popularityPlotly_eventReactive <- grouping2 %>%
                                                                filter(date_year == input$year3) %>%
                                                                distinct(song, .keep_all = TRUE) %>%
                                                                group_by(artist) %>%
                                                                count() %>%
                                                                arrange(desc(n)) %>%
                                                                head(30) %>%
                                                                select(n, artist) %>%
                                                                ggplot(aes(x = reorder(artist, +n), y = n)) +
                                                                geom_col() +
                                                                coord_flip() +
                                                                labs(title = "30 Most Popular Artists",
                                                                     subtitle = "Based on Billboard Hot 100 Data",
                                                                     x = "Artist Name",
                                                                     y = "Number of Songs on Hot 100 Chart")
                                                        }
                                                        else {
                                                            popularityPlotly_eventReactive <- grouping3 %>%
                                                                filter(date_year == input$year3) %>%
                                                                count(song) %>%
                                                                arrange(desc(n)) %>%
                                                                head(30) %>%
                                                                select(n, song) %>%
                                                                ggplot(aes(x = reorder(song, +n), y = n)) +
                                                                geom_col() +
                                                                coord_flip() +
                                                                labs(title ="30 Most Popular Songs",
                                                                     subtitle = "Based on Billboard Hot 100 Data",
                                                                     x = "Song Name",
                                                                     y = "Number of Weeks on Hot 100 Chart")
                                                        }
                                                    })
    
    output$artistsGt <- render_gt({
        artistsGt <- artistsGt_eventReactive()
    })
    
    artistsGt_eventReactive <- eventReactive(c(input$attribute2, input$year, input$most_least1),
                                             {
                                                 if (input$most_least1 == "Highest") {
                                                 artistsGt_eventReactive <- grouping2 %>%
                                                     filter(date_year == input$year) %>%
                                                     distinct(song, .keep_all = TRUE) %>%
                                                     group_by(artist) %>%
                                                     summarize(mean_attribute = mean(get(input$attribute2), na.rm = TRUE)) %>%
                                                     ungroup() %>%
                                                     arrange(desc(mean_attribute)) %>%
                                                     head(10) %>%
                                                     select(artist) %>%
                                                     gt() %>% 
                                                     tab_header(title = "Top 10 Artists for Highest Mean Feature") %>%
                                                     cols_label(artist = "") %>%
                                                     tab_source_note(source_note = "Note: Mean features calculated from Billboard Hot 100 acoustic data.")
                                                 }
                                                 else {
                                                     artsitsGt_eventReactive <- grouping2 %>%
                                                         filter(date_year == input$year) %>%
                                                         distinct(song, .keep_all = TRUE) %>%
                                                         group_by(artist) %>%
                                                         summarize(mean_attribute = mean(get(input$attribute2), na.rm = TRUE)) %>%
                                                         ungroup() %>%
                                                         arrange(mean_attribute) %>%
                                                         head(10) %>%
                                                         select(artist) %>%
                                                         gt() %>% 
                                                         tab_header(title = "Top 10 Artists for Lowest Mean Feature") %>%
                                                         cols_label(artist = "") %>%
                                                         tab_source_note(source_note = "Note: Mean features calculated from Billboard Hot 100 acoustic data.")
                                                 }
                                                
                                             })
    
    output$songsGt <- render_gt({
        songsGt <- songsGt_eventReactive()
    })
    
    songsGt_eventReactive <- eventReactive(c(input$attribute6, input$year2, input$most_least2),
                                           {
                                               if (input$most_least2 == "Highest") {
                                                   grouping2 %>%
                                                        filter(date_year == input$year2) %>%
                                                        distinct(song, .keep_all = TRUE) %>%
                                                        group_by(song) %>%
                                                        mutate(mean_attribute = mean(get(input$attribute6), na.rm = TRUE)) %>%
                                                        ungroup() %>%
                                                        arrange(desc(mean_attribute)) %>%
                                                        select(song, artist) %>%
                                                        head(10) %>%
                                                        gt() %>% 
                                                        tab_header(title = "Top 10 Songs for Highest Feature") %>%
                                                        cols_label(song = "Song",
                                                              artist = "Artist") %>%
                                                       tab_source_note(source_note = "Note: Mean features calculated from Billboard Hot 100 acoustic data.")
                                               }
                                               else {
                                                   grouping2 %>%
                                                       filter(date_year == input$year2) %>%
                                                       distinct(song, .keep_all = TRUE) %>%
                                                       group_by(song) %>%
                                                       mutate(mean_attribute = mean(get(input$attribute6), na.rm = TRUE)) %>%
                                                       ungroup() %>%
                                                       arrange(mean_attribute) %>%
                                                       select(song, artist) %>%
                                                       head(10) %>%
                                                       gt() %>% 
                                                       tab_header(title = "Top 10 Songs for Lowest Feature") %>%
                                                       cols_label(song = "Song",
                                                                  artist = "Artist")%>%
                                                       tab_source_note(source_note = "Note: Mean features calculated from Billboard Hot 100 acoustic data.")
                                               }
                                           })

    output$artistdata <- renderPlotly({
        artistdata <- artistdata_eventReactive()
        
    })
    
    artistdata_eventReactive <- eventReactive(c(input$name, input$attribute3, input$most_least3),
                                              {
                                                  if (input$most_least3 == "Most") {
                                                  grouping2 %>%
                                                      filter(artist == input$name) %>%
                                                      distinct(song, .keep_all = TRUE) %>%
                                                      arrange(desc(`Peak Position`)) %>%
                                                      head(10) %>%
                                                      ggplot(aes(x = reorder(song, -`Peak Position`), y = get(input$attribute3))) +
                                                      geom_col(fill = "royalblue4") +
                                                      labs(title = "Audio Attributes of Most Popular 10 Songs for a Given Artist",
                                                           subtitle = "Songs in descending order based on peak chart position",
                                                           x = "Song Title",
                                                           y = "Attribute Value") +
                                                      theme(axis.text.x = element_text(angle = 45, hjust = 1))
                                                  }
                                                  else {
                                                  grouping2 %>%
                                                      filter(artist == input$name) %>%
                                                      distinct(song, .keep_all = TRUE) %>%
                                                      arrange(`Peak Position`) %>%
                                                      head(10) %>%
                                                      ggplot(aes(x = reorder(song, -`Peak Position`), y = get(input$attribute3))) +
                                                      geom_col(fill = "royalblue4") +
                                                      labs(title = "Audio Attributes of Least Popular 10 Songs for a Given Artist",
                                                           subtitle = "Songs in descending order based on peak chart position",
                                                           x = "Song Title",
                                                           y = "Attribute Value") +
                                                      theme(axis.text.x = element_text(angle = 45, hjust = 1))
                                                  }
                                              })
        
    
    
}
shinyApp(ui = ui, server = server)
