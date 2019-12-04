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

# Loaded all the packages that I thought I would need for the Shiny app.
# Since I planned on making several tables and a coefficient plot, coefplot
# and gt were especially necessary.

grouping1 <- read_rds("grouping1.rds")
grouping2 <- read_rds("grouping2.rds")
grouping3 <- read_rds("grouping3.rds")
decades <- read_rds("decades.rds")

# I read in all of my .rds files here, at the start, as I knew
# they would all be used throughout the code in my Shiny ui and server,
# as they have all the data being analyzed in my project.

ui <- navbarPage(theme = shinytheme("flatly"),
                 
                 # Added a theme "flatly" for the Shiny app to give it some aesthetic
                 # flair.
                 
                 "Behind Billboard: Exploring the Audio Features of Popular Music",
                 
                 # Gave my Shiny app an interesting title
                 
                 tabPanel(
                     title = "Artist and Song Data",
                     h3("Exploring the Popularity and Audio Features of Pop Music"),
                     tabsetPanel(
                         tabPanel(
                             h6("Artist & Song Popularity"),
                             br(),
                             
                             # I added both a slider panel, for the viewer to change the year, and radio buttons,
                             # for the viewer to select what they wanted to see visualized (artist/song). I also kept the animate feature
                             # so they they could choose to watch the animation if they so chose.
                             
                             sidebarPanel(
                                 sliderInput("year3",
                                             "Year", min = 1960, max = 2018, value = 2018, animate = TRUE),
                                 radioButtons("artist_song",
                                              "Song/Artist", c("Song", "Artist"))
                                 
                                 # Set my main panel to output the graphic that shows the most popular artists/songs based on
                                 # the server code. Added text below to help contextualize the graphic and provide more detail
                                 # for the viewer.
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
                         
                         # Here I added a tab that will allow the viewer to select the year using a slider,
                         # the attribute using a drop-down menu, and radio buttons to select whether they wanted to
                         # see the artists with the highest or lowest mean features for the attribute and year.
                         
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
                             
                             # Set the main panel to output the gt table that I coded in the server.
                             
                             mainPanel(
                                 gt_output("artistsGt"))),
                         
                         # This panel is basically the same thing as the extreme features tab for the artists, but for
                         # individual songs instead. The same inputs are presented and the output will basically be the exact same.
                         # More detail can be found in the server code.
                         
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
                         
                         # I also wanted to have a tab where the viewer could search for say, their favorite rapper. Here
                         # I elected to create a tab where there is a text input box. I also added a drop-down menu and radio buttons
                         # so the viewer could choose which attribute they would like to look at and whether they wanted to see the
                         # artist's most or least popular songs.
                        
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
                             
                             # The main panel outputs a bar graph that changes based on the input.
                             
                             mainPanel(
                                 plotlyOutput("artistdata")),
                             br(),
                             tags$div("Note: Songs are arranged in the graph from most to least popular (left to right), both for the artist's ten most and ten least popular songs."),
                         )
                     )),
                 
                 # For the audio features over time panel, I could keep things a little simpler. I only had two tabs, one with
                 # a visualization of audio features over time by year and another one by decade. By using radio buttons,
                 # a viewer can select which attribute they would like to see visualized, and by switching the tabs, they can see
                 # both years and decades visualized.
                 
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
                    
                    # The output will be a line graph that differentiates between Hot 100 and non-Hot 100 means.
                    
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
                    
                     # Here the output is a double bar graph instead of a line plot, as I thought these better represented
                     # decades than a line graph would. The colors differentiate Hot 100 versus non-Hot 100 means.
                            
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

                 # The following is the panel that has the multivariate linear regression I ran on my data. The viewer can select
                 # from a drop-down menu which decade's worth of data they would like to see the model run on, and the resulting coefficient
                 # plot will correspond to that decade. I also added a paragraph of extra instruction for the viewer in case it was unclear what the
                 # drop down menu was for.
                 
                 tabPanel(
                     title = "Predictive Model",
                     h3("What Makes a Popular Song? How Audio Features Predict Peak Chart Position"),
                     br(),
                     sidebarPanel(
                         selectInput("decade",
                                     "Decade:", c("1960", "1970", "1980", "1990", "2000", "2010")),
                         p("Select a decade from the drop-down menu to run a linear regression on the data from that decade. The plot represents the coefficients for the variables included in the model."),
                     ),
                     
                     # The output here is a coefficient plot created using the coefplot package. Additionally,
                     # I added a decent amount of text below that explains exactly what the model is and some of the 
                     # conclusions that could be drawn based on the 2010 decade model.
                     
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
                         
                  The final panel is my About page, where I give information about what this project is generally,
                 where I got my data and what it represents, an explaination of each of the audio features of the music that
                 is referenced in this app. Finally, I included in about me page as per the instructions. I also figured out how how
                 to add links to text and added links to jouralistic sources, the sources for my data, and my Github repo, as appropriate
                        
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
                 
                     
          # Below is the code for my server. This is where most of the actual R code is actually run, 
          # as most of the UI code has to do with specifying HOW the inputs and outputs are presented 
          # as opposed to the code that tells the Shiny app WHAT to display.


server <- function(input, output) {
    
    # The first output I created was my coefficient plot based on the model I
    # ran for each decade. I figured out how I could use the input$decade term so
    # that the output of my coefficient plot would correspond to whatever the viewer
    # selected as the input. I also edited the aesthetics of the coefficient plot along
    # the lines of what Sascha suggested and changed my linear regression to not show the interactions
    # of the variables, as these would probably be too complicated for the average viewer and thus unhelpful.
    # I also added an interesting title and axis titles as appropriate.
    
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
    
    # Here is where I created the line graph that shows the changes in a given attribute based
    # on the input year and attribute using the slider and drop down menu. Using get(input$attribute),
    # which draws the column data rather than just using the literal text of input as the code. I had to
    # group by Hot100 and year to get the data the way I wanted it, and I also made sure to add different
    # colored lines for the plot as well as a title and axis titles.
    
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
    
    # Instead of the line graph, which showed mean audio attributes for years,
    # I chose here to make a bar graph that showed the mean attributes for decades.
    # My code is fairly similar except I group by decade instead of year and I have
    # to set the position in geom_col() to dodge to specify that I want a double
    # bar chart (one bar for Hot 100 and one for non-Hot 100).
    
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
        
    # Here, I leanred to do reactive coding within shiny using the eventReactive() command.
    # While I still don't totally understand what it means, I understand that by putting my code
    # within an eventReactive command and specifying which variables should be reactive, I can save that 
    # output, which becomes a command that can be rendered as a plot. From there, I use if else commands to add
    # another layer of reactivity in that the output will change based on multiple inputs. With this 
    # plot, the visualization will change based on the selection of song or artist using radio buttons
    # and year using the slider. For the sake of being able to see the text on the visualizations,
    # I took only the top 30 artists/songs and also made sure to reorder the bars based on the order of their value,
    # with the higher values first (on top after the coordinate flip).
    
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
    
    # Here, I created another reactive table that changes based on 3 inputs and uses if and else commands.
    # I used date$year to filter for the selected year, grouped by artist because that is the unit of analysis
    # here, and used summarize() to get the mean attrbute based on whatever the viewer selected as their input
    # (represnted through input$attribute2). I then arranged by either descending or ascending order and took the top 10
    # based on what the viewer selected (either the Highest mean attribute artists or the lowest). I then selected only
    # artist and used gt() to create a clean looking table. I also added a source note clarifying which set of data
    # is being used to generate these tables.
    
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
