library(shiny)
library(tidyverse)
library(shinythemes)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

######### DATA CLEANSING ########

#Load the data
attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')


#Tidy and enrichment of the data

## attendance data
attendance <- attendance %>%
                mutate(full_name = paste(team, team_name, sep = " "))

att_summ <- attendance %>%
                group_by(year) %>%
                summarise(total = mean(total),
                          home = mean(home))

att_team <- attendance %>%
              group_by(year, full_name) %>%
              summarise(total_2 = sum(weekly_attendance, na.rm = TRUE))


## games data
games <- games %>%
            mutate(home_win = if_else(winner == home_team, 1, 0),
                   away_win = if_else(winner == away_team, 1, 0))

home_win_pctg <- games %>%
                    group_by(year) %>%
                    summarise(home = sum(home_win),
                              total_games = sum(home_win) + sum(away_win),
                              home_win_pctgs = sum(home_win)/total_games)


home_away <- games %>%
                 group_by(year, home_team) %>%
                 summarise(home = sum(home_win),
                           away = sum(away_win)) %>%
                 pivot_longer(c("home","away"),
                              names_to = "home_aways",
                              values_to = "win_number")


########## UI & SERVER ##########

# BUILD UI
ui <- fluidPage(theme = shinytheme("cosmo"),

  navbarPage(
    "National Football League",

    # App Logo
    ##tags$div(
    ##    class = "navbar-brand",
    ##    tags$img(src="figures/nfl-logo.png", width = 30, height = 30, alt = "NFL Logo") #style="float:left; margin-left: 5px; margin-right: 5px; margin-top: -15px")),
    ##    ),

    # HOME SECTION
    tabPanel("Home",
             h2(tags$strong("Welcome to the National Football League App!")),
             p("This app was developed to give you a bit of knowledge about National Football League. If you are new to this sport, then this is for you!"),
             p("On the navigation bar, you will find several section that will embark you on this rookie journey. Each of them will provide you a simple yet insightful statistics that will help you understand more about this sport. I hope you get exciting seeing this and ready to influence your circle to follow this sport with you!"),
             p("As a starter, have a look at this video below."),
             tags$iframe(width="840", height="473", src="https://www.youtube.com/embed/3t6hM5tRlfA", frameborder = "0", allowfullscreen = "true"),
             p("Feel free to explore the data and discover interesting insights!")
            ),

    # ATTENDANCE SECTION
    tabPanel("Attendance",
             h2(tags$strong("NFL Attendance")),
             p("NFL is one of the famous sport in the United States, even nowadays globally!"),
             p("But, how is the number of attendance in the game usually? We will take a look at some statistics on the NFL's attendance number from 2000 - 2019."),
             p("Let's explore the statistic below!"),

             br(),
             br(),

             h3(tags$strong("The breakdown of Attendance by Year")),
             fluidRow(
               sidebarLayout(
                  sidebarPanel(
                    sliderInput(
                      inputId = "year_slider",
                      label = "Select Year Range:",
                      min = floor(min(att_summ$year)),
                      max = floor(max(att_summ$year)),
                      value = c(floor(min(att_summ$year)), floor(max(att_summ$year))),
                      step = 1,
                      sep = " "
                    ),
                ),
                mainPanel(
                  plotOutput("TotAttPlot")
                  )
                )
             ),

             br(),
             br(),

             h3(tags$strong("The breakdown of Attendance by Team")),
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   sliderInput(
                     inputId = "year_slider_2",
                     label = "Select Year Range:",
                     min = floor(min(att_team$year)),
                     max = floor(max(att_team$year)),
                     value = c(floor(min(att_team$year)), floor(max(att_team$year))),
                     step = 1,
                     sep = " "
                   ),
                   selectInput(
                     inputId = "team_selector_2",
                     label = "Select Team:",
                     choices = c("All", unique(att_team$full_name)),
                     selected = "All"
                   )
                 ),
                 mainPanel(
                   plotOutput("TeamAttPlot", width = "100%", height = "750px")
                 )
               )
             )
        ),

  # GAME SECTION
  tabPanel("Games",
           h2(tags$strong("Does Game Location Matters?")),
           p("As a sport fan, we want the sport to be competitive, a domination might ruin the sport in general and it becomes boring to watch."),
           p("But, does a NFL team always dominates? furthermore, does home advantage guarantee a win? or maybe the away team will give a surprise?"),
           p("Let's see the facts!"),

           br(),
           br(),

           h3(tags$strong("How does the home winning percentage annually?")),
           fluidRow(
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId = "year_slider_game",
                   label = "Select Year Range:",
                   min = floor(min(home_win_pctg$year)),
                   max = floor(max(home_win_pctg$year)),
                   value = c(floor(min(home_win_pctg$year)), floor(max(home_win_pctg$year))),
                   step = 1,
                   sep = " "
                 ),
               ),
               mainPanel(
                 plotOutput("home_win_plot")
               )
             )
          ),

          br(),
          br(),

          h3(tags$strong("The breakdown of Home & Away win by Team")),
          fluidRow(
            sidebarLayout(
              sidebarPanel(
                sliderInput(
                  inputId = "year_slider_game_2",
                  label = "Select Year Range:",
                  min = floor(min(home_away$year)),
                  max = floor(max(home_away$year)),
                  value = c(floor(min(home_away$year)), floor(max(home_away$year))),
                  step = 1,
                  sep = " "
                ),
                selectizeInput(
                  inputId = "team_selector_game",
                  label = "Select Team:",
                  choices = c("All", unique(home_away$home_team)),
                  selected = "All",
                  multiple = TRUE
                )
              ),
              mainPanel(
                plotOutput("home_away_win", width = "100%", height = "600px")
              )
            )
        )
    ),

  fluidRow(
    column(10,
           div(class = "about",
               uiOutput('about'))
    )
  ),
  includeCSS("styles.css")
 )
)

# Define server logic ----
server <- function(input, output) {

  # OUTPUT FOR ATTENDANCE PAGE
  ##For Yearly Attendance
  filtered_data_by_year <- reactive({
    year_range <- as.integer(input$year_slider[1]):as.integer(input$year_slider[2])
    return(att_summ %>% filter(year %in% year_range))
  })

  output$TotAttPlot <- renderPlot({
    ggplot(filtered_data_by_year(), aes(x = as.integer(year), y = as.integer(total), group = 1)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      #geom_text(aes(label = scales::comma(total)),
      #          position = position_dodge(width = 0.9),
      #          vjust = -0.5,
      #          hjust = 0.5,
      #          size = 4) +
      theme_minimal(base_size = 14) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Year", y = "Total Attendance")
  })



  ## For Team Attendance
  filtered_data_by_team <- reactive({
    year_range_2 <- as.integer(input$year_slider_2[1]):as.integer(input$year_slider_2[2])

    selected_team_2 <- input$team_selector_2

    filtered_data_2 <- att_team %>%
      filter(year %in% year_range_2) %>%
      group_by(full_name) %>%
      summarize(total_2 = sum(total_2))

    if (selected_team_2 != "All") {
      filtered_data_2 <- filtered_data_2 %>%
        filter(full_name == selected_team_2)
    }

    return(filtered_data_2)
  })


  output$TeamAttPlot <- renderPlot({
    max_2 <- filtered_data_by_team() %>%
      group_by(full_name) %>%
      summarise(max_att = max(total_2))
    ggplot(filtered_data_by_team()) +
      geom_bar(aes(x = reorder(full_name, total_2), y = total_2), position = "dodge", stat = "identity", fill = "skyblue") +
      geom_text(aes(label = scales::comma(max_att), x = full_name, y = max_att),
                position = position_dodge(width = 0.9),
                vjust = 0.5,
                hjust = 1.25,
                size = 4,
                data = max_2) +
      coord_flip() +
      xlab("Team") +
      ylab("Number of Attendance") +
      scale_y_continuous(labels = scales::comma) +
      theme(legend.position="bottom", text = element_text(size = 12)) +
      theme_minimal(base_size = 14)
  })


  #OUTPUT FOR GAME PAGE
  ## For Home Win Percentage
  filtered_data_home <- reactive({
    year_range_home <- as.integer(input$year_slider_game[1]):as.integer(input$year_slider_game[2])
    return(home_win_pctg %>% filter(year %in% year_range_home))
  })

  output$home_win_plot <- renderPlot({
    ggplot(filtered_data_home(), aes(x = as.integer(year), y = home_win_pctgs)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      geom_text(aes(label = scales::percent(home_win_pctgs, accuracy=0.1)),
                position = position_dodge(width = 0.9),
                vjust = -1,
                hjust = 1.15,
                size = 3) +
      labs(
        x = "Year",
        y = "Home Win Percentage (%)"
      ) +
      theme_minimal()
  })

  ## For Home/Away Win
  filtered_data_game_2 <- reactive({
    year_range_games_2 <- as.integer(input$year_slider_game_2[1]):as.integer(input$year_slider_game_2[2])

    selected_teams <- input$team_selector_game

    if ("All" %in% selected_teams) {
      filtered_data <- home_away %>% filter(year %in% year_range_games_2)
    } else {
      filtered_data <- home_away %>% filter(year %in% year_range_games_2, home_team %in% selected_teams)
    }

    return(filtered_data)
  })



  output$home_away_win <- renderPlot({
    maxs <- filtered_data_game_2() %>%
      group_by(home_team) %>%
      summarise(max_wins = max(win_number))

    sorted_data <- filtered_data_game_2() %>%
      arrange(desc(win_number))

    ggplot(sorted_data) +
        geom_bar(aes(x = reorder(home_team, +win_number), y = win_number, fill = home_aways), position = "dodge", stat = "identity") +
        #geom_col(position = "dodge") +
        #geom_text(aes(label = max_wins, x = home_team, y = max_wins),
        #          position = position_dodge(width = 0.9),
        #          vjust = 0.5,
        #          hjust = 1.25,
        #          size = 4,
        #          data = maxs) +
        scale_y_continuous(breaks = seq(0, 12, 1)) +
        scale_fill_manual(values=c("skyblue", "navy")) +
        coord_flip() +
        xlab("Team") +
        ylab("Number of Win") +
        labs(fill = NULL) +
        theme(legend.position="bottom", text = element_text(size = 12)) +
        theme_minimal(base_size = 14)
  })


  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)


