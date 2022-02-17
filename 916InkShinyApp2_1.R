# Load packages

library(shiny)
library(openxlsx)
library(knitr)
library(readr)
library(dplyr)
library(scales)
library(stringr)
library(ggplot2)
library(cowplot)


# Import and Data Prep

final_app_df_10.11.19 <- read.xlsx("FinalDF2.xlsx")
final_app_df_10.11.19$SurveyDate <- convertToDate(final_app_df_10.11.19$SurveyDate) # convert dates to Date data type
response_codes <- read.xlsx("916InkSurveyResponseCodes.xlsx", sheet = 3) # data frame containing the desired direction of change for each survey question


# Define main function

get_pct_change3 <- function(df){
  # mean values for pre-surveys
  pre <- as.data.frame(df) %>%
    filter(., PrePost == 1) %>%
    select(., 2:38) %>% # refine to relevant columns
    apply(., 2, mean, na.rm = TRUE)
  # mean values for post surveys
  post <- as.data.frame(df) %>%
    filter(., PrePost == 2) %>%
    select(., 2:38) %>% # refine to relevant columns
    apply(., 2, mean, na.rm = TRUE)
  # create table showing pre and post means for each survey question
  report <- cbind(Questions = names(pre),
                  Avg.Pre.Value = as.vector(pre),
                  Avg.Post.Value = as.vector(post)) %>%
    as.data.frame(., stringsAsFactors = FALSE)
  class(report$Avg.Pre.Value) <- "numeric"
  class(report$Avg.Post.Value) <- "numeric"
  # add column for percent change between pre- and post-surveys
  report <- report %>%
    mutate(Pct.Change = percent((Avg.Post.Value-Avg.Pre.Value)/Avg.Pre.Value))
  # join report to response codes DF to show whether change is good or bad
  report <- left_join(report, response_codes[,c("Questions", "ImproveDirection")], by = "Questions")
  report
}


# UI

ui <- fluidPage(
  titlePanel("Tool for Demographic Analysis of 916 Ink Participants"), # App title
  sidebarLayout(
    sidebarPanel(
      # Update Button
      actionButton(inputId = "update",
                   label = "Update"),
      p("Customize the checkboxes and click \"Update\" to apply your changes. The table may take a moment to render."),
      # Grade in school checkboxes
      checkboxGroupInput(inputId = "grade",
                         label = "Grade Level: ",
                         choices = sort(unique(final_app_df_10.11.19$Grade)),
                         inline = TRUE),
      # Select All button for grades
      actionButton(inputId = "gradeSA",
                   label = "Select All"),
      # Gender checkboxes
      checkboxGroupInput(inputId = "gender",
                         label = "Gender: ",
                         choices = list("M", "F", "Decline"),
                         inline = TRUE),
      # Select All button for gender
      actionButton(inputId = "genderSA",
                   label = "Select All"),
      # Race and ethnicity checkboxes
      checkboxGroupInput(inputId = "race",
                         label = "RaceEth: ",
                         choices = sort(unique(final_app_df_10.11.19$RaceEth)),
                         inline = TRUE),
      # Select All button for race and ethnicity
      actionButton(inputId = "raceSA",
                   label = "Select All"),
      # Primary language checkboxes
      checkboxGroupInput(inputId = "language",
                         label = "Primary Language: ",
                         choices = sort(unique(final_app_df_10.11.19$Language)),
                         inline = TRUE),
      # Select All button for primary language
      actionButton(inputId = "languageSA",
                   label = "Select All"),
      # City checkboxes
      checkboxGroupInput(inputId = "city",
                         label = "City: ",
                         choices = sort(unique(final_app_df_10.11.19$City)),
                         inline = TRUE),
      # Select All button for cities
      actionButton(inputId = "citySA",
                   label = "Select All"),
      # Program site checkboxes
      checkboxGroupInput(inputId = "programsite",
                         label = "Program Site: ",
                         choices = sort(unique(final_app_df_10.11.19$Program.Site)),
                         inline = TRUE),
      # Select All button for program site
      actionButton(inputId = "programsiteSA",
                   label = "Select All"),
      # Name of school checkboxes
      checkboxGroupInput(inputId = "schoolname",
                         label = "School Name: ",
                         choices = sort(unique(final_app_df_10.11.19$SchoolName)),
                         inline = TRUE),
      # Select All button for names of schools
      actionButton(inputId = "schoolnameSA",
                   label = "Select All")
    ),
    mainPanel(
      tabsetPanel(type = "tabs", # 2 tabs: one with the report in table form, the other showing the bar plots for each survey question
                  tabPanel("Report",
                           tableOutput("report"),
                           htmlOutput("message"),
                           htmlOutput("intro")),
                  tabPanel("Plot",
                           plotOutput("plot"),
                           height = 5000,
                           width = 500,
                           textInput(inputId = "my_file_name",
                                     label = "Name Plot for Download: "),
                           downloadButton("downloadPlot", "Download")
                          )
                  )
              )
  ),
  # HTML/CSS for adjusting the dimensions of the report/plot
  tags$script("$(document).on('shiny:connected', function(event) {
var myWidth = $(window).width();
              Shiny.onInputChange('shiny_width',myWidth)
              
              });"),

  tags$script("$(document).on('shiny:connected', function(event) {
              var myHeight = $(window).height();
              Shiny.onInputChange('shiny_height',myHeight)
              
              });")
)


# Server

server <- function(input, output, session){
  
  # set default to 'Select All' for every variable
  # if 'Select All' has been clicked an odd number of times, de-select all options
  # if it's been clicked an even number of times, select all options
  
  observe({
    if (input$gradeSA%%2 == 0){
      updateCheckboxGroupInput(session,
                               inputId = "grade",
                               label = "Grade Level: ",
                               choices = sort(unique(final_app_df_10.11.19$Grade)),
                               inline = TRUE)
    }
    if (input$gradeSA%%2 != 0){
      updateCheckboxGroupInput(session,
                               inputId = "grade",
                               label = "Grade Level: ",
                               choices = sort(unique(final_app_df_10.11.19$Grade)),
                               selected = sort(unique(final_app_df_10.11.19$Grade)),
                               inline = TRUE)
    }
  })
  observe({
    if (input$genderSA%%2 == 0){
      updateCheckboxGroupInput(session,
                               inputId = "gender",
                               label = "Gender: ",
                               choices = c("M", "F", "Decline"),
                               inline = TRUE)
    }
    if (input$genderSA%%2 != 0){
      updateCheckboxGroupInput(session,
                               inputId = "gender",
                               label = "Gender: ",
                               choices = c("M", "F", "Decline"),
                               selected = c("M", "F", "Decline"),
                               inline = TRUE)
    }
  })
  observe({
    if (input$raceSA%%2 == 0){
      updateCheckboxGroupInput(session,
                               inputId = "race",
                               label = "Race/Eth: ",
                               choices = sort(unique(final_app_df_10.11.19$RaceEth)),
                               inline = TRUE)
    }
    if (input$raceSA%%2 != 0){
      updateCheckboxGroupInput(session,
                               inputId = "race",
                               label = "Race/Eth: ",
                               choices = sort(unique(final_app_df_10.11.19$RaceEth)),
                               selected = sort(unique(final_app_df_10.11.19$RaceEth)),
                               inline = TRUE)
    }
  })
  observe({
    if (input$languageSA%%2 == 0){
      updateCheckboxGroupInput(session,
                               inputId = "language",
                               label = "Primary Language: ",
                               choices = sort(unique(final_app_df_10.11.19$Language)),
                               inline = TRUE)
    }
    if (input$languageSA%%2 != 0){
      updateCheckboxGroupInput(session,
                               inputId = "language",
                               label = "Primary Language: ",
                               choices = sort(unique(final_app_df_10.11.19$Language)),
                               selected = sort(unique(final_app_df_10.11.19$Language)),
                               inline = TRUE)
    }
  })
  observe({
    if (input$citySA%%2 == 0){
      updateCheckboxGroupInput(session,
                               inputId = "city",
                               label = "City: ",
                               choices = sort(unique(final_app_df_10.11.19$City)),
                               inline = TRUE)
    }
    if (input$citySA%%2 != 0){
      updateCheckboxGroupInput(session,
                               inputId = "city",
                               label = "City: ",
                               choices = sort(unique(final_app_df_10.11.19$City)),
                               selected = sort(unique(final_app_df_10.11.19$City)),
                               inline = TRUE)
    }
  })
  observe({
    if (input$programsiteSA%%2 == 0){
      updateCheckboxGroupInput(session,
                               inputId = "programsite",
                               label = "Program Site: ",
                               choices = sort(unique(final_app_df_10.11.19$Program.Site)),
                               inline = TRUE)
    }
    if (input$programsiteSA%%2 != 0){
      updateCheckboxGroupInput(session,
                               inputId = "programsite",
                               label = "Program Site: ",
                               choices = sort(unique(final_app_df_10.11.19$Program.Site)),
                               selected = sort(unique(final_app_df_10.11.19$Program.Site)),
                               inline = TRUE)
    }
  })
  observe({
    if (input$schoolnameSA%%2 == 0){
      updateCheckboxGroupInput(session,
                               inputId = "schoolname",
                               label = "School Name: ",
                               choices = sort(unique(final_app_df_10.11.19$SchoolName)),
                               inline = TRUE)
    }
    if (input$schoolnameSA%%2 != 0){
      updateCheckboxGroupInput(session,
                               inputId = "schoolname",
                               label = "School Name: ",
                               choices = sort(unique(final_app_df_10.11.19$SchoolName)),
                               selected = sort(unique(final_app_df_10.11.19$SchoolName)),
                               inline = TRUE)
    }
  })
  
  # assemble the dynamic report
  
  reactive_report <- eventReactive(input$update, {
    # apply all the filters chosen by the user
    final_app_df_10.11.19 %>%
      filter(., Grade %in% input$grade) %>%
      filter(., Gender %in% input$gender) %>%
      filter(., RaceEth %in% input$race) %>%
      filter(., Program.Site %in% input$programsite) %>%
      filter(., Language %in% input$language) %>%
      filter(., SchoolName %in% input$schoolname) %>%
      filter(., City %in% input$city) %>%
    # calculate the percentchange between pre- and post-values
      get_pct_change3(.) %>%
      assign("ink_report", ., envir = .GlobalEnv)
      ink_report
  })
  
  # dynamically calculate how many program paticipants meet the criteria specified by the filters
  
  reactive_count <- eventReactive(input$update, {
    filtered_df <- final_app_df_10.11.19 %>%
      filter(., Grade %in% input$grade) %>%
      filter(., Gender %in% input$gender) %>%
      filter(., RaceEth %in% input$race) %>%
      filter(., Program.Site %in% input$programsite) %>%
      filter(., Language %in% input$language) %>%
      filter(., SchoolName %in% input$schoolname) %>%
      filter(., City %in% input$city)
    # use HTML to format the number of participants
    print(paste("*There are ", "<font color=\"#FF0000\"><b>", nrow(filtered_df)/2, "</b></font>", " Inkers in this report."))
  })
  
  # dynamically produce a barplots showing % change for each survey question, disaggregated by category of question
  
  reactive_bars <- eventReactive(reactive_report(), {
    bar_report <- ink_report
    bar_report$Pct.Change <- gsub("%", "", ink_report$Pct.Change)
    bar_report <- bar_report %>%
    # specify when changes are good and when they are bad; color good changes green and bad changes red
      mutate(., Color = factor(case_when(as.numeric(Pct.Change) > 0 & ImproveDirection == "ImproveUp" ~ "green",
                                                        as.numeric(Pct.Change) < 0 & ImproveDirection == "ImproveDown" ~ "green",
                                                        as.numeric(Pct.Change) > 0 & ImproveDirection == "ImproveDown" ~ "red",
                                                        as.numeric(Pct.Change) < 0 & ImproveDirection == "ImproveUp" ~ "red")),
                                         Lab.Pos = case_when(as.numeric(Pct.Change) > 0 ~ 1.5,
                                                             as.numeric(Pct.Change) < 0 ~ -1.5,
                                                             as.numeric(Pct.Change) == 0 ~ 0.5)) %>%
      left_join(., response_codes[, c("Questions", "Category")])
    
    # plot for questions related to attitudes about writing
    
    attitudes_plot <- ggplot(bar_report %>% filter(., Category == "Attitudes"), aes(x = factor(Questions), y = as.numeric(Pct.Change))) +
      geom_col(aes(x = factor(Questions),
                   y = as.numeric(Pct.Change),
                   fill = Color)) +
      scale_fill_manual(values = c(red = "#FA3438", green = "#158C29")) +
      geom_text(aes(x = factor(Questions),
                    y = as.numeric(Pct.Change) + Lab.Pos,
                    label = round(as.numeric(Pct.Change), 2))) +
      coord_flip() +
      labs(x = "",
           y = "Percent Change",
           title = "Attitudes") +
      theme_bw() +
      theme(plot.title = element_text(size = 16),
            legend.position = "none")
    
    # plot about questions related to participant behavior
    
    behavior_plot <- ggplot(bar_report %>% filter(., Category == "Behavior"), aes(x = factor(Questions), y = as.numeric(Pct.Change))) +
      geom_col(aes(x = factor(Questions),
                   y = as.numeric(Pct.Change),
                   fill = Color)) +
      scale_fill_manual(values = c(red = "#FA3438", green = "#158C29")) +
      geom_text(aes(x = factor(Questions),
                    y = as.numeric(Pct.Change) + Lab.Pos,
                    label = round(as.numeric(Pct.Change), 2))) +
      coord_flip() +
      labs(x = "",
           y = "Percent Change",
           title = "Behavior") +
      theme_bw() +
      theme(plot.title = element_text(size = 16),
            legend.position = "none")
    
    # plot about questions related to social-emotional learning
    
    socemot_plot <- ggplot(bar_report %>% filter(., Category == "SocEmotional"), aes(x = factor(Questions), y = as.numeric(Pct.Change))) +
      geom_col(aes(x = factor(Questions),
                   y = as.numeric(Pct.Change),
                   fill = Color)) +
      scale_fill_manual(values = c(red = "#FA3438", green = "#158C29")) +
      geom_text(aes(x = factor(Questions),
                    y = as.numeric(Pct.Change) + Lab.Pos,
                    label = round(as.numeric(Pct.Change), 2))) +
      coord_flip() +
      labs(x = "",
           y = "Percent Change",
           title = "Social Emotional") +
      theme_bw() +
      theme(plot.title = element_text(size = 16),
            legend.position = "none")
    plot_grid(attitudes_plot, behavior_plot, socemot_plot, ncol = 3)
  })
  
  # print the report
  # if any of the variables have no options selected, do not render a report
  output$report <- 
    renderTable({
      if(is.null(input$gender)){
        return(NULL)
      }
      if(is.null(input$grade)){
        return(NULL)
      }
      if(is.null(input$race)){
        return(NULL)
      }
      if(is.null(input$schoolname)){
        return(NULL)
      }
      if(is.null(input$programsite)){
        return(NULL)
      }
      if(is.null(input$city)){
        return(NULL)
      }
      if(is.null(input$language)){
        return(NULL)
      }
      reactive_report()
    })
  
  # print the count of participants meeting the specified criteria
  output$message <-
    renderText({
      reactive_count()
    })
  
  # introductory message to the app
  output$intro <-
    renderText({
      if(input$update == 0){
        print("Welcome! Use the checkboxes on the left to set your parameters and click \"Update\" to run a report. The table may take a few seconds to render.")
      }
      else{
        return(NULL)
      }
    })
  
  # print the reactive barplot
   output$plot <-
       renderPlot({
         reactive_bars()
         })
  
  # option to download the custom barplot
   output$downloadPlot <- downloadHandler(
     filename <- function() {
       paste(input$my_file_name, "png", sep = ".")
       },
     content = function(file) {
       png(file,
           width = input$shiny_width,
           height = input$shiny_height)
       print(reactive_bars())
       dev.off()
     }
   )
}


shinyApp(ui = ui, server = server)
