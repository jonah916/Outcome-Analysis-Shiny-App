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
final_app_df_10.11.19$SurveyDate <- convertToDate(final_app_df_10.11.19$SurveyDate)
response_codes <- read.xlsx("916InkSurveyResponseCodes.xlsx", sheet = 3)
# Questions <- c("FeelAboutWriting", "HowOftenJournal", "FeelAboutAssignment", "ThinkAboutYourWriting",
#                "WorryOthersThink", "AskForHelp", "WritingBoring", "WriteMyIdeas", "AfraidWriteForGrade",
#                "WasteOfTime", "WritingIsFun", "MyStoriesMatter", "FeelAboutReading", "ThinkAboutYourReading",
#                "FeelAboutSchool", "HomeworkFinished", "LikePeopleAtSchool", "GoodImagination", "UsuallyGiveUp",
#                "EnjoyMeetingPeople", "PeopleLikeMe", "TryMyBest", "StickWithAssignments", "TalkFeelingsToPeers",
#                "TalkFeelingsToAdults", "FeelWorried", "FeelMad", "FeelSad", "HowConfident", "ReadyWriteCollege",
#                "WritingNervous", "CloseToPeopleAtSchool", "HarderOnMyself", "CanGetIntoCollege", "InControl",
#                "TryBestAtSchool", "CommunicateToPeers", "CommunicateToAdults")
# final_response_codes <- left_join(as.data.frame(Questions), response_codes[,c("Questions", "Category","ImproveDirection")])
# final_response_codes$ImproveDirection[29:38] <- c("Improve Up",
#                                                   "ImproveDown",
#                                                   "ImproveUp",
#                                                   "ImproveUp",
#                                                   "ImproveUp",
#                                                   "ImproveDown",
#                                                   "ImproveDown",
#                                                   "ImproveDown",
#                                                   "ImproveUp",
#                                                   "ImproveUp")



#test <- apply(test,2,function(x){as.numeric(extract_num(x))})

# Define main function

get_pct_change3 <- function(df){
  pre <- as.data.frame(df) %>%
    filter(., PrePost == 1) %>%
    select(., 2:38) %>%
    apply(., 2, mean, na.rm = TRUE)
  post <- as.data.frame(df) %>%
    filter(., PrePost == 2) %>%
    select(., 2:38) %>%
    apply(., 2, mean, na.rm = TRUE)
  report <- cbind(Questions = names(pre),
                  Avg.Pre.Value = as.vector(pre),
                  Avg.Post.Value = as.vector(post)) %>%
    as.data.frame(., stringsAsFactors = FALSE)
  class(report$Avg.Pre.Value) <- "numeric"
  class(report$Avg.Post.Value) <- "numeric"
  report <- report %>%
    mutate(Pct.Change = percent((Avg.Post.Value-Avg.Pre.Value)/Avg.Pre.Value))
  report <- left_join(report, response_codes[,c("Questions", "ImproveDirection")], by = "Questions")
  report
}

# UI

ui <- fluidPage(
  titlePanel("Tool for Demographic Analysis of 916 Ink Participants"),
  sidebarLayout(
    sidebarPanel(
      actionButton(inputId = "update",
                   label = "Update"),
      p("Customize the checkboxes and click \"Update\" to apply your changes. The table may take a moment to render."),
      checkboxGroupInput(inputId = "grade",
                         label = "Grade Level: ",
                         choices = sort(unique(final_app_df_10.11.19$Grade)),
                         inline = TRUE),
      #                         selected = unique(final_app_df_10.11.19$Grade)),
      actionButton(inputId = "gradeSA",
                   label = "Select All"),
      checkboxGroupInput(inputId = "gender",
                         label = "Gender: ",
                         choices = list("M", "F", "Decline"),
                         inline = TRUE),
      #                         selected = unique(final_app_df_10.11.19$Gender)),
      actionButton(inputId = "genderSA",
                   label = "Select All"),
      checkboxGroupInput(inputId = "race",
                         label = "RaceEth: ",
                         choices = sort(unique(final_app_df_10.11.19$RaceEth)),
                         inline = TRUE),
      #                        selected = sort(unique(final_app_df_10.11.19$RaceEth))),
      actionButton(inputId = "raceSA",
                   label = "Select All"),
      checkboxGroupInput(inputId = "language",
                         label = "Primary Language: ",
                         choices = sort(unique(final_app_df_10.11.19$Language)),
                         inline = TRUE),
      #                        selected = sort(unique(final_app_df_10.11.19$Language))),
      actionButton(inputId = "languageSA",
                   label = "Select All"),
      checkboxGroupInput(inputId = "city",
                         label = "City: ",
                         choices = sort(unique(final_app_df_10.11.19$City)),
                         inline = TRUE),
      #                        selected = sort(unique(final_app_df_10.11.19$City))),
      actionButton(inputId = "citySA",
                   label = "Select All"),
      checkboxGroupInput(inputId = "programsite",
                         label = "Program Site: ",
                         choices = sort(unique(final_app_df_10.11.19$Program.Site)),
                         inline = TRUE),
      #                         selected = sort(unique(final_app_df_10.11.19$Program.Site))),
      actionButton(inputId = "programsiteSA",
                   label = "Select All"),
      checkboxGroupInput(inputId = "schoolname",
                         label = "School Name: ",
                         choices = sort(unique(final_app_df_10.11.19$SchoolName)),
                         inline = TRUE),
      actionButton(inputId = "schoolnameSA",
                   label = "Select All")
      #                         selected = sort(unique(final_app_df_10.11.19$SchoolName)))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
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
  
  
  
  reactive_report <- eventReactive(input$update, {
    final_app_df_10.11.19 %>%
      #      filter(., SurveyDate > as.character(input$TimeRange[1]) & SurveyDate < as.character(input$TimeRange[2])) %>%
      filter(., Grade %in% input$grade) %>%
      filter(., Gender %in% input$gender) %>%
      filter(., RaceEth %in% input$race) %>%
      filter(., Program.Site %in% input$programsite) %>%
      filter(., Language %in% input$language) %>%
      filter(., SchoolName %in% input$schoolname) %>%
      filter(., City %in% input$city) %>%
      get_pct_change3(.) %>%
      assign("ink_report", ., envir = .GlobalEnv)
      ink_report
  })
  reactive_count <- eventReactive(input$update, {
    filtered_df <- final_app_df_10.11.19 %>%
      #      filter(., SurveyDate > as.character(input$TimeRange[1]) & SurveyDate < as.character(input$TimeRange[2])) %>%
      filter(., Grade %in% input$grade) %>%
      filter(., Gender %in% input$gender) %>%
      filter(., RaceEth %in% input$race) %>%
      filter(., Program.Site %in% input$programsite) %>%
      filter(., Language %in% input$language) %>%
      filter(., SchoolName %in% input$schoolname) %>%
      filter(., City %in% input$city)
    print(paste("*There are ", "<font color=\"#FF0000\"><b>", nrow(filtered_df)/2, "</b></font>", " Inkers in this report."))
  })
  reactive_bars <- eventReactive(reactive_report(), {
      # if (any(input$grade) < 6 & !any(input$grade %in% c(6:12))){
      #   ink_report <- ink_report %>% slice(., )
      # }
    bar_report <- ink_report
    bar_report$Pct.Change <- gsub("%", "", ink_report$Pct.Change)
    bar_report <- bar_report %>%
      mutate(., Color = factor(case_when(as.numeric(Pct.Change) > 0 & ImproveDirection == "ImproveUp" ~ "green",
                                                        as.numeric(Pct.Change) < 0 & ImproveDirection == "ImproveDown" ~ "green",
                                                        as.numeric(Pct.Change) > 0 & ImproveDirection == "ImproveDown" ~ "red",
                                                        as.numeric(Pct.Change) < 0 & ImproveDirection == "ImproveUp" ~ "red")),
                                         Lab.Pos = case_when(as.numeric(Pct.Change) > 0 ~ 1.5,
                                                             as.numeric(Pct.Change) < 0 ~ -1.5,
                                                             as.numeric(Pct.Change) == 0 ~ 0.5)) %>%
      left_join(., response_codes[, c("Questions", "Category")])
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
    # ggdraw() +
    #   draw_plot(attitudes_plot, 0, 0.67, 1, 0.5) +
    #   draw_plot(behavior_plot, 0, 0.33, 1, 0.5) +
    #   draw_plot(socemot_plot, 0, 0, 1, 0.5)
    plot_grid(attitudes_plot, behavior_plot, socemot_plot, ncol = 3)
  })
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
  output$message <-
    renderText({
      reactive_count()
    })
  output$intro <-
    renderText({
      if(input$update == 0){
        print("Welcome! Use the checkboxes on the left to set your parameters and click \"Update\" to run a report. The table may take a few seconds to render.")
      }
      else{
        return(NULL)
      }
    })
   output$plot <-
       renderPlot({
         reactive_bars()
         })
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
