#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)

# Reads in data
ms_cert <- readRDS("ms_cert.RDS")
hs_cert <- readRDS("hs_cert.RDS")
public_age <- readRDS("public_age.RDS")

# Define UI for application that draws a histogram
ui <- navbarPage(
  #theme = shinytheme("flatly"),
  "Impact of Teacher's Social Status on Student Performance",
  
  tabPanel("Purpose",
           titlePanel("Project Background and Motivations"),
           
           p("One of the most well known aphorisms in education is 
           the 'quality of an education system cannot exceed the quality of its 
           teachers'."),
           
           p("Thus, much has been researched with regard to the competence of 
           teachers and the effectiveness of their teaching. However, there has 
           not been as much focus on how the social status of teachers may 
           play a role in students outcomes.   
           The Global Teacher Status Index in 2013 was the first international 
           in-depth survey that collected data from 21 countries on details such 
           as how students respect their teachers, where teachers rank among other 
           professions, how high of a salary teachers deserve, etc. The 2018 
           study analysis released even more information on 35 nations, and 
           concluded that both high teacher pay and high status are necessary 
           to produce top academic outcomes."),
          
           p("For my final project, I will use this 
           GTSI2018 data to breakdown relationships between the various factors 
           they surveyed to more comprehensively understand the results. 
           Currently, I am using data from the U.S. Department of Education's 
           National Center for Education Statistics and its School and Staffing
           Survey. Although only based in the U.S., the SASS provides
           comprehensive, standardized data on schools across the nation.")),
  
  tabPanel("Teacher Qualifications",
           fluidPage(
             titlePanel("Percentage of U.S. Students Taught by Teachers with
                        Various Qualifications in 2011-2012"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "plot_type",
                   "Grade Level",
                   c("Middle School (Grades 6-8)" = "a", 
                     "High School (Grades 9-12)" = "b")
                 )),
               mainPanel(plotOutput("cert_plot")))
           )),
  
  tabPanel("Teacher Age Ranges",
           fluidPage(
             titlePanel("Age of U.S. Teachers in Public Schools from 1987-2018"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "plot_type_age",
                   "Age Range",
                   c("Under 30" = "a", 
                     "30 to 39" = "b", 
                     "40 to 49" = "c",
                     "50 to 59" = "d",
                     "60 and over" = "e")
                 )),
               mainPanel(plotOutput("public_age_plot")))
           )),
  
  # tabPanel("Test",
  #          h2("Teacher Age Data"),
  #          DT::dataTableOutput("my_table")),
  
  tabPanel("Discussion",
           titlePanel("Why I Chose this Data"),
           p("To start out, I thought modeling Teacher Qualifications would
             help orient the audience to the current standards teachers are
             held up to in the U.S. As one can see in the figures, quite a high
             percentage of middle school students are taught by teachers who 
             don't have a teaching certification or a major related to their 
             subject, especially for science subjects. The standard is much 
             higher for high school students, as over 50% of students are 
             taught by teachers who have certification and/or major related to
             their teaching subject.")),
  
  tabPanel("About", 
           titlePanel("About Me"),
           p("My name is Emily He and I'm pursuing an S.B. in Bioengineering. 
             Feel free to contact me at emily_he@college.harvard.edu."),
           p('Link to Github repo: https://github.com/2022ehe/gov50-finalproject')))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$public_age_plot <- renderPlot({
    # Generate type based on input$plot_type from ui
    
    # ifelse(
    #   input$plot_type_age == "a",
    #   x <- public_age %>%
    #         filter(teacher_char == 'Under 30'),
    #   x <- public_age %>%
    #         filter(teacher_char == '30 to 39')
    # )
    data <- switch(input$plot_type_age, 
                   "a" = public_age %>%
                      filter(teacher_char == 'Under 30'),
                   "b" = public_age %>%
                     filter(teacher_char == '30 to 39'),
                   "c" = public_age %>%
                     filter(teacher_char == '40 to 49'),
                   "d" = public_age %>%
                     filter(teacher_char == '50 to 59'),
                   "e" = public_age %>%
                     filter(teacher_char == '60 and over'))
    # case_when(
    #   input$plot_type_age == 'a' ~ (data <- public_age %>%
    #                               filter(teacher_char == 'Under 30')),
    #   input$plot_type_age == 'b' ~ (data <- public_age %>%
    #                               filter(teacher_char == '30 to 39')),
    #   input$plot_type_age == 'c' ~ (data <- public_age %>%
    #                               filter(teacher_char == '40 to 49')),
    #   input$plot_type_age == 'd' ~ (data <- public_age %>%
    #                               filter(teacher_char == '50 to 59')),
    #   input$plot_type_age == 'e' ~ (data <- public_age %>%
    #                               filter(teacher_char == '60 and over')),
    #   TRUE ~ (data <- public_age %>%
    #             filter(teacher_char == 'Under 30'))
    # )

    ggplot(data, aes(x = year, y = percentage)) + 
      geom_point(size = 2) + 
      geom_errorbar(aes(x = year, ymin = min, ymax = max), 
                    width=0.1, color = 'dodgerblue', alpha=0.9, size=1) + 
      labs(title = 'Age Ranges of U.S. Teachers in Public Schools from 1987-2018',
           x = 'Year',
           y = 'Percentage of Teachers', 
           caption = 'Source: U.S. Department of Education, National Center for Education Statistics')
    
  })
  
  output$cert_plot <- renderPlot({
    # Generate type based on input$plot_type from ui
    
    ifelse(
      input$plot_type == "a",
      
      # If input$plot_type is "a", plot bar graph of percentage of middle 
      # school students taught by teachers of various qualification levels
      
      x <- ms_cert,
      
      # If input$plot_type is "b", plot bar graph of percentage of middle 
      # school students taught by teachers of various qualification levels
      
      x <- hs_cert
    )
   ggplot(x, aes(x = level_subject, y = percentage, fill = qualification)) + 
    geom_col(position = 'dodge') + 
    scale_fill_brewer(name = 'Teacher Qualification', 
                      labels = c('Certification and Related Major',
                                 'Certification Only',
                                 'Related Major Only', 
                                 'Neither Qualification'),
                      palette = 'Blues') + 
    theme_bw() +
    labs(x = 'Class Subject',
         y = 'Percentage of Students',
         caption = 'Source: U.S. Department of Education, National Center for Education Statistics')

    
  })
  # output$my_table <- DT::renderDataTable({
  #   public_age
  # })

}

# Run the application 
shinyApp(ui = ui, server = server)
