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
library(gganimate)
library(maps)
library(mapproj)

source("helper.R")

# Reads in certification data
ms_cert <- readRDS("ms_cert.RDS")
hs_cert <- readRDS("hs_cert.RDS")

# Reads in age data
public_age <- readRDS("public_age.RDS")
private_age <- readRDS("private_age.RDS")
age_total <- readRDS("age_total.RDS")
age_plot <- readRDS("age.RDS")

# Reads in degree data
public_degree <- readRDS("public_degree.RDS")
private_degree <- readRDS("private_degree.RDS")
degree_total <- readRDS("degree_total.RDS")

# Reads in years of teaching experience data
public_years <- readRDS("public_years.RDS")
private_years <- readRDS("private_years.RDS")
years_total <- readRDS("year_total.RDS")

# Reads in degree and years data by state
state_degree <- readRDS("state_degree.RDS")
state_years <- readRDS("state_years.RDS")

# Reads in total number of teachers in private and public schools for model
public_teachers <- readRDS("public_teachers.RDS")
private_teachers <- readRDS("private_teachers.RDS")

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme("flatly"),
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
  
  tabPanel("Teacher Age Ranges",
           fluidPage(
             titlePanel("Age of Teachers in U.S. Schools from 1987-2018"),
             fluidRow(
               column(12, 
                      imageOutput("animated_age_plot", 
                                  height = "100%",
                                  width = "100%"),
                      align = "center"),
               br(),
               br(),
               h3("Explore the Data"),
               column(4,
                      selectInput(
                        "plot_type_age",
                        "Age Range",
                        c("Under 30" = "a", 
                          "30 to 39" = "b", 
                          "40 to 49" = "c",
                          "50 to 59" = "d",
                          "60 and over" = "e"))
                ),
               column(7, 
                      plotOutput("all_age_plot"),
                      # plotOutput("public_age_plot"),
                      # plotOutput("private_age_plot"),
                      align = "center")
             ))),
  
  # tabPanel("Test",
  #          h2("Teacher Age Data"),
  #          DT::dataTableOutput("my_table")),
  
  tabPanel("Teacher Qualifications",
           fluidPage(
             titlePanel("Teacher Qualifications"),
             fluidRow(
               column(12,
                      h4("Percentage of U.S. Students Taught by Teachers with
                        Various Qualifications in 2011-2012"),
                      align = "left"),
               br(),
               column(4,
                      selectInput(
                        "plot_type",
                        "Grade Level",
                        c("Middle School (Grades 6-8)" = "a",
                          "High School (Grades 9-12)" = "b")),
                      align = "left"
               ),
               column(7,
                      plotOutput("cert_plot"),
               align = "center"),
               br(),
               column(3,
                      selectInput(
                        "var_degree",
                        "Highest Degree Earned",
                        c("Less than Bachelor's" = "a",
                          "Bachelor's" = "b",
                          "Master's" = "c",
                          "Education Specialist or Doctor's" = "d")),
                      align = "left",
                      # sliderInput("range",
                      #             label = "Range of interest:",
                      #             min = 0, max = 100, value = c(0, 100))
                      ),
               column(7,
                      plotOutput("state_degree_plot"),
                      align = "center"),
               br(),
               column(3,
                      selectInput(
                        "plot_type_degree",
                        "Degree Type",
                        c("Less than Bachelor's" = "a",
                          "Bachelor's" = "b",
                          "Master's" = "c",
                          "Education Specialist" = "d",
                          "Doctor's" = "e")),
                      align = "left"
               ),
               column(7,
                      plotOutput("all_degree_plot"),
                      #plotOutput("public_degree_plot"),
                      #plotOutput("private_degree_plot"),
                      align = "center"),
             )
             )),

  tabPanel("Teacher Experience",
           fluidPage(
             titlePanel("Teacher Experience"),
             fluidRow(
               column(3,
                      selectInput(
                        "var_years",
                        "Years of Teaching Experience",
                        c("Less than 3 years" = "a",
                          "3-9 years" = "b",
                          "10-20 years" = "c",
                          "Over 20 years" = "d")),
                      align = "left",
                      # sliderInput("range",
                      #             label = "Range of interest:",
                      #             min = 0, max = 100, value = c(0, 100))
               ),
               column(7,
                      plotOutput("state_years_plot"),
                      align = "center"),
               br(),
               column(3,
                      selectInput(
                        "plot_type_years",
                        "Years of Teaching Experience",
                        c("Less than 3" = "a",
                          "3 to 9" = "b",
                          "10 to 20" = "c",
                          "Over 20" = "d")),
                      align = "left"
               ),
               column(7,
                      plotOutput("all_years_plot"),
                      # plotOutput("public_years_plot"),
                      # plotOutput("private_years_plot"),
                      align = "center"),
             ))),

  tabPanel("Model",
           fluidPage(
             titlePanel("Linear Regression Model"),
             fluidRow(
               column(3,
                      selectInput(
                        "var",
                        "School Type",
                        c("Public" = "a",
                          "Private" = "b")),
                      align = "left"
               ),
               column(7,
                      plotOutput("model_plot"),
                      align = "center"),
             ))),
  
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
           p('Link to Github repo: https://github.com/2022ehe/US-Teachers')))

# Define server logic required to draw all plots
server <- function(input, output) {
  output$public_age_plot <- renderPlot({
    # Generate type based on input$plot_type from ui

    public_data <- switch(input$plot_type_age, 
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

    ggplot(public_data, aes(x = year, y = percentage)) + 
      geom_point(size = 2) + 
      geom_errorbar(aes(x = year, ymin = min, ymax = max), 
                    width=0.1, color = 'dodgerblue', alpha=0.9, size=1) + 
      labs(title = 'Age Ranges of U.S. Teachers in Public Schools from 1987-2018',
           x = 'Year',
           y = 'Percentage of Teachers', 
           caption = 'Source: U.S. Department of Education, National Center for Education Statistics') + 
      theme_bw()
  })
  
  output$private_age_plot <- renderPlot({
    
    private_data <- switch(input$plot_type_age, 
                           "a" = private_age %>%
                             filter(teacher_char == 'Under 30'),
                           "b" = private_age %>%
                             filter(teacher_char == '30 to 39'),
                           "c" = private_age %>%
                             filter(teacher_char == '40 to 49'),
                           "d" = private_age %>%
                             filter(teacher_char == '50 to 59'),
                           "e" = private_age %>%
                             filter(teacher_char == '60 and over'))
    
    ggplot(private_data, aes(x = year, y = percentage)) + 
      geom_point(size = 2) + 
      geom_errorbar(aes(x = year, ymin = min, ymax = max), 
                    width=0.1, color = 'dodgerblue', alpha=0.9, size=1) + 
      labs(title = 'Age Ranges of U.S. Teachers in Private Schools from 1987-2018',
           x = 'Year',
           y = 'Percentage of Teachers', 
           caption = 'Source: U.S. Department of Education, National Center for Education Statistics') + 
      theme_bw()
  })
  
  output$all_age_plot <- renderPlot({
    
    data <- switch(input$plot_type_age, 
                           "a" = age_total %>%
                             filter(teacher_char == 'Under 30'),
                           "b" = age_total %>%
                             filter(teacher_char == '30 to 39'),
                           "c" = age_total %>%
                             filter(teacher_char == '40 to 49'),
                           "d" = age_total %>%
                             filter(teacher_char == '50 to 59'),
                           "e" = age_total %>%
                             filter(teacher_char == '60 and over'))
    
    ggplot(data, aes(x = year, y = percentage, color = type)) + 
      geom_point(size = 2) + 
      geom_errorbar(aes(x = year, ymin = min, ymax = max), 
                    width=0.1, alpha=0.9, size=1) + 
      labs(title = 'Age Ranges of U.S. Teachers in Schools from 1987-2018',
           x = 'Year',
           y = 'Percentage of Teachers', 
           caption = 'Source: U.S. Department of Education, National Center for Education Statistics') + 
      theme_bw()
  })
  
  output$animated_age_plot <- renderImage({
    # Return a list containing the filename
    list(src = "age.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = FALSE)
  
  output$public_degree_plot <- renderPlot({
    
    public_data <- switch(input$plot_type_degree, 
                           "a" = public_degree %>%
                             filter(teacher_char == "Less than bachelor's"),
                           "b" = public_degree %>%
                             filter(teacher_char == "Bachelor's"),
                           "c" = public_degree %>%
                             filter(teacher_char == "Master's"),
                           "d" = public_degree %>%
                             filter(teacher_char == "Education specialist"),
                           "e" = public_degree %>%
                             filter(teacher_char == "Doctor's"))
    
    ggplot(public_data, aes(x = year, y = percentage)) + 
      geom_point(size = 2) + 
      geom_errorbar(aes(x = year, ymin = min, ymax = max), 
                    width=0.1, color = 'deepskyblue', alpha=0.9, size=1) + 
      labs(title = 'Highest Degree Earned for U.S. Teachers in Public Schools from 1987-2018',
           x = 'Year',
           y = 'Percentage of Teachers', 
           caption = 'Source: U.S. Department of Education, National Center for Education Statistics') + 
      theme_bw()
  })
  
  output$private_degree_plot <- renderPlot({
    
    private_data <- switch(input$plot_type_degree, 
                           "a" = private_degree %>%
                             filter(teacher_char == "Less than bachelor's"),
                           "b" = private_degree %>%
                             filter(teacher_char == "Bachelor's"),
                           "c" = private_degree %>%
                             filter(teacher_char == "Master's"),
                           "d" = private_degree %>%
                             filter(teacher_char == "Education specialist"),
                           "e" = private_degree %>%
                             filter(teacher_char == "Doctor's"))
    
    ggplot(private_data, aes(x = year, y = percentage)) + 
      geom_point(size = 2) + 
      geom_errorbar(aes(x = year, ymin = min, ymax = max), 
                    width=0.1, color = 'royalblue3', alpha=0.9, size=1) + 
      labs(title = 'Highest Degree Earned for U.S. Teachers in Private Schools from 1987-2018',
           x = 'Year',
           y = 'Percentage of Teachers', 
           caption = 'Source: U.S. Department of Education, National Center for Education Statistics') +
      theme_bw()
  })
  
  output$all_degree_plot <- renderPlot({
    
    data <- switch(input$plot_type_degree, 
                           "a" = degree_total %>%
                             filter(teacher_char == "Less than bachelor's"),
                           "b" = degree_total %>%
                             filter(teacher_char == "Bachelor's"),
                           "c" = degree_total %>%
                             filter(teacher_char == "Master's"),
                           "d" = degree_total %>%
                             filter(teacher_char == "Education specialist"),
                           "e" = degree_total %>%
                             filter(teacher_char == "Doctor's"))
    
    ggplot(data, aes(x = year, y = percentage, color = type)) + 
      geom_point(size = 2) + 
      geom_errorbar(aes(x = year, ymin = min, ymax = max), 
                    width=0.1, alpha=0.9, size=1) + 
      labs(title = 'Highest Degree Earned for U.S. Teachers in Schools from 1987-2018',
           x = 'Year',
           y = 'Percentage of Teachers', 
           caption = 'Source: U.S. Department of Education, National Center for Education Statistics') +
      theme_bw()
  })
  
  output$public_years_plot <- renderPlot({
    
    public_data <- switch(input$plot_type_years, 
                           "a" = public_years %>%
                             filter(teacher_char == "Less than 3"),
                           "b" = public_years %>%
                             filter(teacher_char == "3 to 9"),
                           "c" = public_years %>%
                             filter(teacher_char == "10 to 20"),
                           "d" = public_years %>%
                             filter(teacher_char == "Over 20"))
    
    ggplot(public_data, aes(x = year, y = percentage)) + 
      geom_point(size = 2) + 
      geom_errorbar(aes(x = year, ymin = min, ymax = max), 
                    width=0.1, color = 'deepskyblue', alpha=0.9, size=1) + 
      labs(title = 'Years of Teaching Experience for U.S. Teachers in Public Schools from 1987-2018',
           x = 'Year',
           y = 'Percentage of Teachers', 
           caption = 'Source: U.S. Department of Education, National Center for Education Statistics') + 
      theme_bw()
  })
  
  output$private_years_plot <- renderPlot({
    
    private_data <- switch(input$plot_type_years, 
                           "a" = private_years %>%
                             filter(teacher_char == "Less than 3"),
                           "b" = private_years %>%
                             filter(teacher_char == "3 to 9"),
                           "c" = private_years %>%
                             filter(teacher_char == "10 to 20"),
                           "d" = private_years %>%
                             filter(teacher_char == "Over 20"))
    
    ggplot(private_data, aes(x = year, y = percentage)) + 
      geom_point(size = 2) + 
      geom_errorbar(aes(x = year, ymin = min, ymax = max), 
                    width=0.1, color = 'royalblue3', alpha=0.9, size=1) + 
      labs(title = 'Years of Teaching Experience for U.S. Teachers in Private Schools from 1987-2018',
           x = 'Year',
           y = 'Percentage of Teachers', 
           caption = 'Source: U.S. Department of Education, National Center for Education Statistics') +
      theme_bw()
  })
  
  output$all_years_plot <- renderPlot({
    
    data <- switch(input$plot_type_years, 
                           "a" = years_total %>%
                             filter(teacher_char == "Less than 3"),
                           "b" = years_total %>%
                             filter(teacher_char == "3 to 9"),
                           "c" = years_total %>%
                             filter(teacher_char == "10 to 20"),
                           "d" = years_total %>%
                             filter(teacher_char == "Over 20"))
    
    ggplot(data, aes(x = year, y = percentage, color = type)) + 
      geom_point(size = 2) + 
      geom_errorbar(aes(x = year, ymin = min, ymax = max), 
                    width=0.1, alpha=0.9, size=1) + 
      labs(title = 'Years of Teaching Experience for U.S. Teachers from 1987-2018',
           x = 'Year',
           y = 'Percentage of Teachers', 
           caption = 'Source: U.S. Department of Education, National Center for Education Statistics') +
      theme_bw()
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
      scale_fill_manual(
        name = 'Teacher Qualification',
        labels = c(
          'Certification and Related Major',
          'Certification Only',
          'Related Major Only',
          'Neither Qualification'
        ),
        values = c("steelblue", "steelblue2", "steelblue3", "steelblue4")
      ) +
      theme_bw() +
      labs(x = 'Class Subject',
           y = 'Percentage of Students',
           caption = 'Source: U.S. Department of Education, National Center for Education Statistics')

    
  })
  
  output$state_degree_plot <- renderPlot({
    # Generate type based on input$plot_type from ui
    
    data <- switch(input$var_degree, 
                   "a" = state_degree$less,
                   "b" = state_degree$bachelor,
                   "c" = state_degree$master,
                   "d" = state_degree$eddoc)
    
    color <- switch(input$var_degree, 
                    "a" = "darkgreen",
                    "b" = "black",
                    "c" = "darkorange",
                    "d" = "darkviolet")
    
    legend <- switch(input$var_degree, 
                     "a" = "% with Less than Bachelor's",
                     "b" = "% with Bachelor's",
                     "c" = "% with Master's",
                     "d" = "% with Education Specialist or Doctor's")

    percent_map(data, color, legend)
    
  })
  
  output$state_years_plot <- renderPlot({
    # Generate type based on input$plot_type from ui
    
    data <- switch(input$var_years, 
                   "a" = state_years$`Less than 3`,
                   "b" = state_years$`3 to 9`,
                   "c" = state_years$`10 to 20`,
                   "d" = state_years$`Over 20`)
    
    color <- switch(input$var_years, 
                    "a" = "darkgreen",
                    "b" = "black",
                    "c" = "darkorange",
                    "d" = "darkviolet")
    
    legend <- switch(input$var_years, 
                     "a" = "% with Less than 3 years",
                     "b" = "% with 3-9 years",
                     "c" = "% with 10-20 years",
                     "d" = "% with Over 20 years")
    
    percent_map(data, color, legend)
    
  })
  
  output$model_plot <- renderPlot({
    
    #data <- private_teachers
    
    ifelse(input$var == 'a', 
           x <- public_teachers,
           x <- private_teachers)
    
    fit <- lm(teachers ~ year, x)
    
    ggplot(x, aes(x = year, y = teachers)) + 
      geom_point() + 
      geom_line(aes(x = year, y = fitted(fit)), color = 'blue') +
      theme_bw() + 
      labs(x = 'Year', y = 'Number of Teachers (in thousands)',
           title = 'Number of Teachers Between 1987 and 2018',
           subtitle = 'The number of teachers has been increasing in a linear fashion over the 30 year range.',
           caption = 'Source: U.S. Department of Education, National Center for Education Statistics')
  }
  )
  
  # output$my_table <- DT::renderDataTable({
  #   public_age
  # })

}

# Run the application 
shinyApp(ui = ui, server = server)
