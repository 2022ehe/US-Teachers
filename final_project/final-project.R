#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# I imported the libraries I need, including gganimate for the animation plot
# and maps and mapproj for my U.S. map plots.

library(shiny)
library(tidyverse)
library(shinythemes)
library(gganimate)
library(maps)
library(mapproj)

# helper.R contains the percent_map function used to plot the U.S. map figures.

source("helper.R")

# I read in the certification datasets.

ms_cert <- readRDS("ms_cert.RDS")
hs_cert <- readRDS("hs_cert.RDS")

# I read in the age datasets and plot.

public_age <- readRDS("public_age.RDS")
private_age <- readRDS("private_age.RDS")
age_total <- readRDS("age_total.RDS")
age_plot <- readRDS("age.RDS")

# I read in the degree datasets.

public_degree <- readRDS("public_degree.RDS")
private_degree <- readRDS("private_degree.RDS")
degree_total <- readRDS("degree_total.RDS")

# I read in the years of teaching experience datasets.

public_years <- readRDS("public_years.RDS")
private_years <- readRDS("private_years.RDS")
years_total <- readRDS("year_total.RDS")

# I read in the degrees earned and years of teaching experience datasets by state.

state_degree <- readRDS("state_degree.RDS")
state_years <- readRDS("state_years.RDS")

# I read in the total number of teachers datasets to be used for my model.

public_teachers <- readRDS("public_teachers.RDS")
private_teachers <- readRDS("private_teachers.RDS")


# This UI defines the user interface for the application.

ui <- navbarPage(
  theme = shinytheme("flatly"),
  "U.S. Teacher Characteristics",
  
  tabPanel("About",
           column(6, 
           h1("Project Background"),
           
           p("One of the most well known aphorisms in education is 
           the 'quality of an education system cannot exceed the quality of its 
           teachers'."),
           
           p("Thus, much has been researched with regard to the competence of 
           teachers and the effectiveness of their teaching. As the world
           races ahead in terms of technology, research, and scientific discovery,
           how has the number and quality of teachers in the U.S. education system
           evolved to keep up with such rapid changes?"),
           
           p("According to the Global Teacher Status Index 2018, the U.S. has not
           caught up in terms of the amount of respect teachers, i.e. the 
           social status associated with the teaching role. As teachers are
           responsible for shaping the future, this is especially concerning."),
          
           h1('The Data'),
           p("For my final project, I use data from the U.S. Department of Education's 
           National Center for Education Statistics and its School and Staffing
           Survey to analyze trends in the quantity and quality of teachers
           across the U.S. and comparing public vs. private schools.")),
           
           column(6, 
                  imageOutput("country_list", height = "100%", width = "100%")),
           
           column(12,
           h1('About Me'),
           p("My name is Emily He and I'm an undergraduate pursuing an S.B. in 
           Bioengineering. Feel free to contact me at ", 
             a("emily_he@college.harvard.edu", 
               href = "mailto: emily_he@college.harvard.edu"),
             ". You can find the code to this project on my  ",
             a("Github", 
               href = "https://github.com/2022ehe/US-Teachers"),
             "."))
  ),
  
  tabPanel("Teacher Age Ranges",
           fluidPage(
             titlePanel("Age of Teachers in U.S. Schools from 1987-2018"),
             fluidRow(
               column(6, 
                      p("Let's first take a look at the age ranges of teachers
                        throughout the years."),
                      p("One would expect the number of
                        teachers to match the fluctuations in U.S. population. 
                        If not, it could indicate teaching is either an 
                        unsatisfactory or extremely appealing position."),
                      p("The U.S. experienced a baby boom, i.e. increase in 
                        birth rate, between 1946 and 1964 as the WWII brought 
                        around an age of prosperity and economic growth. This
                        matches the animation shown on the right, as there is 
                        a peak of teachers in the 40-47 age range during years 
                        1987-90, and a corresponding increase in the number of
                        teachers aged 50-59 in the 1995-2004. The numbers then
                        equilibrate at around 20% in each age range. It should 
                        also be noted that the number of teachers aged 60-69 
                        increases relatively rapidly from 2002-2012 as well."),
                      p("This data suggests that the teaching profession seems
                        to match up roughly with U.S. fluctuations in population,
                        and thus age cannot be used to show any trends about how
                        appealing the teaching profession is, or how there
                        might have been a teacher shortage/surplus."),
                      ),
               column(6, 
                      imageOutput("animated_age_plot", 
                                  height = "100%",
                                  width = "100%"),
                      align = "center"),
               br(),
               br(),
               h3("Explore the Data"),
               column(2,
                      selectInput(
                        "plot_type_age",
                        "Age Range",
                        c("Under 30" = "a", 
                          "30 to 39" = "b", 
                          "40 to 49" = "c",
                          "50 to 59" = "d",
                          "60 and over" = "e"))
                ),
               column(6, 
                      plotOutput("all_age_plot"),
                      align = "center"),
               column(4, 
                      p("The trends between public and private schools also 
                        tend to match each other through the years, but in the
                        60-69 age range, it's consistent throughout all the years
                        that private schools have a higher percentage. This trend
                        of private schools having a higher proportion of older
                        teachers is quite interesting as it could indicate that
                        teachers are satisfied with their career teaching in 
                        a private school and decide to remain until, or even
                        after, retirement age."))
             ))),
  
  tabPanel("Teacher Qualifications",
           fluidPage(
             fluidRow(
               column(12,
                      h2("Percentage of U.S. Students Taught by Teachers with
                        Various Qualifications in 2011-2012"),
                      align = "left"),
               br(),
               column(3,
                      selectInput(
                        "plot_type",
                        "Grade Level",
                        c("Middle School (Grades 6-8)" = "a",
                          "High School (Grades 9-12)" = "b")),
                      align = "left"
               ),
               column(6,
                      plotOutput("cert_plot"),
                      align = "center"),
               column(3,
                      p("Quite a high percentage of middle school students are 
                        taught by teachers who do not have a teaching certification 
                        or a major related to their subject. This is especially 
                        concerning for science subjects as ~60% of science students
                        are taught by teachers with neither qualification. Since 
                        middle school is the time where students lay their 
                        educational foundation and start to develop
                        interests in certain subjects, this information
                        is mildly alarming."),
                      p("The standard is much higher for high school students, 
                        as over 50% of students are taught by teachers who have 
                        certification and/or major related to their teaching 
                        subject. This makes sense as high school subjects 
                        become more specialized and advanced, meaning that 
                        teachers would likely need to be more qualified.")),
               br(),
               column(12,
                      h2("Highest Degree Earned Among U.S. Teachers in 2011-2012"),
                      align = "left"),
               br(),
               column(4, 
                      p("This plot shows the relative percentages of highest
                        degree earned for teachers in each state when compared 
                        to other states. For example, in New York, only 2.815% 
                        of teachers have Less than Bachelor's, but the shade of
                        green is not the lightest, because the maximum value in 
                        any state is 6.978%. Thus, the darkest shade representing
                        100% for Less than Bachelor's indicates 6-7% of teachers. 
                        The degree percentage is normalized among the states for 
                        all the degree options."),
                      p("I expected teachers located on the coast, especially 
                        New York and California to have the highest percentage
                        of higher degrees. I was therefore surprised to see that
                        the darkest shades were not localized around the coasts.
                        It is true that 84% of teachers have a Master's degree 
                        in New York, but California is average (intermediate color
                        shades) for both Master's and Education Specialist/Doctor's.
                        It turns out that Georgia has the highest percentage of
                        teachers with a Education Specialist/Doctorate degree: 
                        23.606%.")),
               column(2,
                      selectInput(
                        "var_degree",
                        "Highest Degree Earned",
                        c("Less than Bachelor's" = "a",
                          "Bachelor's" = "b",
                          "Master's" = "c",
                          "Education Specialist or Doctor's" = "d")),
                      align = "left"
                      ),
               column(6,
                      plotOutput("state_degree_plot"),
                      align = "center"),
               br(),
               column(12,
                      h3('Explore the Data')),
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
               column(6,
                      plotOutput("all_degree_plot"),
                      align = "center"),
               column(3,
                      p("The overall trend in both public and private schools
                        is an increase of more advanced degrees, as evidenced by
                        the increase in the percentage of teachers who have a
                        Master's, Education Specialist, and Doctorate degree."),
                      p("I expected a larger percentage of private school teachers
                        would have higher degrees, and that holds for the Doctorate
                        degree. However, public schools have a consistently higher
                        percentage of teachers with Master and Education Specialist
                        degrees."))
             ))
             ),

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
                      align = "left"
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
               br(),
               column(6,
                      gt_output("model_table_public")),
               column(6,
                      gt_output("model_table_private")),
               br(),
               br(),
               column(12,
                      p("To model the increase of teachers over the last ~30 years,
                        I conducted a linear regression for both teachers in public
                        and private schools. The independent variable in this case
                        would be year, and the dependent would be the number
                        of teachers. The slope, or year coefficient, for public 
                        school teachers is 41.34, indicating an increase of 
                        ~41,340 teachers per year. The slope, or year coeffieicnt, 
                        for private school teachers is 6.340, indicating an increase
                        of ~6,340 teachers per year."))
             )))

  )

# Define server logic required to create all plots

server <- function(input, output) {
  
  # Renders plot of teacher age in public schools
  
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
  
  # Renders plot of teacher age in private schools
  
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
  
  # Renders plot of teacher age in public and private schools
  
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
  
  # Renders animated plot of teacher age in public and private schools throughout 
  # the years
  
  output$animated_age_plot <- renderImage({
    # Return a list containing the filename
    list(src = "age.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = FALSE)
  
  # Renders plot of teacher degrees in public schools
  
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
  
  # Renders plot of teacher degrees in private schools
  
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
  
  # Renders plot of teacher degrees in public and private schools
  
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
  
  # Renders plot of teacher years of experience in public schools
  
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
  
  # Renders plot of teacher years of experience in private schools
  
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
  
  # Renders plot of teacher years of experience in public and private schools 
  
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
        values = c("darkorchid3", "steelblue2", "mediumvioletred", "thistle4")
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
  
  # Renders images on the About tab
  
  output$country_list <- renderImage({
    # Return a list containing the filename
    list(src = "varkey.jpg",
         contentType = 'image/jpg',
         width = 500,
         height = 450,
         alt = "This is alternate text"
    )}, deleteFile = FALSE)
  
  
  output$model_table_public <- render_gt({
    fit_public <- stan_glm(teachers ~ year, data = public_teachers, refresh = 0)

    tbl_regression(fit_public, intercept = FALSE) %>%
      as_gt() %>%
      tab_header(title = "Regression for Public Schools", 
                 subtitle = "The Effect of Year on Number of Teachers in Public Schools") %>%
      tab_source_note("Source: National Center for Educational Statistics")
  })
  
  output$model_table_private <- render_gt({
    fit_private <- stan_glm(teachers ~ year, data = private_teachers, refresh = 0)
    tbl_regression(fit_private, intercept = FALSE) %>%
      as_gt() %>%
      tab_header(title = "Regression for Private Schools", 
                 subtitle = "The Effect of Year on Number of Teachers in Private Schools") %>%
      tab_source_note("Source: National Center for Educational Statistics")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
