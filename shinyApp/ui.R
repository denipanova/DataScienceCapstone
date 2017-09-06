library(shiny)
library(wordcloud)
library(data.table)
library(DT)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Predict Your Next Word"),
  
  # Input Box for the sentence
  sidebarPanel(
    h3("Start your sencene here and our algorithm will tell you how it is most likely to finish."),
    numericInput("num_matches", "Number of words to choose from: ", 3,min = 1, max = 10),
    textInput("mysentence","My Sentence","To"),
    submitButton("Predict")
    ),
  
  # Show the most probable word, a wordcloud and the top matches 
  mainPanel(
    h3("Your Prediction:"),
    tabsetPanel(
      tabPanel('Most probable word:',
               textOutput("most_probable_word")
      ),
      tabPanel('Word Cloud of probable words',
               plotOutput('wc')),
      tabPanel('Give your input',
               DT::dataTableOutput('best_matches_table'),
               h3('Tell us which is the word which you meant and hit the submit button'),
               textInput("whatimeant",'What I meant'),
               submitButton('Submit')),
      tabPanel('Documentation',
                tagList('This application is created as part of the Capstone project for
                        the Data Science Specialization in', 
                        a('Coursera.',href = 'https://www.coursera.org/specializations/jhu-data-science')),
                tagList('The algoritm and code under this platform is available in',
                     a("GitHub",href='https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny')),
                 tagList('The tutorial on how to operate with the application is available',
                         a("here",href='https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny'))
                 
    )
  )
))
)