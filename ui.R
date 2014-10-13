library(shiny)
library(knitr)

# Define UI for application
shinyUI(pageWithSidebar(
  
  ## Application title
  headerPanel("The letterator!"),
  
  ## Sidebar panel
  sidebarPanel("This app automatically generates a letter to the editor",
               br(),
               br(),
               "The source code is available on GitHub:",
               a(href = "https://github.com/joebrew/fakepaper",
                 "https://github.com/joebrew/fakepaper")
  ),
  
  ## Main panel
  mainPanel(
    wellPanel(
      textInput("letter_title",
                "Title of your letter:",
                "Doritos aren't the new cigarette: you are."),
      selectInput("sentiment", "What 'flavor' is your letter?",
                  c("Negative" = "negative",
                    "Positive" = "positive")),
      textInput("title", "Article title",
                "Doritos: the new cigarette?"),
      textInput("author", "Article authors",
                "Brew et al"),
      textInput("to", "To whom are you addressing this letter?",
                "most esteemed editors"),
      textInput("journal","Journal",
                "Northern Alachua County Journal of Epidemiology and Drag Racing"),
      textInput("letter_writer","Author of the letter",
                "Diana Rojas"),
      textInput("writer_position", "Description of letter writer",
                "Investigadora extraordinaria")),
    downloadButton("downloadPDF", "Download your letter!")
    #downloadButton("downloadHTML", "Download shiny HTML report")
  )
))
