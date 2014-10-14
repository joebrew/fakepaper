library(shiny)
library(knitr)

# Define UI for application
shinyUI(pageWithSidebar(
  
  ## Application title
  headerPanel("The letterator!"),
  
  ## Sidebar panel
  sidebarPanel("The letterator automatically generates a epidemiology-related letter to the editor.",
               br(),
               br(),
               "This app is based on the",
               a(href = "http://en.wikipedia.org/wiki/Infinite_monkey_theorem",
                 "infinite monkey theorem,"),
               "which posits that an infinite amount of random input will eventually lead to any specific output.",
               "In this case, the input is random",
               a(href = "https://github.com/joebrew/fakepaper/blob/master/helper.R",
                 "(but somewhat structured)"),
               "words, and the output is a perfect letter to the editor.",
               br(),
               br(),
               "Try it out, and if the first go doesn't generate that perfect paper, fear not -",
               "you've got infinity on your side.",
               "Just try again (and again, and again), and eventually you'll get that perfect (publishable) letter, guarnteed.",
               br(),
               br(),
               
               "The source code is available on ",
               a(href = "https://github.com/joebrew/fakepaper",
                 "GitHub."),
               br(),
               br(),
               "The generation of random words relies on Harvard's",
               a(href = "http://www.wjh.harvard.edu/~inquirer/spreadsheet_guide.htm",
                 "General Inquirer project."),
               br(),
               br(),
               
               "A debt of gratitude is owed to",
               a(href = "https://github.com/brechtdv",
                 "Brecht Devleesschauwer"),
               "who unwittingly wrote some of the code that makes the letterator possible."
  ),

  
  ## Main panel
  mainPanel(
    wellPanel(
      textInput("letter_title",
                "Title of your letter:",
                "Doritos aren't the new cigarette: you are"),
      selectInput("sentiment", "What 'flavor' is your letter?",
                  c("Negative" = "negative",
                    "Positive" = "positive")),
      textInput("title", "Article title",
                "Doritos: the new cigarette"),
      textInput("author", "Article authors",
                "Brew et al"),
      textInput("to", "To whom are you addressing this letter?",
                "most esteemed editors"),
      textInput("journal","Journal",
                "Northern Alachua County Journal of Epidemiology and Drag Racing"),
      textInput("letter_writer","Author of the letter",
                "Jacob Ball")),
#       textInput("writer_position", "Description of letter writer",
#                 "Investigadora extraordinaria")),
    downloadButton("downloadPDF", "Download your letter!")
    #downloadButton("downloadHTML", "Download shiny HTML report")
  )
))
