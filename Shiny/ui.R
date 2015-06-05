
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)



shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Summarize the logs in Graphtec GL220/820 with Shiny"),

  # Sidebar with a slider input for number of bins
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  '.XLS',
                  '.XLSX',
                  '.xls',
                  '.xlsx'
                )),
      selectInput("GL_id", label = "GL-ID", selected = "B",
                  choices = c("A" = "A (chamA-F)", "B", "C", "D")),
      dateRangeInput("FromTo", label = "Log span", format = "yymmdd",
                     start = "2015-04-030", end = "2015-05-10",
                     min = Sys.Date() - 365, max = Sys.Date()),
      submitButton()
    ),
#      sliderInput("From", "Start Date:",
#                  min = 1, max = 1000, value = 10),
#       sliderInput("To", "End Date:",
#                 min = 1, max = 1000, value = 100)
#   ),
    # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot"),
    tableOutput("contents")
    )
))
