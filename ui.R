library(shiny)
library(ggplot2)
library(plotly)
source("functions.R")

ui <- fluidPage(
  
  titlePanel("RandVar"),
  
  ##e6ffe6
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
        body {
            background-color: none;
        }
        .container-fluid {
            background-color: none;
        }
    "))
  ),
  
  #Sidebar with a slider input for number of bins 
  sidebarLayout(
    #input sidebar panel elements
    sidebarPanel(
      radioButtons( "randvariable", label = withMathJax("$$Random \\space Variable \\space (X)$$"), choices = c("discrete", "continuous")),
      selectInput("randvalue", label = "$$Random \\space Values \\space Distribution \\space (x_i)$$", choices = NULL),
      selectInput("pdforcdf", label = "$$Distribution \\space Function \\space \\space (DF)$$", choices = NULL, selected = "PDF"),
      uiOutput("dynamicUI"),
      uiOutput("mathEquation")
      #sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
    ),
    #main panel
    mainPanel(
      #panel above the plot will be a conditional every time
      uiOutput("distributionProperties"),
      plotlyOutput("distroPlots")
    )
  )
)

ui
