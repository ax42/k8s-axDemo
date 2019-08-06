library(shiny)
library(dplyr)
library(ggplot2)
library(httr)
library(base64enc)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("k8s demo app"),
    fluidRow(
        column(2,
               tabsetPanel(type = "tabs",
                   tabPanel("Parameters",
                       sliderInput("numPoints", "Number of points", 100, 6000, 1000, 50, 
                                   animate = animationOptions(100, loop = TRUE)),
                       sliderInput("numCats", "Number of categories", 1, 6, 2, 1),
                       sliderInput("xMean", "x-axis mean", -10, 10, 0, .1),
                       sliderInput("xStdDev", "x-axis std dev", 0, 10, 1, .1),
                       sliderInput("yMean", "y-axis mean", -10, 10, 0, .1),
                       sliderInput("yStdDev", "y-axis std dev", 0, 10, 2, .1)
                   ),
                   tabPanel("API",
                        # checkboxInput("useAPI", label = "Use API", value = FALSE),
                        textInput("apiURL", "API endpoint", "http://localhost:8001/"),
                        # textInput("apiURL", "API endpoint", "http://walker:8001/"),
                        textOutput("apiNodename")
                        # textOutput("apiSum")
                   )
               )
        ),
        column(5,
               h3("Local"),
               tableOutput("summaryTable"),
               plotOutput("dotPlot")
        ),
        column(5,
               h3("API"),
               checkboxInput("useAPITable", label = "Use API", value = FALSE),
               tableOutput("summaryTableAPI"),
               checkboxInput("useAPIPlot", label = "Use API", value = FALSE),
               uiOutput("dotPlotAPI")
        )
    )
)

server <- function(input, output) {
  
    output$summaryTable <- renderTable({
      srcData() %>% 
        group_by(cat) %>% 
        summarise(n = n(),
                  xMean = mean(x), #StdDev = sd(x),
                  yMean = mean(y), #yStdDev = sd(y)
                  )
    })
   
    apiPOST <- function(url, body) {
        resp <- POST(url, body = body, content_type_json(), encode = "json") 
        # print(resp)
        resp
    } 
    
    apiSum <- function(url, body) {
      url$path <- "sum"
      as.numeric(content(apiPOST(build_url(url), body)))
    }
    apiCount <- function(url, body) {
      url$path <- "count"
      as.numeric(content(apiPOST(build_url(url), body)))
    }
    apiMean <- function(url, body) {
      n <- apiCount(url, body)
      s <- apiSum(url, body)
      return(s/n)
    }
    output$summaryTableAPI <- renderTable({
        if (input$useAPITable) {
          url <- parse_url(input$apiURL)
          body = list(d = srcData()$x)
          # resp <- POST(build_url(url), body = body, encode = "json", content_type_json(), verbose())
          srcData() %>% 
            group_by(cat) %>% 
            summarise(n = apiCount(url, list(d = .data$x)),
                      xMean = apiMean(url, list(d = .data$x)),
                      yMean = apiMean(url, list(d = .data$y))
                      )
            
        }
    })
    
    output$dotPlot <- renderPlot({
        # print(srcData())
        ggplot(srcData(), aes(x, y)) + geom_point(alpha = 0.2) +
            facet_wrap(~cat) +
            theme(legend.position = "none")
    })
    
    # output$dotPlotAPI <- renderPlot({
    output$dotPlotAPI <- renderUI({
        if (input$useAPIPlot) {
          url <- parse_url(input$apiURL)
          body <- list(x = srcData()$x, y = srcData()$y, cat = srcData()$cat)
          url$path <- "plot"
          resp <- POST(url, body = body, encode = "json") 
          tags$img(src = paste0("data:image/png;base64,",
                                base64encode(resp[["content"]])))
        }
    })
   
    randomValue <- function(mean, sd) {
        return (rnorm(1, mean, sd))
    } 
    
    srcData <- reactive({
        data.frame(
            x = replicate(input$numPoints, randomValue(input$xMean, input$xStdDev)),
            y = replicate(input$numPoints, randomValue(input$yMean, input$yStdDev)),
            cat = replicate(input$numPoints, sample(1:input$numCats, 1))
        )
    })
    
    output$apiNodename <- renderText({
        # if (input$useAPITable) {
            url <- parse_url(input$apiURL)
            url$path <- "nodename"
            resp <- GET(build_url(url))
            return(paste("node:", content(resp, "text")))
        # } 
    })
    
    output$apiSum <- renderText({
        if (input$useAPITable) {
          url <- parse_url(input$apiURL)
          url$path <- "sum"
          body = list(d = c(1,2,3,4,5))
          resp <- POST(build_url(url), body = body, encode = "json", content_type_json(), verbose())
          return(content(resp, "text"))
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
