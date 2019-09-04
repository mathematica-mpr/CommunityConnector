library(shiny)
library(shinydashboard)
library(fmsb)

ui <- shinyUI(dashboardPage(
  dashboardHeader(title = "Working on it"),
  dashboardSidebar(),
  dashboardBody(# Boxes need to be put in a row (or column)
    
    # fluidRow(
    #   box(
    #     selectInput(
    #       "School",
    #       "Please select the schools you want to compare",
    #       choices = c("Elementary", "Middle & High")
    #     )
    #   ),
    #
    #   box(
    #     title = "Controls",
    #     sliderInput(
    #       "schoolSize",
    #       "Please filter the schools based upon student population:",
    #       min = 2,
    #       max = 800,
    #       value = c(100, 200),
    #       step = 20
    #     )
    #   )
    # ),
    fluidRow(# box(
      #   selectizeInput("geo_13_14",
      #                  "Select the school, sos my wording:",
      #                  choices = geo_school)
      # ),
      box(
        plotOutput('radarPlot')
      )))
))
server <- shinyServer(function(input, output) {
  output$radarPlot <- renderPlot({
    # Create data: note in High school for several students
    set.seed(99)
    data = as.data.frame(matrix(sample(0:20 , 15 , replace = F) , ncol = 5))
    colnames(data) = c("math" , "english" , "biology" , "music" , "R-coding")
    rownames(data) = paste("mister" , letters[1:3] , sep = "-")
    
    # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
    data = rbind(rep(20, 5) , rep(0, 5) , data)
    
    colors_border = c(rgb(0.2, 0.5, 0.5, 0.9),
                      rgb(0.8, 0.2, 0.5, 0.9) ,
                      rgb(0.7, 0.5, 0.1, 0.9))
    colors_in = c(rgb(0.2, 0.5, 0.5, 0.4),
                  rgb(0.8, 0.2, 0.5, 0.4) ,
                  rgb(0.7, 0.5, 0.1, 0.4))
    radarchart(
      data  ,
      axistype = 1 ,
      #custom polygon
      pcol = colors_border ,
      pfcol = colors_in ,
      plwd = 4 ,
      plty = 1,
      #custom the grid
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      caxislabels = seq(0, 20, 5),
      cglwd = 0.8,
      #custom labels
      vlcex = 0.8
    )
    legend(
      x = 0.7,
      y = 1,
      legend = rownames(data[-c(1, 2), ]),
      bty = "n",
      pch = 20 ,
      col = colors_in ,
      text.col = "grey",
      cex = 1.2,
      pt.cex = 3
    )
  })
})

# Run the application
shinyApp(ui = ui, server = server)