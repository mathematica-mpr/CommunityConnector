# Define UI for app that draws a histogram ----
ui <- fluidPage(#theme = "styles.css",
  setBackgroundColor(config$colors$tan25),
  
  fluidRow(
    column(width = 4, h2("Community Connector")),
    column(width = 6,
           HTML("Discover communities that are similar to yours.<br>
                   Understand how you compare in health outcomes and utilzation.<br>
                   See what your peers do differently."))),
  fluidRow(
    column(width = 2,
           selectInput('county_selection_type', label = 'Select your county by',
                       choices = c('FIPS Code' = 'fips', 'County Name' = 'name'), selected = 'fips'),
           searchInput('county_selection', label = '', placeholder = 'Search your county',
                       btnSearch = icon('search'), value = default_county),
           textOutput('county_selection_message')),
    column(width = 5,
           fluidRow(htmlOutput("my_county_name")),
           fluidRow(
             column(width = 6, plotOutput("my_county_radar")),
             column(width = 3, 
                    DT::DTOutput("my_county_demo")),
             column(width = 3)),
          # fluidRow(d3Output("test"))
           ),
    column(width = 5)
  ),
  
  fluidRow(
    column(width = 4,
           div(id = "density_plot_container",
               uiOutput(outputId = "density_graphs_ui"))),
    column(width = 4, plotOutput("compare_county_radars")),
    column(width = 4, plotlyOutput("map"))
  ),
  
 # fluidRow(
 #   column(width = 4,
 #          div(id = "density_plot_container",
 #              uiOutput(outputId = "density_graphs_ui")))
 # )
  

)