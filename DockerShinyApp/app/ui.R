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
                       choices = c('FIPS Code' = 'FIPS', 'County Name' = 'name'), selected = 'FIPS'),
           searchInput('county_selection', label = '', placeholder = 'Search your county',
                       btnSearch = icon('search')),
           textOutput('county_selection_message')),
    column(width = 5,
           fluidRow(htmlOutput("my_county_name")),
           fluidRow(
             column(width = 6, plotOutput("my_county_radar")),
             column(width = 3, 
                    DT::DTOutput("my_county_demo")),
             column(width = 3)) #,
         #  fluidRow(d3Output("test"))
           ),
    column(width = 5)
  ),
  
  fluidRow(
    column(width = 4, plotOutput("health_outcomes_density")),
    column(width = 4),
    column(width = 4)
  )
  

)