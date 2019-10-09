# Define UI for app that draws a histogram ----
ui <- fluidPage(#theme = "styles.css",
  setBackgroundColor(config$colors$tan),
  
  fluidRow(
    column(width = 4, h2("Community Connector")),
    column(width = 6,
           HTML("Discover communities that are similar to yours.<br>
                   Understand how you compare in health outcomes and utilzation.<br>
                   See what your peers do differently."))),
  fluidRow(
    column(width = 4,
           selectInput('county_selection_type', label = 'Select your county by',
                       choices = c('FIPS Code' = 'FIPS', 'County Name' = 'name'), selected = 'FIPS'),
           searchInput('county_selection', label = '', placeholder = 'Search your county',
                       btnSearch = icon('search'))),
    column(width = 4),
    column(width = 4)
  ),
  
  fluidRow(
    column(width = 4),
    column(width = 4),
    column(width = 4)
  )
  

)