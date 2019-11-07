# Define UI for app that draws a histogram ----
ui <- fluidPage(#theme = "styles.css",
  
  fluidRow(
    column(width = 4, h2("Community Connector")),
    column(width = 6,
           HTML("Discover communities that are similar to yours.<br>
                   Understand how you compare in health outcomes and utilzation.<br>
                   See what your peers do differently.")),
    column(width = 2, uiOutput("logo"))),
  fluidRow(
    column(width = 2,
           h3("Get Started"),
           selectInput('county_selection_type', label = 'Select your county by',
                       choices = c('FIPS Code' = 'fips', 'County Name' = 'name'), selected = 'fips'),
           searchInput('county_selection', label = '', placeholder = 'Search your county',
                       btnSearch = icon('search'), value = "8001"),
           textOutput('county_selection_message')),
    column(width = 5, style = "max-height: 50vh; overflow-y: auto;",
           fluidRow(htmlOutput("my_county_name")),
           fluidRow(
             column(width = 6, plotOutput("my_county_radar")),
             column(width = 6, 
                    DT::DTOutput("my_county_demo"))
          # fluidRow(d3Output("test"))
           ),
          fluidRow(
            column(width = 6, DT::DTOutput('my_county_econ_stab')),
            column(width = 6, DT::DTOutput('my_county_neigh'))
          ),
          fluidRow(
            column(width = 6, DT::DTOutput('my_county_edu')),
            column(width = 6, DT::DTOutput('my_county_food'))
          ),
          fluidRow(
            column(width = 6, DT::DTOutput('my_county_community')),
            column(width = 6, DT::DTOutput('my_county_health'))
          )),
    column(width = 5, style = "max-height: 50vh; overflow-y: auto;",
           fluidRow(uiOutput('select_comparison_county')),
           fluidRow(
             column(width = 6, plotOutput('comp_county_radar')),
             column(width = 6, DT::DTOutput('comp_county_demo'))
           ),
           fluidRow(
             column(width = 6, DT::DTOutput('comp_county_econ_stab')),
             column(width = 6, DT::DTOutput('comp_county_neigh'))
           ),
           fluidRow(
             column(width = 6, DT::DTOutput('comp_county_edu')),
             column(width = 6, DT::DTOutput('comp_county_food'))
           ),
           fluidRow(
             column(width = 6, DT::DTOutput('comp_county_community')),
             column(width = 6, DT::DTOutput('comp_county_health'))
           ))
  ),
  
  fluidRow(
    column(width = 4, style = "max-height: 50vh; overflow-y: auto;",
           fluidRow(uiOutput('map_header')),
           fluidRow(plotlyOutput("map"))),
    column(width = 4, style = "max-height: 50vh; overflow-y: auto;",
           fluidRow(uiOutput('my_matches_header')),
           fluidRow(plotOutput("compare_county_radars"))),
    column(width = 4, style = "max-height: 50vh; overflow-y: auto;",
           fluidRow(uiOutput('health_outcomes_header')),
           fluidRow(
             div(id = "density_plot_container",
                 uiOutput(outputId = "density_graphs_ui"))))
  ),
  
 # fluidRow(
 #   column(width = 4,
 #          div(id = "density_plot_container",
 #              uiOutput(outputId = "density_graphs_ui")))
 # )
  

)