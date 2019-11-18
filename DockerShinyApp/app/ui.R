# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
 # style = "padding-right: 1%; padding-left: 1%;",
  fluidRow(
    column(width = 4, h2("Community Connector")),
    column(width = 3,
           HTML(lang_cfg$intro)),
    column(width = 3),
    column(width = 2, uiOutput("logo"))),
  fluidRow(
    sidebarPanel(width = 2,
                 h3("Get Started"),
                 selectInput('county_selection_type', label = 'Select your county by',
                             choices = c('FIPS Code' = 'fips', 'County Name' = 'name'), selected = 'fips'),
                 searchInput('county_selection', label = '', placeholder = 'Search your county',
                             btnSearch = icon('search'), value = "8001"),
                 textOutput('county_selection_message'),
                 br(),
                 HTML(lang_cfg$howto)),
    mainPanel(width = 10, 
              column(width = 6, 
                     fluidRow(
                       column(width = 6, htmlOutput("my_county_name"))),
                     fluidRow(
                       column(width = 6, 
                              uiOutput('select_comparison_county'))),
                     fluidRow(
                       column(width = 12, plotlyOutput("my_county_radar",
                                                      height = "80%"
                                                      )))),
              column(width = 6,
                     #style = "max-height: 80vh; overflow-y: auto;",
                     tabsetPanel(type = 'pills',
                                 tabPanel("My Matches", 
                                          plotlyOutput("compare_county_radars",
                                                       height = "600px"
                                                       ),
                                          br(),
                                          HTML("<center>ES: Economic Stability, NEP: Neighborhood & Physical Environment, <br>
                                               E = Education, F = Food, C = Community, HC = Health Coverage </center>")),
                                 tabPanel("Demographics",
                                          fluidRow(column(width = 12, DT::DTOutput("my_county_demo"))),
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
                                 tabPanel("Health Outcomes",
                                          fluidRow(uiOutput('health_outcomes_header')),
                                          fluidRow(
                                            div(id = "density_plot_container",
                                                uiOutput(outputId = "density_graphs_ui")))),
                                 tabPanel("County Map",
                                          fluidRow(leafletOutput("map")))
                     )
              )
    )
  )
)
