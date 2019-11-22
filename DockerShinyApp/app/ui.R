# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
 # style = "padding-right: 1%; padding-left: 1%;",
  fluidRow(
    column(width = 4, h2(lang_cfg$title)),
    column(width = 3,
           HTML(lang_cfg$intro)),
    column(width = 3),
    column(width = 2, align = "right", uiOutput("logo"))),
  fluidRow(
    sidebarPanel(width = 2,
                 h3("Get Started"),
                 selectInput('county_selection_type', label = lang_cfg$titles$county_selection_type,
                             choices = c('FIPS Code' = 'fips', 'County Name' = 'name'), selected = 'fips'),
                 searchInput('county_selection', label = '', placeholder = 'Search your county',
                             btnSearch = icon('search'), value = "8001"),
                 uiOutput('select_comparison_county'),
                 br(),
                 HTML(lang_cfg$howto)),
    mainPanel(width = 10, 
              column(width = 6, 
                     fluidRow(
                       column(width = 6, htmlOutput("my_county_name"))),
                     fluidRow(
                       column(width = 12, plotlyOutput("my_county_radar",
                                                      height = "80%"
                                                      )))),
              column(width = 6,
                     #style = "max-height: 80vh; overflow-y: auto;",
      #               tags$style(HTML("
      #  .tabbable > .nav > li > a {
      #     background-color: #000;
      #     color: #FFF;
      #  }")),
                     tabsetPanel(type = 'pills', id = "tabs",
                                 tabPanel(span("My Matches", title = lang_cfg$my_matches),
                                          plotlyOutput("compare_county_radars",
                                                       height = "600px"
                                                       ),
                                          br()),
                                 tabPanel(span("Demographics", title = lang_cfg$demographics),
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
                                 tabPanel(span("Health Outcomes", title = lang_cfg$health_outcomes),
                                          fluidRow(uiOutput('health_outcomes_header')),
                                          fluidRow(
                                            div(id = "density_plot_container",
                                                uiOutput(outputId = "density_graphs_ui")))),
                                 tabPanel(span("County Map", title = lang_cfg$map),
                                          fluidRow(leafletOutput("map")))
                     )
              )
    )
  )
)
