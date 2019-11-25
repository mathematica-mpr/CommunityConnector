# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
 # style = "padding-right: 1%; padding-left: 1%;",
  fluidRow(
    column(width = 3, h1(lang_cfg$title)),
    column(width = 4,
           h4(HTML(lang_cfg$intro))),
    column(width = 2, align = "right",
           actionBttn("method_read_more", 
                      label = lang_cfg$titles$method_read_more,
                      size = "xs", color = "success",
                      style = "minimal"),
           actionBttn("data_read_more", 
                      label = lang_cfg$titles$data_read_more,
                      size = "xs", color = "success",
                      style = "minimal")
           ),
    
    column(width = 3, align = "right", uiOutput("logo"))),
  fluidRow(
    sidebarPanel(width = 2,
                 h2("Get Started"),
                 selectInput('county_selection_type', label = lang_cfg$titles$county_selection_type,
                             choices = c('FIPS Code' = 'fips', 'County Name' = 'name'), selected = 'fips'),
                 uiOutput('select_my_county'),
                 uiOutput('select_comparison_county'),
                 br()),
    mainPanel(width = 10,
              column(width = 6, 
                     fluidRow(
                       column(width = 12, h1(" "))
                     ), 
                     fluidRow(
                       column(width = 12, h1(" "))
                     ),
                     fluidRow(
                       column(width = 12, align = "center", htmlOutput("my_county_header"))),
                     fluidRow(
                       column(width = 12, align = "right",
                              actionBttn("radar_read_more", 
                                         label = lang_cfg$titles$radar_read_more,
                                         size = "sm", color = "success",
                                         style = "pill"))),
                     fluidRow(
                       column(width = 12, plotlyOutput("my_county_radar",
                                                       height = "80%"
                       )))),
              column(width = 6,
                     #style = "max-height: 80vh; overflow-y: auto;",
                     
                     # css styles for tab colors
                     tags$style(HTML("
        .tabbable > .nav > li > a {
           background-color: #28b78d;
           color: #FFF;
      }")),
                     tags$style(HTML("
        .nav-pills>li.active>a, .nav-pills>li.active>a:focus, .nav-pills>li.active>a:hover{
           background-color: #046B5C;
                                     color: #FFF;
                                       }")),
             
          
                     tabsetPanel(type = 'pills', id = "tabs",
                                 tabPanel(span("My Most Similar Counties", title = lang_cfg$my_matches),
                                        #  fluidRow(
                                        #    column(width = 12, h1(" "))
                                        #  ),
                                        #  fluidRow(align = "center", uiOutput("comp_radar_header")),
                                          plotlyOutput("compare_county_radars",
                                                       height = "600px"
                                                       ),
                                          br()),
                                 tabPanel(span("Demographics", title = lang_cfg$demographics),
                                          fluidRow(
                                            column(width = 12, h1(" "))
                                          ),
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
                                          fluidRow(
                                            column(width = 12, h1(" "))
                                          ),
                                          fluidRow(uiOutput('health_outcomes_header')),
                                          fluidRow(
                                            div(id = "density_plot_container",
                                                uiOutput(outputId = "density_graphs_ui")))),
                                 tabPanel(span("County Map", title = lang_cfg$map),
                                          fluidRow(
                                            column(width = 12, h1(" "))
                                          ),
                                          fluidRow(leafletOutput("map")))
                     )
              )
    )
  )
)
