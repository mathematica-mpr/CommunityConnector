# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
  # style = "padding-right: 1%; padding-left: 1%;",
  fluidRow(
    column(width = 2, align = "left"),
    column(width = 8, align = 'center',
           wellPanel(h1(lang_cfg$welcome), 
                     h4(HTML(lang_cfg$intro)),
                     style = paste0("background: ",config$colors$tan25))),
    column(width = 2, align = "right", uiOutput("logo"))
  ),
  fluidPage(
    fluidRow(
      column(width = 2,
             wellPanel(
               tags$style(HTML("
          .selectize-control.single .selectize-input:after{
          visibility:hidden;
          }")),
               h2("Get Started"),
               radioButtons('county_selection_type', label = lang_cfg$titles$county_selection_type,
                           choices = c('County Name' = 'name', 'FIPS Code' = 'fips'), selected = 'name'),
               uiOutput('select_my_county'),
               uiOutput('select_comparison_county'),
               h6(em(lang_cfg$titles$comparison_county_footnote)),
               br(),
               actionBttn("method_read_more", 
                          label = lang_cfg$titles$method_read_more,
                          size = "sm", color = "success",
                          style = "minimal"),
               br(),
               br(),
               actionBttn("data_read_more", 
                          label = lang_cfg$titles$data_read_more,
                          size = "sm", color = "success",
                          style = "minimal"),
               
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               uiOutput("health_plans_url"),
               br(),
               br(),
               uiOutput("diab_prev_prog"),
               style = paste0("background: ",config$colors$grey25))
             ),
      column(width = 10,
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
                                                      height = "80%") #%>%
                               #withSpinner(type = getOption("spinner.type", default = 1))
                      ))),
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
                                tabPanel(span("Demographics", title = lang_cfg$demographics_tab),
                                         fluidRow(
                                           column(width = 12, h1(" "))
                                         ),
                                         fluidRow(column(width = 12, 
                                                         uiOutput('demo_tables_header'),
                                                         uiOutput('demo_tables')
                                                        
                                         ))
                                ),
                                tabPanel(span("County Map", title = lang_cfg$map_tab),
                                         fluidRow(
                                           column(width = 12, h1(" "))
                                         ),
                                         fluidRow(leafletOutput("map") #%>% 
                                                    #withSpinner(type = getOption("spinner.type", default = 1))
                                         )
                                ),
                                tabPanel(span("My Most Similar Counties*", title = lang_cfg$my_matches_tab),
                                         fluidRow(
                                           column(width = 12, h1(" "))
                                         ),
                                         fluidRow(lang_cfg$my_matches, align = "center"),
                                         br(),
                                         # fluidRow(align = "center", uiOutput("comp_radar_header")),
                                         plotlyOutput("compare_county_radars"
                                         ) #%>% 
                                           #withSpinner(type = getOption("spinner.type", default = 1))
                                           ,
                                         br()
                                ),
                                tabPanel(span("Health Outcomes", title = lang_cfg$health_outcomes_tab),
                                         fluidRow(
                                           column(width = 12, h1(" "))
                                         ),
                                         fluidRow(uiOutput('health_outcomes_header')),
                                         fluidRow(
                                           div(id = "density_plot_container",
                                               uiOutput(outputId = "density_graphs_ui")))
                                )
                    )
             )
      )
    )
  )
)