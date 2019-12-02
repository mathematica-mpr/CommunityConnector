# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
  theme = "style.css",
  fluidRow(
    column(width = 2, align = "left"),
    column(width = 8, align = 'center',
           wellPanel(h1(lang_cfg$welcome), 
                     h4(HTML(lang_cfg$intro)),
                     actionButton("method_read_more", 
                                label = lang_cfg$titles$method_read_more,
                                style = paste0("color: ", config$colors$purple100,
                                               "; background-color: ", config$colors$tan25,
                                               "; border-color: ", config$colors$purple100)
                                ),
                     style = paste0("background: ",config$colors$tan25))),
    column(width = 2, align = "right", uiOutput("logo"))
  ),
  fluidPage(
    fluidRow(
      column(width = 2,
             wellPanel(
               h2("Get Started"),
               radioButtons('county_selection_type', label = lang_cfg$titles$county_selection_type,
                           choices = c('County Name' = 'name', 'FIPS Code' = 'fips'), selected = 'name'),
               uiOutput('select_my_county'),
               uiOutput('select_comparison_county'),
               h6(em(lang_cfg$titles$comparison_county_footnote)),
               style = paste0("background: ",config$colors$grey25)),
             actionButton("data_read_more", 
                          label = lang_cfg$titles$data_read_more,
                          style = paste0("color: ", config$colors$purple100,
                                         "; background-color: ", config$colors$white100,
                                         "; border-color: ", config$colors$purple100)),
             br(),
             br(),
             wellPanel(
               tags$style(HTML("
          .selectize-control.single .selectize-input:after{
          visibility:hidden;
          }")),
               uiOutput("health_plans_url"),
               uiOutput("diab_prev_prog"),
               style = paste0("background: ",config$colors$white100,
                              "; background-color: ", config$colors$white100,
                              "; border-color: ", config$colors$purple100)
             )
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
                      column(width = 12, align = "center",
                             actionButton("radar_read_more", 
                                        label = lang_cfg$titles$radar_read_more,
                                        size = "sm",
                                        style = paste0("color: ", config$colors$white100,
                                                       "; background-color: ", config$colors$purple100,
                                                       "; border-color: ", config$colors$purple100)))),
                    fluidRow(
                      column(width = 12, plotlyOutput("my_county_radar",
                                                      height = "80%") %>%
                               withSpinner(type = getOption("spinner.type", default = 1))
                      ))),
             column(width = 6,
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
                                tabPanel(span("My Most Similar Counties*", title = lang_cfg$my_matches_tab),
                                         fluidRow(
                                           column(width = 12, h1(" "))
                                         ),
                                         fluidRow(lang_cfg$my_matches, align = "center"),
                                         br(),
                                         plotlyOutput("compare_county_radars"
                                         ) %>% 
                                           withSpinner(type = getOption("spinner.type", default = 1))
                                         ,
                                         br()
                                ),
                                tabPanel(span("County Map", title = lang_cfg$map_tab),
                                         fluidRow(
                                           column(width = 12, h1(" "))
                                         ),
                                         fluidRow(leafletOutput("map") %>% 
                                                  withSpinner(type = getOption("spinner.type", default = 1))
                                         ),
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