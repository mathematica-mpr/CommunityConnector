# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
  theme = "style.css",
  # padding such that nav bar does not cover body 
  tags$head(tags$style(HTML("body {padding-top: 70px;}"))),
  # hack to hide ghost first tab that appears when include title 
  tags$head(tags$style(HTML('#parenttabs > li:first-child { display: none; }'))),
  tags$head(tags$style(HTML('.navbar-default .navbar-brand {color: #000000;}'))),
  tags$head(tags$style(HTML('.navbar-default .navbar-brand:focus, .navbar-default .navbar-brand:hover {color: #000000;}'))),
  navbarPage(useShinyjs(), id = 'parenttabs', 
             position = 'fixed-top',
             selected = "landing",
             title = "Community Connector",
             # landing page --------------------------------------------------------------------------------------------
             tabPanel(icon("info-circle", lib = "font-awesome"), value = "landing",
                      tags$head(tags$style(HTML('.navbar-default{background-color:#f7f4ec}
                                                                                       .navbar-default .navbar-nav>li>a{
                                                                                       color: #046B5C;
                                                                                       font-size:24px;
                                                                                       font-weight:600;}

                                                                                       .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus,
                                                                                       .navbar-default .navbar-nav>li>a:focus{
                                                                                       color: #046B5C;
                                                                                       background-color: #f7f4ec;
                                                                                       font-size:24px;
                                                                                       font-weight:600
                                                                                       }

                                                                                       .navbar-default .navbar-nav>.active>a:hover,.navbar-default .navbar-nav>li>a:hover{color: #000000;
                                                                                       background-color:#e0d4b5;
                                                                                       font-size:24px;
                                                                                       font-weight:600 }
                                                                                       
                                                                                       .green-button {
                                                                                       background-color: #17A673;
                                                                                       color: #ffffff;
                                                                                       border-color: #ccc
                                                                                       }

                                                                                       .green-button:hover, .green-button:active, .green-button:focus, .green-button:visited, .green-button:focus-within{
                                                                                       background-color: #046B5C;
                                                                                       color: #ffffff;
                                                                                       }
                                                                                       
                                                                                       .multicol {
                                                                                       -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                                                                       -moz-column-count: 2;    /* Firefox */ 
                                                                                       column-count: 2; 
                                                                                       -moz-column-fill: auto;
                                                                                       -column-fill: auto;
                                                                                       }
                                                                                      
                                                                                       .btn-primary.active.focus, .btn-primary.active:focus, .btn-primary.active:hover, .btn-primary:active.focus, .btn-primary:active:focus, .btn-primary:active:hover, .open>.dropdown-toggle.btn-primary.focus, .open>.dropdown-toggle.btn-primary:focus, .open>.dropdown-toggle.btn-primary:hover {
                                                                                        color: #fff;
                                                                                        background-color: #046B5C;
                                                                                        border-color: #046B5C;
                                                                                       }'))),
                      tags$script(HTML("var header = $('.navbar > .container-fluid');
                                             header.append('<div style=\"float:right\"><img src = logo.png height=50px></img></div>');"
                      )),
                      fluidPage(
                        fluidRow(
                          column(width = 12, align = "center",
                                 wellPanel(h1(lang_cfg$welcome), 
                                           h4(HTML(lang_cfg$intro)),
                                           actionButton(inputId = "fromlandingtoapp", label = "Go to app",
                                                        class = "btn-primary btn-lg green-button",
                                                        width = '100%'),
                                           style = paste0("background: ",config$colors$tan25)))
                        ),
                        fluidRow(
                          column(width = 12, align = "center",
                                 h4(HTML(lang_cfg$landing$browser)))
                        ),
                        # upper row
                        fluidRow(
                          HTML('<center><img style=\"min-width:900px;\" src="landing_page.jpg" width= "75%"></center>')
                        ),
                        br(),
                        # go to app button at bottom
                        fluidRow(
                          column(width = 12, align = "center",
                                 actionButton(inputId = "fromlandingtoapp2", label = "Go to app",
                                              class = "btn-primary btn-lg green-button",
                                              width = '100%'))
                        )
                      )
                      
             ),
             # main page with tool --------------------------------------------------------------------------------------------------
             tabPanel("Main Page", value = "main_page",
                      fluidPage(
                        fluidRow(
                          column(width = 2,
                                 wellPanel(
                                   h2("Get Started"),
                                   uiOutput('select_my_county'),
                                   uiOutput('select_comparison_county'),
                                   h6(em(lang_cfg$titles$comparison_county_footnote)),
                                   style = paste0("background: ",config$colors$grey25)),
                                 column(width = 12,
                                        actionButton("method_read_more", 
                                                     label = lang_cfg$titles$method_read_more,
                                                     style = paste0("color: ", config$colors$accent,
                                                                    "; background-color: ", config$colors$white100,
                                                                    "; border-color: ", config$colors$accent)),
                                        br(),
                                        br(),
                                        actionButton("data_read_more", 
                                                     label = lang_cfg$titles$data_read_more,
                                                     style = paste0("color: ", config$colors$accent,
                                                                    "; background-color: ", config$colors$white100,
                                                                    "; border-color: ", config$colors$accent)),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        HTML(lang_cfg$titles$external_links),
                                        uiOutput("health_plans_url"),
                                        uiOutput("diab_prev_prog"),
                                        uiOutput("github")
                                 )
                          ),
                          column(width = 10,
                                 column(width = 6, 
                                        fluidRow(
                                          column(width = 12, align = "center", htmlOutput("my_county_header"))),
                                        fluidRow(
                                          column(width = 12, align = "center",
                                                 uiOutput('radar_read_button'),
                                                 br(),
                                                 br(),
                                                 br())),
                                        fluidRow(
                                          column(width = 12, plotlyOutput("my_county_radar",
                                                                          height = "80%") %>%
                                                   withSpinner(type = getOption("spinner.type", default = 1),
                                                               color = getOption("spinner.color", default = "#046B5C"))
                                          ))),
                                 column(width = 6,
                                        tabsetPanel(type = 'pills', id = "tabs",
                                                    tabPanel(span(HTML("<br/><center>Demographics</center>"), title = lang_cfg$demographics_tab),
                                                             fluidRow(
                                                               column(width = 12, h1(" "))
                                                             ),
                                                             fluidRow(column(width = 12, 
                                                                             uiOutput('essentials_tables')
                                                                             
                                                             ))
                                                    ),
                                                    tabPanel(span(HTML("<center>Social Determinants<br/>of Health</center>"), title = lang_cfg$sdoh_tab),
                                                             fluidRow(
                                                               column(width = 12, h1(" "))
                                                             ),
<<<<<<< HEAD
                                                             fluidRow(column(width = 12,
                                                                             h4(lang_cfg$titles$sdoh_table_title, align = "center"),
                                                                             h5(HTML("<b>Filter by categories:</b>")),
                                                                             tags$div(align = 'left', 
                                                                                      class = 'multicol',
                                                                                      uiOutput('demo_tables_header')),
=======
                                                             fluidRow(column(width = 12, 
                                                                             uiOutput('demo_tables_header'),
>>>>>>> e17e743b0cbf6ca3ba39189a54095a730c86b610
                                                                             uiOutput('demo_tables')
                                                                             
                                                             ))
                                                    ),
                                                    tabPanel(span(HTML("<center>My Most Similar<br/>Counties*</center>"), title = lang_cfg$my_matches_tab),
                                                             fluidRow(
                                                               column(width = 12, h1(" "))
                                                             ),
                                                             uiOutput('comp_radar_header'),
                                                             br(),
                                                             plotlyOutput("compare_county_radars"
                                                             ) %>% 
                                                               withSpinner(type = getOption("spinner.type", default = 1),
                                                                           color = getOption("spinner.color", default = "#046B5C")),
                                                             br()
                                                    ),
                                                    tabPanel(span(HTML("<br/><center>County Map</center>"), title = lang_cfg$map_tab),
                                                             fluidRow(
                                                               column(width = 12, h1(" "))
                                                             ),
                                                             fluidRow(leafletOutput("map") %>% 
                                                                        withSpinner(type = getOption("spinner.type", default = 1),
                                                                                    color = getOption("spinner.color", default = "#046B5C"))
                                                             )
                                                    ),
                                                    tabPanel(span(HTML("<br/><center>Health Outcomes</center>"), title = lang_cfg$health_outcomes_tab),
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
  )
  
  
)