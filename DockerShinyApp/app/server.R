server <- function(input, output, session) {
  
  options(DT.options = list(dom = "t", ordering = F))
  
  # hide tabs ------------------------------------------------------------------
  observe({
    hide(selector = "#parenttabs li a[data-value=main_page]")
  })
  
  # move around buttons --------------------------------------------------------
  observeEvent(input$fromlandingtoapp, {
    updateTabsetPanel(session, "parenttabs",
                      selected = "main_page")
  })
  observeEvent(input$fromlandingtoapp2, {
    updateTabsetPanel(session, "parenttabs",
                      selected = "main_page")
  })

  # error handling checks ------------------------------------------------------
  county_check <- reactive({
      input$county_selection %in% dat$county
  })
  
  # reactive values and data frames --------------------------------------------
  county_fips <- reactive({
    req(county_check())
    dat %>% filter(county == input$county_selection) %>% pull(fips)
  })
  
  county_name <- reactive({
    req(county_check())
      input$county_selection
  })
  
  county_state <- reactive({
    req(county_check())
    dat %>% filter(fips == county_fips()) %>% pull(state)
  })
  
  county_dat <- reactive({
    dat %>% filter(fips == county_fips())
  })
  
  comp_county_dat <- reactive({
    req(county_check())
    req(input$comparison_county_selection)
    dat %>% filter(county == input$comparison_county_selection)
  })
  
  comp_county_fips <- reactive({
    req(county_check())
    req(input$comparison_county_selection)
    dat %>% filter(county == input$comparison_county_selection) %>% pull(fips)
  })
  
  # creates list of matched counties
  my_matches <- reactive({
    req(county_check())
    find_my_matches(county_fips(), dat, 20)[[2]] 
  })
  

  
  # outcomes data
  outcomes_dat <- reactive({
    req(county_check())
    
    outcomes_dd <- get_dd(dd, "outcome")
    
    outcomes <- outcomes_dd %>% pull(column_name)
    
    # need to add coloring to the vlines
    df <- dat %>% select(fips, state, county, outcomes) %>%
      pivot_longer(cols = outcomes) %>%
      # selected county and matches to selected county
      mutate(type = case_when(
        fips %in% my_matches() ~ "matches",
        fips == county_fips() ~ "selected",
        TRUE ~ "other"
      )) %>%
      # left join data dictionary to get real outcome names
      rename(column_name = name) %>%
      left_join(outcomes_dd, by = "column_name") 
    df
  })
  
  # output ---------------------------------------------------------------------
  output$select_my_county <- renderUI({
    choice_list <- sort(dat$county)
    selectizeInput('county_selection', label = lang_cfg$titles$county_selection,
                   choices = choice_list)
  })
  
  ## methodology modal dialogue ------------------------------------------------
  methodology_modal <- modalDialog(
    HTML(lang_cfg$method),
    size = "l",
    footer = modalButton("Close"),
    easyClose = T
  )
  
  # modal based on button in main page
  observeEvent(input$method_read_more, {
    showModal(methodology_modal)
  })
  
  # modal based on button in radar modal
  observeEvent(input$method_read_more_in_radar, {
    showModal(methodology_modal)
  })
  
  ## data sources modal dialogue -----------------------------------------------
  observeEvent(input$data_read_more, {
    showModal(modalDialog(
      title = lang_cfg$titles$data_source_modal,
      tagList(
        "Demographic data:",
        tags$br(),
        a("American Community Survey", 
          href = "https://www.census.gov/programs-surveys/acs/news/data-releases.html?#",
          target = "_blank"),
        tags$br(),
        a("Opportunity Atlas", 
          href = "https://www.opportunityatlas.org/",
          target = "_blank"),
        tags$br(),
        a("Robert Wood Johnson Foundation", 
          href = "https://www.countyhealthrankings.org/explore-health-rankings/use-data",
          target = "_blank"),
        tags$br(),
        tags$br(),
        "Social Determinants of Health data:",
        tags$br(),
        a("Area Health Resource Files (AHRF)", 
          href = "https://data.hrsa.gov/topics/health-workforce/ahrf",
          target = "_blank"),
        tags$br(),
        a("Colorado Department of Public Health and Environment", 
          href = "https://www.colorado.gov/pacific/cdphe/data",
          target = "_blank"),
        tags$br(),
        a("Walk Score", 
          href = "https://www.walkscore.com/cities-and-neighborhoods/",
          target = "_blank"),
        tags$br(),
        tags$br(),
        "Outcomes data:",
        tags$br(),
        a("Centers for Disease Control and Prevention's (CDC) Diabetes Atlas, 2016", 
          href = "https://gis.cdc.gov/grasp/diabetes/DiabetesAtlas.html#",
          target = "_blank"),
        tags$br(),
        a("Healthcare Cost and Utilization Project (HCUP), 2016", 
          href = "https://hcupnet.ahrq.gov",
          target = "_blank"),
        tags$br(),
        a("Centers for Medicare and Medicaid Services (CMS), 2017", 
          href = "https://www.cms.gov/Research-Statistics-Data-and-Systems/Research-Statistics-Data-and-Systems",
          target = "_blank"),
        tags$br(),
),
      size = "l",
      footer = modalButton("Close"),
      easyClose = T
    ))
  })
  
  
  output$radar_read_button <- renderUI({
    req(county_check())
    actionButton("radar_read_more", 
                 label = lang_cfg$titles$radar_read_more,
                 size = "sm",
                 style = paste0("color: ", config$colors$accent,
                                "; background-color: ", config$colors$white100,
                                "; border-color: ", config$colors$accent))
  })
  
  ## radar chart description modal dialogue ------------------------------------
  observeEvent(input$radar_read_more, {
    showModal(modalDialog(
      HTML('<center><img src="fingerprint_read_more.jpg" width="100%"></center>'),
      br(),
      actionButton("method_read_more_in_radar", 
                   label = lang_cfg$titles$method_read_more,
                   style = paste0("color: ", config$colors$accent,
                                  "; background-color: ", config$colors$white100,
                                  "; border-color: ", config$colors$accent)),
      size = "l",
      footer = modalButton("Close"),
      easyClose = T
    ))
  })
  
  ## radar chart description modal dialogue ------------------------------------
  observeEvent(input$density_read_more, {
    showModal(modalDialog(
      HTML('<center><img src="density_read_more.jpg" width="100%" max-width="900px"></center>'),
      size = "l",
      footer = modalButton("Close"),
      easyClose = T
    ))
  })
  
  ## local public health plans url ---------------------------------------------
  output$health_plans_url <- renderUI({
    tagList(a(lang_cfg$titles$health_plans, 
              href = "https://www.colorado.gov/pacific/cdphe-lpha/chaps",
              target = "_blank"))
  })
  
  ## diabetes prevention programs url ---------------------------------------------
  output$diab_prev_prog <- renderUI({
    tagList(a(lang_cfg$titles$diab_prev_prog, 
              href = " https://nccd.cdc.gov/DDT_DPRP/CitiesList.aspx?STATE=CO", 
              target = "_blank"))
  })
  
  ## github url ----------------------------------------------------------------
  output$github <- renderUI({
    tagList(a(lang_cfg$titles$github, 
              href = " https://github.com/mathematica-mpr/CommunityConnector", 
              target = "_blank"))
  })
  
  ## selected county information -----------------------------------------------
  output$my_county_header <- renderUI({
    req(county_check())
    HTML(paste0("<h3>Social Determinants of Health Scores <br> for ", county_name(), ", ", county_state(), "</h3>"))
  })
  
  
  output$my_county_radar <- renderPlotly({
    req(county_check())
    req(input$comparison_county_selection)
    
    sdoh_dd <- get_dd(dd, "sdoh_score")
    
    sdohs <- sdoh_dd %>% pull(column_name)
    
    my_county_df <- county_dat() %>% select(county, state, sdohs)
    
    if (input$comparison_county_selection == "None") {
      radar_chart(my_county_df, dd)
    } else {
      comp_county_df <- comp_county_dat() %>% select(county, state, sdohs)
      
      radar_chart_overlay(my_county_df, comp_county_df, dd)
    }
  })
  
  ## demographics tables filter ------------------------------------------------
  output$general_demo_header <- renderUI({
    req(county_check())
    tagList(
      fluidRow(
        column(width = 12, 
               box(align = "center",
                   width = '100%',
                   height = '100%',
                   HTML(lang_cfg$tab_headers$demographics_description),
                   style = paste0("background-color: ", config$colors$greenaccent, "40",
                                  "; border-color: ", config$colors$greenaccent, "40",
                                  "; padding: 10px")),
               br()
        ))
    )
  })
  
  output$demo_tables_header <- renderUI({
    req(county_check())
    tagList(
      fluidRow(
        column(width = 12, 
               box(align = "center",
                   width = '100%',
                   height = '100%',
                         HTML(lang_cfg$tab_headers$sdoh_tab_description),
                         style = paste0("background-color: ", config$colors$greenaccent, "40",
                                        "; border-color: ", config$colors$greenaccent, "40",
                                        "; padding: 10px")),
               br(),
               h5(HTML("<b>Filter by categories:</b>"))
        ))
      )
  })
  
  output$demo_tables_checkboxes <- renderUI({
    req(county_check())
    tagList(
      fluidRow(
        column(width = 12, 
               checkboxGroupInput('demo_filter', label = NULL, 
                                  choices = c('Economic Stability' = 'used_sdoh_1',
                                              'Neighborhood & Physical Environment' = 'used_sdoh_2',
                                              'Education' = 'used_sdoh_3',
                                              'Food' = 'used_sdoh_4',
                                              'Community' = 'used_sdoh_5',
                                              'Health Care System' = 'used_sdoh_6'),
                                  selected = c('used_sdoh_1', 'used_sdoh_2', 'used_sdoh_3',
                                               'used_sdoh_4', 'used_sdoh_5', 'used_sdoh_6'))
        ))
    )
  })
  
  output$map_tab_header <- renderUI({
    req(county_check())
    tagList(
      fluidRow(
        column(width = 12, 
               box(align = "center",
                   width = '100%',
                   height = '100%',
                   HTML(lang_cfg$tab_headers$map_tab_description, lang_cfg$tab_headers$similarity_description),
                   style = paste0("background-color: ", config$colors$greenaccent, "40",
                                  "; border-color: ", config$colors$greenaccent, "40",
                                  "; padding: 10px")),
               br()
        ))
    )
  })
  
  output$outcomes_tab_header <- renderUI({
    req(county_check())
    tagList(
      fluidRow(
        column(width = 12, 
               box(align = "center",
                   width = '100%',
                   height = '100%',
                   HTML(lang_cfg$tab_headers$outcomes_tab),
                   style = paste0("background-color: ", config$colors$greenaccent, "40",
                                  "; border-color: ", config$colors$greenaccent, "40",
                                  "; padding: 10px")),
               br()
        ))
    )
  })
  
  
  ## render demo DTs based on selected input -----------------------------------
  output$demo_tables <- renderUI({
    req(county_check())
    req(input$comparison_county_selection)
    req(input$demo_filter)
    
    demo_tables_list <- lapply(input$demo_filter, function(x) 
      tagList(
        make_demo_dt(county_dat = county_dat(),
                     comp_county_dat = comp_county_dat(),
                     comp_county_select = input$comparison_county_selection,
                     demo_select = x,
                     dd = dd)
      )
    )
    
    tagList(
      demo_tables_list
    )
  })
  
  output$essentials_tables <- renderUI({
    req(county_check())
    req(input$comparison_county_selection)
    
    demo_tables_list <- lapply("demographic", function(x) 
      tagList(
        make_demo_dt(county_dat = county_dat(),
                     comp_county_dat = comp_county_dat(),
                     comp_county_select = input$comparison_county_selection,
                     demo_select = x,
                     dd = dd)
      )
    )
    
    tagList(
      demo_tables_list
    )
  })
  
  ## selected comparison county info -------------------------------------------
  output$select_comparison_county <- renderUI({
    req(my_matches())
    comp_counties <- dat %>% filter(fips %in% my_matches()) %>% pull(county)
    selectInput('comparison_county_selection', label = lang_cfg$titles$comparison_county_selection,
                choices = c("None", comp_counties), selected = "None")
  })
  

  ## comparison counties info --------------------------------------------------
  output$comp_radar_header <- renderUI({
    req(county_check())
    tagList(
      fluidRow(
        column(width = 12,
               box(align = "center",
                   width = '100%',
                   height = '100%',
                   HTML(lang_cfg$tab_headers$matches_tab_description, lang_cfg$tab_headers$similarity_description),
                   style = paste0("background-color: ", config$colors$greenaccent, "40",
                                  "; border-color: ", config$colors$greenaccent, "40",
                                  "; padding: 10px"))
        ))
    )
  })
  
  output$compare_county_radars <- renderPlotly({
    req(county_check())
    
    sdoh_dd <- get_dd(dd, "sdoh_score")
    
    sdohs <- sdoh_dd %>% pull(column_name)
    
    df <- dat %>% select(fips, state, county, sdohs) %>%
      filter(fips %in% my_matches())
    
    grid_radar(df, dd)
  })
  
  output$map_header <- renderUI({
    req(my_matches())
    tagList(
      HTML(paste0("<h3>County Map<br/></h3>"))
    )
  })
  
  output$map <- renderLeaflet ({
    req(county_check())
    req(input$comparison_county_selection)
    
    df <- find_my_matches(county_fips(), dat)[[1]] %>%
        mutate(GEOID = str_pad(fips, width = 5, side = "left", pad = "0")) %>%
        mutate(NAME = str_replace(county, " County", ""))
    
    st_fips <- str_sub(df$GEOID, 1, 2)[1]
    cty_fips <- str_pad(county_fips(), width = 5, side = "left", pad = "0")
    comparison_cty_fips <- str_pad(comp_county_fips(), width = 5, side = "left", pad = "0")
    
    if (input$comparison_county_selection == "None") {
      county_map(df, st_fips, cty_fips)
    } else {
      county_map_comp(df, st_fips, cty_fips, comparison_cty_fips)
    }
    
  })

  # dynamic number of density graphs -------------------------------------------

  output$health_outcomes_header <- renderUI({
    req(county_check())
    tagList(
      fluidRow(
        column(width = 6, 
               checkboxGroupInput('outcome_filter', label = 'Filter by health conditions:', 
                                  choices = c('Diabetes' = 'diab',  
                                              'Kidney Disease' = 'kidney',
                                              'Obesity' = 'obes'),
                                  selected = c('diab', 'kidney', 'obes')
               )
        ),
        column(width = 6, 
               checkboxInput(inputId = 'show_matches', 
                             label = 'Compare to my most similar counties'),
               actionButton("density_read_more", 
                            label = lang_cfg$titles$density_read_more,
                            style = paste0("color: ", config$colors$accent,
                                           "; background-color: ", config$colors$white100,
                                           "; border-color: ", config$colors$accent)),
               value = F)))
  })
  
  
  density_graphs <- eventReactive(
    {outcomes_dat()
     comp_county_dat()
     input$outcome_filter
     input$show_matches
     }, {
    
    if(input$comparison_county_selection == "None") {
      if (!input$show_matches) {
        outcomes_dat() %>%
          group_by(column_name) %>%
          nest() %>%
          # filter by dropdown selection
          filter_category(input$outcome_filter) %>%
          mutate(graphs = purrr::map(data, density_plot)) %>%
          pull(graphs)
      } else {
        outcomes_dat() %>%
          group_by(column_name, higher_better) %>%
          nest() %>%
          # filter by dropdown selection
          filter_category(input$outcome_filter) %>%
          mutate(graphs = purrr::map(data, density_plot_overlay)) %>%
          pull(graphs)
      }
    } else {
      if (!input$show_matches) {
        compare_countyname <- comp_county_dat() %>% 
          pull(county)
        outcomes_dat() %>%
          group_by(column_name) %>%
          nest() %>%
          # filter by dropdown selection
          filter_category(input$outcome_filter) %>%
          mutate(compare_name = compare_countyname) %>% 
          mutate(compare_value = purrr::map2(data, compare_name, get_compare_value)) %>% 
          mutate(graphs = purrr::map2(data, compare_value, density_plot)) %>%
          pull(graphs)
      } else {
        compare_countyname <- comp_county_dat() %>% 
          pull(county)
        outcomes_dat() %>%
          group_by(column_name, higher_better) %>%
          nest() %>%
          # filter by dropdown selection
          filter_category(input$outcome_filter) %>%
          mutate(compare_name = compare_countyname) %>% 
          mutate(compare_value = purrr::map2(data, compare_name, get_compare_value)) %>% 
          mutate(graphs = purrr::map2(data, compare_value, density_plot_overlay)) %>%
          pull(graphs)
      }
    }
    
    
  })
  
  observeEvent(
    {outcomes_dat()
    comp_county_dat()
    input$outcome_filter
    input$show_matches}, {
    req(density_graphs())
    
    purrr::iwalk(density_graphs(), ~{
      output_name <- paste0("density_graph", .y)
      output[[output_name]] <- renderPlotly(.x)
    })
  })
  
  
  
  output$density_graphs_ui <- renderUI({
    req(county_check())
    req(density_graphs())
    
    density_plots_list <- purrr::imap(density_graphs(), ~{
      tagList(
        plotlyOutput(
          outputId = paste0("density_graph", .y),
          height = '200px'
        ) %>% withSpinner(type = getOption("spinner.type", default = 1),
                          color = getOption("spinner.color", default = "#046B5C")),
        br()
      )
    })
    tagList(density_plots_list)
  })
  
  
}
