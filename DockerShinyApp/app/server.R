server <- function(input, output) {
  
#  county_selection_check <- reactive({
#    validate({
#      if (input$county_selection_type == "FIPS") {
#        need(nchar(input$county_selection) == 4, 'Please enter a four digit FIPS Code for your county.')
#      } else if (input$county_selection_type == "name") {
#        need(input$county_selection %in% dat$County, 'Please enter a valid county name.')
#      }
#    })
#  })
#  
#  output$county_selection_message <- renderText({
#    county_selection_check
#  })
  
  options(DT.options = list(dom = "t", ordering = F))

  county_check <- reactive({
    if (input$county_selection_type == "FIPS") {
      input$county_selection %in% dat$FIPS
    } else if (input$county_selection_type == "name") {
      input$county_selection %in% dat$County
    }
  })
  
  county_FIPS <- reactive({
    req(county_check)
    if (input$county_selection_type == "FIPS") {
      input$county_selection
    } else if (input$county_selection_type == "name") {
      dat %>% filter(County == input$county_selection) %>% pull(FIPS)
    }
  })
  
  county_name <- reactive({
    req(county_check)
    if (input$county_selection_type == "name") {
      input$county_selection
    } else if (input$county_selection_type == "FIPS") {
      dat %>% filter(FIPS == input$county_selection) %>% pull(County)
    }
  })
  
  county_dat <- reactive({
    dat %>% filter(FIPS == county_FIPS())
  })

  output$my_county_name <- renderUI({
    req(county_check())
    HTML(paste0("<h3>My Selection<br/></h3>", "<h4>", county_name(), ", ", county_dat()$State, "</h4>"))
  })
  
  
  output$my_county_radar <- renderPlot({
    req(county_check())
    dd <- dd %>% 
      filter(grepl("sdoh_score", column_name))
    df <- rbind(rep(1, 6), rep(0, 6),
                # 50% circle color
                rep(.5, 6),
                # 100 % circle color
                rep(1, 6),
                county_dat() %>% select(starts_with("sdoh_score"))) %>%
      rename_at(vars(dd$column_name), ~ dd$descrip_new)
    
    par(bg = config$colors$tan25)
    radarchart(df, 
               pcol = c(NA, NA,
                        paste0(config$colors$red100, '80')), 
               plty = 0,
               pfcol = c(paste0(config$colors$grey50, '80'),
                         paste0(config$colors$grey25, '33'),
                         paste0(config$colors$yellow100, '33')),
               cglcol = config$colors$grey100,
               seg = 4, vlcex = 0.8)
  })
  
  output$my_county_demo <- DT::renderDT({
    req(county_check())
    dd <- dd %>%
      filter(grepl("demographics_", column_name))
    df <- county_dat() %>% select(starts_with("demographics_")) %>%
      rename_at(vars(dd$column_name), ~ dd$descrip_new) %>%
      t() %>% data.frame() %>%
      rownames_to_column()
    DT::datatable(df, rownames = FALSE, colnames = c("Essential facts", ""), class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt",
                      background = config$colors$tan25)
  })
  
  output$health_outcomes_density <- renderPlot({
    req(county_check())
    
    outcomes <- grep("outcome_", names(dat), value = T)
    
    dd <- dd %>% 
      filter(grepl("outcome_", column_name)) %>% 
      select(column_name, description)
    
    # selected county and matches to selected county dataframe
    my_matches <- find_my_matches(county_FIPS(), dat) 
    
    # need to add coloring to the vlines
    df <- dat %>% select(FIPS, State, County, outcomes) %>%
      pivot_longer(cols = outcomes) %>%
      # selected county and matches to selected county
      mutate(type = case_when(
        FIPS %in% my_matches ~ "matches",
        FIPS == county_FIPS() ~ "selected",
        TRUE ~ "other"
      )) %>%
      # left join data dictionary to get real outcome names
      rename(column_name = name) %>%
      left_join(dd, by = "column_name") 
    
    ggplot(df, aes(x=value)) + geom_density() +
      facet_wrap(~description, scales = "free", ncol = 1) +
      geom_vline(data = filter(df, type != "other"), aes(xintercept=value, color = as.factor(type))) 
    
  })
  
 # output$test <- renderD3({
 #   r2d3(
 #     c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20),
 #     script = "./d3/radar_chart.js"
 #   )
 # })

  
}
