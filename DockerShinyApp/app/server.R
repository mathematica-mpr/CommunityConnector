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
    df <- rbind(rep(1, 6), rep(0, 6),
                # 50% circle color
                rep(.5, 6),
                # 100 % circle color
                rep(1, 6),
                county_dat() %>% select(starts_with("sdoh_score"))) %>%
      rename(`Economic Stability`                      = sdoh_score_1,
             `Neighborhood\nand Physical\nEnvironment` = sdoh_score_2,
             `Education`                               = sdoh_score_3,
             `Food`                                    = sdoh_score_4,
             `Community\nand Social\nContext`          = sdoh_score_5,
             `Health Care\nSystem`                     = sdoh_score_6)
    
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

  
}