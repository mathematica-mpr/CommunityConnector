# TODO
# - add errors for incorrect background or logo options
# - add option for grid lines (major, minor or both)
# - get svg files and figure out adding logo
# _ get feedback
# - update p2 example
# - add minor grid lines
# - add grid line options?

#' A Mathematica branded theme
#'
#' This is a complete theme that controls all non-data display.
#'
#' @param background color for the plot and surrounding area, either white or tan
#' @param accessible logical of whether or not the plot is meant to be 508 compliant;
#' default is FALSE. If TRUE, text will be black. Note that you still must make
#' sure the color palette used in plotting meets 508 compliance,
#' e.g. by using the `white 508` or `tan 508` Mathematica palettes
#' @param major_grid_x logical to include major x-axis gridlines; default is TRUE
#' @param major_grid_y logical to include major y-axis gridlines; default is TRUE
#' @param minor_grid_x logical to include minor x-axis gridlines; default is FALSE
#' @param minor_grid_y logical to include minor y-axis gridlines; default is FALSE
#' @param base_size base font size

#' @export
#' @examples
#'  mtcars2 <- within(mtcars, {
#'    vs <- factor(vs, labels = c("V-shaped", "Straight"))
#'    am <- factor(am, labels = c("Automatic", "Manual"))
#'    cyl  <- factor(cyl)
#'    gear <- factor(gear)
#'  })
#'
#'  p1 <- ggplot(mtcars2) +
#'    geom_point(aes(x = wt, y = mpg, colour = gear)) +
#'    labs(title = "Fuel economy declines as weight increases",
#'         subtitle = "(1973-74)",
#'         caption = "Source: 1974 Motor Trend US magazine",
#'         x = "Weight (1000 lbs)",
#'         y = "Fuel economy (mpg)",
#'         colour = "Gears")
#'
#'  p1 + theme_mpr()
#'
#'  # with Mathematica colors:
#'  p1 +
#'   theme_mpr() +
#'   scale_color_mpr(type = "qual")
#'
#'   p2 <- ggplot(mtcars) +
#'     geom_histogram(aes(x = wt),
#'                  fill = mpr_cols("green"),
#'                  bins = 15)
#'   p2 +
#'    theme_mpr()


theme_mpr <- function(background = "tan",
                              accessible = FALSE,
                              major_grid_x = TRUE,
                              major_grid_y = TRUE,
                              minor_grid_x = FALSE,
                              minor_grid_y = FALSE,
                              base_size = 14
                              ) {
  # check background color
  if(!(background %in% c("white", "tan"))) {
    stop(paste(background,"is not a valid background color. Please use `white` or `tan`. \n"))
  }

  # get Mathematica graphics font if not already loaded
#  if (! "Zilla Slab Medium" %in% fonts()) {
#    suppressMessages(font_import(pattern = 'ZillaSlab', prompt = FALSE))
#  }

  # define fixed parameters/colors
  base_line_size <- base_size / 22
  base_rect_size <- base_size / 22
  grey <- "#5B6771"
  grey75 <- "#7b818a"
  grey50 <- "#9fa2a9"
  green <- "#046B5C"
  tan <- "#f5f1e8"
  white <-  "#ffffff"

  # override greys to black if needs to be 508 compliant
  if(accessible){
    grey <- "#000000"
    grey75 <- "#000000"
    grey50 <- "#000000"
  }


  # check  options
  major_grid_x  <- if (major_grid_x) {
    element_line(color = grey50, size = base_line_size, linetype = "dashed")
  } else {
    element_blank()
  }

  major_grid_y  <- if (major_grid_y) {
    element_line(color = grey50, size = base_line_size, linetype = "dashed")
  } else {
    element_blank()
  }

  minor_grid_x  <- if (minor_grid_x) {
    element_line(color = grey50, size = base_line_size, linetype = "dashed")
  } else {
    element_blank()
  }

  minor_grid_y  <- if (minor_grid_y) {
    element_line(color = grey50, size = base_line_size, linetype = "dashed")
  } else {
    element_blank()
  }

  # Start with theme_bw and then replace things
  theme_bw(
    base_size = base_size,
    base_family = "Zilla Slab Medium",
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.line           = element_line(color = grey75),
      axis.title.x        = element_text(color = grey,
                                         margin = margin(t = 15, b = 0, r = 0, l = 0)),
      axis.title.y        = element_text(color = grey,
                                         margin = margin(t = 0, b = 0, r = 15, l = 0),
                                         angle = 90),
      axis.text           = element_text(color = grey),
      axis.ticks          = element_blank(),
      legend.background   = element_blank(),
      legend.key          = element_blank(),
      legend.text         = element_text(color =  grey),
      legend.title        = element_text(color =  grey),
      panel.background    = element_blank(),
      panel.border        = element_blank(),
      panel.grid.major.x  = major_grid_x,
      panel.grid.major.y  = major_grid_y,
      panel.grid.minor.x  = minor_grid_x,
      panel.grid.minor.y  = minor_grid_y,
      strip.background    = element_blank(),
      plot.background     = element_rect(fill = ifelse(background == "tan", tan, white),
                                         color = ifelse(background == "tan", tan, white),
                                         linetype = NULL),
      plot.margin         = unit(c(1,1,1,1), "lines"),
      plot.tag            = element_text(color = grey),
      plot.title          = element_text(size = base_size*1.2,
                                         face = "bold",
                                         color = grey,
                                         hjust = -0.2,
                                         margin = margin(t = 0, r = 0, b = 10, l = 0)),
      plot.subtitle       = element_text(face = "italic",
                                         size = 0.9*base_size,
                                         hjust = 0,
                                         color = grey,
                                         margin = margin(t = 0, r = 0, b = 10, l = 0)),
      plot.caption        = element_text(face = "italic",
                                       hjust = 1,
                                       color = grey,
                                       size = 0.9*base_size,
                                       margin = margin(t = 10, b = 0, r = 0, l = 0)),
      complete = TRUE

    )

}

#' A function to grab the Mathematica logo
#'
#' @param direction should the logo be horizontal or stacked?
#' @param color a logical value indicating if the logo should be in
#'  color (TRUE) or black (FALSE)

get_logo <- function(direction = "horizontal", color = TRUE) {

  # check direction
  if (! direction %in% c("horizontal", "stacked")) {
    stop(paste(direction,"is not a valid option for direction. Please use `horizontal` or `stacked`. \n"))
  }

  # check color
  if (! color %in% c(TRUE, FALSE)) {
    stop(paste(color,"is not a valid option for color. Please use TRUE or FALSE. \n"))
  }

  file_name <- case_when(
    direction == "horizontal" & color == TRUE    ~  "horz_cmyk.SVG",
    direction == "horizontal" & color == FALSE   ~  "horz_k.SVG",
    direction == "stacked" & color == TRUE       ~  "stacked_cmyk.SVG",
    direction == "stacked" & color == FALSE      ~  "stacked_k.SVG"
  )

  logo <- image_read(system.file("logos", file_name, package = "plotMPR"))
  logo <- image_transparent(logo, "white")
  logo <- image_trim(logo)
  return(logo)
}

#' Adds the Mathematica logo to the last plot

add_logo <- function(direction = "horizontal", color = TRUE){
  logo <- get_logo(direction, color)
  grid::grid.raster(logo,
                    x = unit(0.03, "npc"),
                    y = unit(0.03, "npc"),
                    just = c("left", "bottom"),
                    width  = 0.2)
}

#' Makes geom defaults accessible

geom_accessible <- function() {
  update_geom_defaults(geom = "point",
                       list(color = "black",
                            shape = 21,
                            stroke = 1.06,
                            size = 3))

  update_geom_defaults(geom = "bar",
                       list(color = "black",
                            size = 1))
}

save_defaults <- function() {
  # get current AES and save
  bar_defaults <- ggplot2:::check_subclass("bar", "Geom")$default_aes
  point_defaults <- ggplot2:::check_subclass("point", "Geom")$default_aes
  line_defaults <- ggplot2:::check_subclass("line", "Geom")$default_aes

  return(list(bar_defaults, point_defaults, line_defaults))
}

geom_restore <- function(defaults) {
  update_geom_defaults("bar", defaults[[1]])
  update_geom_defaults("point", defaults[[2]])
  update_geom_defaults("line", defaults[[3]])
}
