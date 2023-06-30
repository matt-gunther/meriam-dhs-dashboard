suppressMessages({
  suppressWarnings({
    library(tidyverse)
    library(here)
    library(sf)
    library(terra)
    library(fs)
  })
})

# Functions 

#' @description List shapefiles (one per country)
#' @param dir Directory where shapefiles were downloaded
#' @return List
#' @export get_shapes
get_shapes <- function(dir){
  dir %>% 
    dir_ls() %>% 
    set_names(~ .x %>% path_file %>% path_ext_remove) %>% 
    map(~.x %>% read_rds %>% vect)
}

#' @description List bboxes for a list of shapefiles (one per country)
#' @param shape_list List of shapefiles 
#' @return List
#' @export get_bboxes
get_bboxes <- function(shape_list){
  shape_list %>% 
    map(~.x %>% ext() %>% floor() %>% st_bbox(crs = st_crs(.x)))
}

#' @description Return NA on function error (for use with `mutate`)
#' @param fn A function that, on error, should return NA rather than abort
#' @export force_make
force_make <- function(fn){try_fetch(fn, error = function(cnd){NA})}

#' @description Make GT table 
#' @param dat 
#' @export make_gt
make_gt <- function(dat){
  dat %>% 
    gt::opt_interactive(
      use_search = TRUE, 
      use_highlight = TRUE,
      use_compact_mode = TRUE, 
      use_page_size_select = TRUE,
      page_size_default = 10,
      page_size_values =  c(5, 10, 15, 20)
    ) %>% 
    gt::render_gt()
}

make_vb_icon <- function(input, label){
  actionLink(
    input, 
    label, 
    icon = icon("up-right-from-square"),
    style = "color: rgba(0, 0, 0, 0.7);"
  )
}
