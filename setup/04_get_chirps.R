source("utils.r")

# Specify host URL and date range to be extracted
host <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_daily/tifs/p05"
dat <- here("data/ipums_dhs.rds") %>% read_rds
daterange <- seq(min(dat$min_date), max(dat$max_date), by = "days")

# List shapefiles for each country 
shapes <- here("data/geo") %>% get_shapes

# List one bbox per country 
bboxes <- shapes %>% get_bboxes

# List of files to be scraped
files <- year(daterange) %>%
  set_names(daterange) %>% 
  imap_chr(
    ~.x %>% 
      path(paste0(
        "chirps-v2.0.", 
        .y %>% str_replace_all("-", "."), 
        ".tif.gz"
      ))
  )

# Scrape and write `raw` (daily) output
files %>% 
  map(.progress = TRUE, ~{
    global <- .x %>% file.path("/vsigzip//vsicurl", host, .) %>% rast()
    bboxes %>% map(~{global %>% crop(.x)})
  }) %>% 
  list_transpose() %>% 
  iwalk(~{
    border <- shapes[[.y]]
    .x %>% 
      rast() %>% 
      classify(cbind(-9999, NA)) %>%
      mask(border) %>%
      writeRaster(
        here("data/chirps", paste0(.y, ".tif")), 
        overwrite = TRUE
      )
  })


# Read `raw` and write monthly 
bboxes %>% 
  iwalk(.progress = TRUE, ~{
    raw <- here("data/chirps/raw", paste0(.y, ".tif")) %>% rast()
    out <- tibble(days = names(raw)) %>% 
      mutate(
        month = month(days %>% as_date),
        year = year(days %>% as_date)
      ) %>% 
      group_by(year, month) %>%
      summarise(dat = raw[[days]] %>% sum() %>% list())
    names <- make_date(out$year, out$month)
    out <- out$dat %>% rast()
    names(out) <- names
    out %>% writeRaster(here("data/chirps", paste0(.y, ".tif")))
  })