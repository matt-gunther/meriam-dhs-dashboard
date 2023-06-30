source("utils.r")

# Get a list of all countries included in IPUMS extract
dd <- here("data/ipums_dhs.rds") %>% 
  read_rds %>% 
  distinct(iso = tolower(iso), country_code)

# Download IPUMS DHS shapefiles 
dd$iso %>% 
  walk(~{
    file <- paste0(
      "https://www.idhsdata.org/idhs/resources/gis/dhs_ipumsi_", 
      .x,".zip"
    )
    file %>% download.file(path_temp(paste0(.x, ".zip")))
    unzip(
      path_temp(paste0(.x, ".zip")),
      exdir = path_temp(.x)
    )
    path_temp(.x) %>% 
      read_sf() %>% 
      filter(!st_is_empty(geometry), ADMIN_NAME != "Waterbodies") %>%
      write_rds(here("data/geo", paste0(.x, '.rds')), compress = "gz")
  }) 

