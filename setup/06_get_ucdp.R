source("utils.r")

# List shapefiles for each country 
shapes <- here("data/geo") %>% get_shapes %>% map(st_as_sf)

# Attach UCDP country labels to `dat`
dat <- here("data/ipums_dhs.rds") %>% 
  read_rds %>% 
  distinct(country, iso = tolower(iso)) %>% 
  mutate(shape = shapes[iso])

# Download the UCDP file to a temporary folder 
"https://ucdp.uu.se/downloads/ged/ged191-geojson.zip" %>% 
  download.file(path_temp("ged191-geojson.zip"))

# Unzip the file 
unzip(
  path_temp("ged191-geojson.zip"),
  exdir = path_temp()
) 

# Load the file 
ucdp <- path_temp("ged191.geojson") %>% read_sf()

# Recode key variables 
ucdp <- ucdp %>% 
  transmute(
    type_of_violence = type_of_violence %>% case_match(
      1 ~ "State-based conflict",
      2 ~ "Non-state conflict",
      3 ~ "One-sided violence"
    ),
    where_prec = where_prec %>% case_match(
      1 ~ "Exact",
      2 ~ "Within 25km",
      3 ~ "Within this point's admin2 border",
      4 ~ "Within this point's admin1 border",
      5:7 ~ "Location is uncertain"
    ),
    dates = interval(date_start, date_end),
    date_prec = date_prec %>% case_match(
      1:4 ~ "Month is certain",
      5 ~ "Month is uncertain"
    ), 
    side_a,
    side_b,
    deaths_a, 
    deaths_b,
    deaths_civilians,
    deaths_other = deaths_unknown,
    country,
    source_article,
    admin2 = adm_2
  ) 

dat$shape %>% 
  map2(dat$country, ~{
    out <- ucdp %>% st_crop(.x) %>% filter(country == .y)
    out$index <- out %>% st_nearest_feature(.x)
    out$admin <- .x$ADMIN_NAME[out$index]
    out
  }) %>% 
  iwalk(~.x %>% write_rds(here("data/ucdp", paste0(.y, ".rds")), "gz"))
