source("utils.r")
library(leaflet)
library(srvyr)
library(htmltools)

library(gtsummary)

input <- list(
  # samp = "Ethiopia 2016",
  samp = "Kenya 2003",
  exp = "CHIRTS",
  time = c(1, 3),
  space = 10
)

# Exposure colors and value-box icons
# Exposure colors and value-box icons
exp_dd <- list(
  CHIRPS = list(
    color = "#0098CE", 
    icon = "ion-waterdrop",
    txt = paste(
      "Total monthly precipitation (mm) averaged 1-3 months before the first",
      "interview"
    )
  ),
  CHIRTS = list(
    color = "#D82310", 
    icon = "ion-android-sunny",
    txt = paste(
      "Maxmimum monthly temperature (C°) averaged 1-3 months before the first",
      "interview"
    )
  ),
  NDVI = list(
    color = "#81A88D", 
    icon = "ion-leaf",
    txt = paste(
      "Maxmimum value (range -1 to 1) averaged 1-3 months before the first",
      "interview"
    )
  ),
  UCDP = list(
    color = "#772953", 
    icon = "ion-alert-circled",
    txt = paste(
      "Count of lethal events within the top administrative area",
      "(including events that may have crossed borders)", 
      "within the period 1-3 months before the first interview"
    )
  ),
  WHZ = list(
    color = "#99469D", 
    icon = "ion-woman",
    txt = paste(
      "Difference between the child's weight and the median weight of a", 
      "healthy child  of the same height and sex (defined by the WHO",
      "Global Database on Child Growth and Malnutrition), expressed in units",
      "equal to one standard deviation of the reference population's",
      "distribution (known as a Z-score)",
      "www.idhsdata.org/idhs-action/variables/HWWHZWHO/#description_section"
    )
  )
)

# Load supplementary georeferenced datasets `geo`
geo <- here("data/geo") %>% 
  get_shapes() %>%  
  imap(~{
    list(
      CHIRPS = path(here("data/chirps"), .y, ext = "tif") %>% rast,
      CHIRTS = path(here("data/chirts"), .y, ext = "tif") %>% rast, 
      NDVI = path(here("data/ndvi"), .y, ext = "tif") %>% rast, 
      UCDP = path(here("data/ucdp"), .y, ext = "rds") %>% read_rds,
      shapes = .x
    )
  })

# Load survey dataset `dat` 
# Only include samps that have GPS coords and full NDVI exposure (post-2000)
dat <- here("data/ipums_dhs.rds") %>% read_rds %>% 
  mutate(sample_lbl = paste(country, sample_yr), iso = iso %>% tolower) %>% 
  filter(has_gps, min_date > (geo[[1]]$NDVI %>% names())[1] %>% as_date())

# Get user-defined exposure type and period
samp <- dat %>% filter(sample_lbl == input$samp)
exposure <- geo[[samp$iso]][[input$exp]]
shapes <- geo[[samp$iso]]$shapes %>% st_as_sf()
start <- floor_date(samp$first_interview - months(max(input$time)), "month")
stop <- ceiling_date(samp$first_interview - months(min(input$time)), "month") 

# Get user-defined EA buffers
km <- input$space
eas <- list_c(samp$data) %>% 
  filter(!is.na(whz)) %>% 
  count(cluster, geometry, geo) %>% 
  st_as_sf() %>%
  st_transform(crs = samp$epsg) %>% 
  st_buffer(1000 * km) %>%
  st_transform(crs = 4326) 

# National-level Exposure for maps 
ntl_exposure <- if(input$exp == "UCDP"){
  exposure %>% filter(dates %>% int_overlaps(interval(start, stop))) 
} else {
  days <- seq(start, stop - days(1), by = "days") %>% as.character()
  months <- exposure[[which(names(exposure) %in% days)]]
  if(input$exp == "NDVI"){max(months)} else {mean(months)}
}

# Admin1-level summaries (mostly for UCDP maps)
admin1_exposure <- if(input$exp == "UCDP"){
  idx <- ntl_exposure %>% st_nearest_feature(shapes)
  ntl_exposure$ADMIN_NAME <- shapes$ADMIN_NAME[idx]
  ntl_exposure %>% 
    st_drop_geometry() %>% 
    count(ADMIN_NAME, name = "exposure") %>% 
    left_join(shapes, ., by = "ADMIN_NAME") %>% 
    transmute(geo = ADMIN_NAME, exposure = exposure %>% replace_na(0)) 
} else {
  shapes$exposure <- shapes$geometry %>% 
    map_dbl(~{
      vals <- ntl_exposure %>% crop(vect(.x)) %>% values()
      mean(vals, na.rm = TRUE)
    })
  shapes 
}

# Set `weights = TRUE` for a spatially weighted `fun`, but sacrifice speed
ea_exposure <- if(input$exp == "UCDP"){
  idx <- eas %>% st_nearest_feature(admin1_exposure)
  eas$exposure <- admin1_exposure$exposure[idx]
  eas
} else {
  eas$exposure <- ntl_exposure %>%
    zonal(vect(eas), fun = mean, weights = FALSE, na.rm = TRUE) %>%
    unlist()
  eas
}

# Update sample data with personal exposure 
personal_exposure <- ea_exposure %>% 
  select(-c(n, geo)) %>% 
  st_drop_geometry() %>% 
  right_join(
    list_c(samp$data),
    by = "cluster", 
    multiple = "all"
  ) 



personal_exposure <- personal_exposure %>% 
  mutate(exposure = if(input$exp == "CHIRPS"){
    exposure / 10
  } else if(input$exp == "CHIRTS"){
    exposure / 5
  } else if(input$exp == "NDVI"){
    exposure * 10
  } else if(input$exp == "UCDP"){
    exposure
  }) 



library(marginaleffects)

personal_exposure %>% 
  filter(!is.na(exposure), !is.na(whz)) %>% 
  as_survey_design(weight = weight, id = cluster) %>% 
  survey::svyglm(
    whz ~ exposure*mom_age + lz + 
      geo + hh_toilet + hh_electric + hh_drink + hh_natfloor +
      kid_male + mom_edu +
      kid_agemo + kid_diar + kid_fevr + birth_order,
    design = .
  ) %>% 
  predictions(
    type = "response", 
    variables = c("exposure"),
    newdata = datagrid(
      lz = "Agricultural",
      geo = "Rift Valley",
      hh_toilet = "Non-flush",
      hh_electric = FALSE,
      hh_drink = "Surface",
      hh_natfloor = TRUE,
      mom_age = 28,
      mom_edu = "Primary",
      kid_agemo = 28,
      kid_male =  FALSE,
      kid_diar = FALSE,
      kid_fevr = FALSE,
      birth_order = 3
    )
  ) %>% 
  tibble() %>% 
  ggplot(aes(x = exposure, y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = geo), alpha = 0.1) +
  theme_minimal()



model %>% 
  predictions(
    type = "response", 
    variables = "exposure",
    newdata = datagrid()
  ) %>% 
  ggplot(aes(x = exposure, y = estimate)) + 
  geom_line(color = exp_dd[[input$exp]]$color) +
  geom_ribbon(alpha = 0.2, aes(ymin = conf.low, ymax = conf.high)) +
  labs(y = "WHZ", x = paste("Exposure \n Jittered +/-")) + 
  scale_x_continuous(labels = ~.x * 5)
