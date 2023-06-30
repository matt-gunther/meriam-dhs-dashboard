source("utils.r")

dat <- here("data/ipums_dhs.rds") %>% read_rds
daterange <- seq(min(dat$min_date), max(dat$max_date), by = "days")

# List shapefiles for each country 
shapes <- here("data/geo") %>% get_shapes

# List one bbox per country 
bboxes <- shapes %>% get_bboxes

# JSON request 
bboxes %>%
  iwalk(~{
    MODIStsp::MODIStsp(
      gui = FALSE,
      selprod = "Vegetation_Indexes_16Days_005dg (M*D13C1)", 
      prod_version = "006", # Version "061" is experimental
      sensor = "Terra",
      bandsel = "NDVI",
      user = Sys.getenv("NASA_USER"), # Must be defined in .Renviron
      password = Sys.getenv("NASA_PWD"), # Must be defined in .Renviron
      start_date = min(daterange) %>% str_replace_all("-", "."),
      end_date = max(daterange) %>% str_replace_all("-", "."),
      spatmeth = "bbox",
      bbox = .x,
      out_format = "GTiff",
      output_proj = "4326",
      out_folder = here("data/ndvi", .y),
      compress = "LZW",
      delete_hdf = TRUE
    )
  })

# Build raster stack 
bboxes %>% 
  iwalk(~{
    # Build
    dat <- here("data/ndvi", .y, "VI_16Days_005dg_v6/NDVI") %>% 
      list.files(full.names = TRUE) %>% 
      map(~rast(.x)) %>% 
      rast() %>% 
      mask(shapes[[.y]]) 
    # Fix dates 
    date <- names(dat) %>% str_remove("MOD13C1_NDVI_")
    day <- date %>% str_remove(".*_")
    year <- date %>% str_remove("_.*")
    names(dat) <- make_date(year) + days(day)
    # Adjust to scale factor 
    dat <- dat / 10000 
    # Write
    dat %>% writeRaster(here("data/ndvi", paste0(.y, ".tif")))
  })

# Remove folders created by MODIStsp
here("data/ndvi") %>% 
  dir_ls(type = "directory") %>% 
  walk(dir_delete)
       

# bboxes %>% 
#   imap(~{
#     dat <- path(here("data/ndvi"), .y, ext = "tif") %>% rast
#     dat <- dat / 10000 
#     dat %>% writeRaster(here("data/ndvi", paste0(.y, ".tif")), overwrite = TRUE)
#   })
