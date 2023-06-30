# options(rsconnect.max.bundle.size = 50145728000)

rsconnect::deployApp(
  appFiles = fs::dir_ls(
    recurse = TRUE, 
    invert = TRUE, 
    regexp = "conda|pdfs|setup|local|gps"
  ), 
  appName = "MERIAM", 
  appPrimaryDoc = "app.Rmd"
)
