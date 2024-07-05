# Temporary directory where files are uploaded
tempdirname <- dirname(input$shapefile$datapath[1])

# Rename files
for (i in 1:nrow(input$shapefile)) {
    file.rename(
        input$shapefile$datapath[i],
        paste0(tempdirname, "/", input$shapefile$name[i])
    )
}

# Read the shapefile using st_read from sf package
shape_input <- sf::st_read(paste(tempdirname,
                            input$shapefile$name[grep(pattern = "*.shp$", input$shapefile$name)],
                            sep = "/"))

# Transform the CRS if necessary
shape_input <- sf::st_transform(shape_input, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +no_defs +datum=WGS84")

# Convert the sf object to a Spatial object
shape_file = methods::as(shape_input, "Spatial")

spat_cov = assessSpatialCov(dat = cleaned_data,
                    periods = as.list(periods),
                    res = 1000,
                    logCount = input$log,
                    shp = shape_file(),
                    species = "species",
                    x = "longitude",
                    y = "latitude",
                    year = "year",
                    spatialUncertainty = NULL,
                    maxSpatUncertainty = NULL,
                    identifier = "identifier",
                    output = input$output)

do.call(ggpubr::ggarrange, spat_cov)

test = as.data.frame(sf::st_as_sf(cleaned_data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE))


























