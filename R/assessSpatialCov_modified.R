#' \code{assessSpatialCov_modified}
#'
#' Grids and maps species occurrence data using specified CRS.
#'
#' @param dat A data.frame containing species occurrence data.
#' @param species Column name in dat giving species names.
#' @param x Column name in dat giving x coordinates.
#' @param y Column name in dat giving y coordinates.
#' @param year Column name in dat giving years.
#' @param spatialUncertainty Column name in dat giving spatial uncertainty (optional).
#' @param identifier Column name for grouping data.
#' @param periods List of year sequences, e.g., list(1950:1990, 1991:2019).
#' @param res Spatial resolution for gridding.
#' @param logCount Whether to log-transform the raster values.
#' @param countries ggplot2::map_data() country names or a list of them.
#' @param shp Shapefile object (sp class), or list of them.
#' @param maxSpatUncertainty Optional numeric cutoff for filtering uncertainty.
#' @param output "density", "overlap", or "nPeriods".
#' @param minPeriods Minimum number of periods required per cell (for "overlap").
#' @param returnRaster Whether to return underlying rasters.
#' @param crs Either "BNG" or "WGS84", specifying the CRS of the x/y input.
#'
#' @return A list of ggplot2 maps and optionally rasters.
#' @export
assessSpatialCov_modified <- function(dat,
                                      species,
                                      x,
                                      y,
                                      year,
                                      spatialUncertainty,
                                      identifier,
                                      res,
                                      logCount = FALSE,
                                      countries = NULL,
                                      shp = NULL,
                                      periods,
                                      maxSpatUncertainty = NULL,
                                      output = "density",
                                      minPeriods = NULL,
                                      returnRaster = FALSE,
                                      crs = "BNG") {
  
  if (any(!(c(species, x, y, year, identifier) %in% colnames(dat)))) {
    stop("You have specified columns that don't exist in dat.")
  }
  
  if (!is.null(spatialUncertainty) && !(spatialUncertainty %in% colnames(dat))) {
    stop("spatialUncertainty column specified but not found in data.")
  }

  # Optional filtering by spatial uncertainty
  if (!is.null(spatialUncertainty) && !is.null(maxSpatUncertainty)) {
    dat <- dat[!is.na(dat[[spatialUncertainty]]) & dat[[spatialUncertainty]] <= maxSpatUncertainty, ]
    if (nrow(dat) == 0) stop("No records remain after applying spatial uncertainty filter.")
  }

  # Remove NA years
  dat <- dat[!is.na(dat[[year]]), ]

  # Filter for relevant time periods
  all_years <- unlist(periods)
  dat <- dat[dat[[year]] %in% all_years, ]
  dat$Period <- NA
  for (i in seq_along(periods)) {
    dat$Period[dat[[year]] %in% periods[[i]]] <- paste0("p", i)
  }

  # Validate CRS input
  if (!(crs %in% c("BNG", "WGS84"))) stop("crs must be either 'BNG' or 'WGS84'")

  sf_crs <- if (crs == "BNG") 27700 else 4326
  dat$X_TEMP <- dat[[x]]
  dat$Y_TEMP <- dat[[y]]
  
  sf_dat <- sf::st_as_sf(dat, coords = c("X_TEMP", "Y_TEMP"), crs = sf_crs)

  coords <- sf::st_coordinates(sf_dat)
  dat$x <- coords[, 1]
  dat$y <- coords[, 2]

  xmin <- min(dat$x, na.rm = TRUE)
  xmax <- max(dat$x, na.rm = TRUE)
  ymin <- min(dat$y, na.rm = TRUE)
  ymax <- max(dat$y, na.rm = TRUE)

  rast <- raster::raster(ncol = length(seq(xmin, xmax, res)),
                         nrow = length(seq(ymin, ymax, res)),
                         xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax)

  raster_list <- list()

  for (id in unique(dat[[identifier]])) {
    subset_dat <- dat[dat[[identifier]] == id, ]
    rasts <- lapply(unique(subset_dat$Period), function(period) {
      period_dat <- subset_dat[subset_dat$Period == period, ]
      if (nrow(period_dat) > 0) {
        raster::rasterize(period_dat[, c("x", "y")], rast)
      } else {
        raster::setValues(rast, NA)
      }
    })

    names(rasts) <- unique(subset_dat$Period)
    rasts <- raster::stack(rasts)

    if (logCount) rasts <- log10(rasts)

    if (output == "density") {
      raster_list[[id]] <- rasts
    } else if (output == "nPeriods") {
      summed <- sum(as.logical(rasts), na.rm = TRUE)
      summed[summed == 0] <- NA
      raster_list[[id]] <- summed
    } else if (output == "overlap") {
      if (is.null(minPeriods)) minPeriods <- length(unique(subset_dat$Period))
      summed <- sum(as.logical(rasts), na.rm = TRUE)
      summed[summed < minPeriods] <- NA
      raster_list[[id]] <- as.logical(summed)
    }
  }

  myCol <- rgb(255, 255, 255, max = 255, alpha = 125)
  legend_title <- switch(output,
                         "density" = ifelse(logCount, "log10(n records)", "n records"),
                         "nPeriods" = "Number of periods sampled",
                         "overlap" = "Sampled in minPeriods periods")

  plots <- lapply(names(raster_list), function(id) {
    rast <- raster_list[[id]]

    if (!is.null(countries) || !is.null(shp)) {
      if (!is.null(shp)) {
        map <- if (is.list(shp)) ggplot2::fortify(shp[[id]]) else ggplot2::fortify(shp)
      } else {
        map_data <- ggplot2::map_data("world", regions = if (is.list(countries)) countries[[id]] else countries)
        map <- map_data
      }
    } else {
      map <- NULL
    }

    p <- rasterVis::gplot(rast) + ggplot2::theme_linedraw()

if (output == "density") {
  p <- p + ggplot2::geom_tile(ggplot2::aes(fill = value))

  # Only facet if there is more than one unique period (i.e., raster layer)
  if (raster::nlayers(rast) > 1) {
    p <- p + ggplot2::facet_wrap(~ variable)
  }

  p <- p + ggplot2::scale_fill_gradient2(
    low = "red", high = "blue", na.value = myCol,
    name = legend_title
  )
} else {
      n <- length(unique(raster::getValues(rast)))
      p <- p +
        ggplot2::geom_tile(ggplot2::aes(fill = factor(value))) +
        ggplot2::scale_fill_manual(values = terrain.colors(n, rev = TRUE),
                                   na.value = myCol,
                                   name = legend_title,
                                   na.translate = FALSE) +
        ggplot2::ggtitle(id)
      if (output == "overlap") {
        p <- p + ggplot2::theme(legend.position = "none")
      }
    }

    if (!is.null(map)) {
      p <- p + ggplot2::geom_polygon(data = map,
                                     ggplot2::aes(x = long, y = lat, group = group),
                                     colour = "black", fill = myCol, inherit.aes = FALSE)
    }

    p
  })

  names(plots) <- names(raster_list)

  if (returnRaster) {
    return(list(plots = plots, rasters = raster_list))
  } else {
    return(plots)
  }
}
