#' \code{assessSpatialBias}
#'
#' This function calculates how far the data deviates from a random distribution in geographic space. It calculates
#' a nearest neighbour index, defined as the mean of the nearest neighbour distances of the emprical data divided by
#' the mean of the neares neighbour distances of a random sample. The user can choose how many random samples to average
#' over, which should be larger where there are fewer data. This is because the random samples are generated in equal number to
#' the data. Where the number of samples is > 1, the estimates come with a measures of uncertainty (5th and 95th percentiles). 
#' The index is calculated for each of n user-specified time periods.
#' @param dat string. A data.frame containing columns for species name, x coordinates, y coordinates, spatialUncertainty, year and an identifier (used to group the data - heuristic will be calculated for each group). 
#' @param species Character string. column name in dat giving species names.
#' @param x string. Column name in dat giving x coordinates.
#' @param y string. Column name in dat giving y coordinates.
#' @param year string. Column name in dat giving years.
#' @param spatialUncertainty String. Column name in dat giving uncertainty associated with x and y. Any units are permitted. 
#' @param identifier String. Column name in dat giving record "identifiers". Identifiers are used to group the data; heuristics will be calculated separately for each group.
#' @param periods Numeric. A list of time periods. For example, for two periods, the first spanning 1950 to 1990, and the second 1991 to 2019: periods = list(1950:1990, 1991:2019).
#' @param nSamps Logical. How many iterations of random samples to use for comparison of empirical NN index with random NN index.
#' @param mask String. A raster object used to indicate the study region over which the random distribution should be generated. In most cases this will be a single raster layer. However, where the identifier 
#'             field is used to subset the data spatially (e.g. splitting it by country, continent, etc.) then mask should be a raster stack with n layers, where n is equal to the number of levels in the identifier field. 
#'             mask may have fewer layers than the number of levels in identifier field when identifier refers to spatial subsets, but any rows in dat with an identifier for which there is no layer 
#'             in mask will be dropped. If nlayers(mask) > 1, i.e. if the identifier field is used to subset the data spatially, then names(mask) should match the names in the identifier field.
#'        Must be NA where points are not to be generated, and numeric where they may be generated. For example, this could be a map of worldclim climate data, cropped to the study region.
#' @param degrade Logical. Whether or not to remove duplicated coordinates from the data. Coordinates are not considered to be duplicated if they are from
#'        different \code{periods}.
#' @param maxSpatUncertainty Numeric. Maximum permitted spatial uncertainty. All records more uncertain than this value will be dropped. Units must match the units in your data.
#' @param crs Either "BNG" or "WGS84", specifying the CRS of the x/y input.
#' @seealso \code{\link{assessSpatialCov}} which maps your data in geographical space.
#' @return A list with two elements: a ggplot2 object and the data underpinning the plot.
#' @export
assessSpatialBias_modified <- function(dat, 
                                       species,
                                       x,
                                       y,
                                       year,
                                       spatialUncertainty,
                                       identifier,
                                       periods, 
                                       mask, 
                                       nSamps = 50, 
                                       degrade = TRUE, 
                                       maxSpatUncertainty = NULL,
                                       crs = "BNG"){
  
  if (any(!(c(species, x, y, year, spatialUncertainty, identifier) %in% colnames(dat)))) {
    stop("You have specified columns that don't exist in dat.")
  }

  dat <- createData(data = dat,
                    species,
                    x,
                    y,
                    year,
                    spatialUncertainty,
                    identifier)

  if (!is.null(maxSpatUncertainty)) {
    dat <- dat[!is.na(dat$spatialUncertainty) & dat$spatialUncertainty <= maxSpatUncertainty, ]
    if (nrow(dat) == 0) stop("No records with spatialUncertainty < maxSpatUncertainty")
  }

  if (raster::nlayers(mask) > 1) {
    if (!any(unique(dat$identifier) %in% names(mask))) {
      stop("No names of layers in mask match levels in the identifier field.")
    }
    if (any(!(unique(dat$identifier) %in% names(mask)))) {
      warning("Dropping data with identifiers not in mask.")
      dat <- dat[dat$identifier %in% names(mask), ]
    }
  }

  # Create sf object and extract coordinates (CRS-aware)
  if (!(crs %in% c("BNG", "WGS84"))) stop("crs must be either 'BNG' or 'WGS84'")
  sf_crs <- if (crs == "BNG") 27700 else 4326

  dat$X_TEMP <- dat[["x"]]
  dat$Y_TEMP <- dat[["y"]]

  sf_dat <- sf::st_as_sf(dat, coords = c("X_TEMP", "Y_TEMP"), crs = sf_crs)
  coords <- sf::st_coordinates(sf_dat)

  dat$x <- coords[, 1]
  dat$y <- coords[, 2]

  dat <- dat[order(dat$year), ]

  dat <- dat[dat$year %in% unlist(periods), ]

  dat$Period <- NA
  for (i in seq_along(periods)) {
    dat$Period[dat$year %in% periods[[i]]] <- i
  }

  if (degrade && any(duplicated(dat[, c("x", "y", "identifier", "Period")]))) {
    dat <- dat[!duplicated(dat[, c("x", "y", "identifier", "Period")]), ]
  }

  result_list <- list()

  for (i in unique(dat$identifier)) {

    domain <- if (raster::nlayers(mask) > 1) mask[[i]] else mask

    index <- lapply(seq_along(periods), function(y) {
      pDat <- dat[dat$Period == y & dat$identifier == i, ]

      if (nrow(pDat) > 2) {
        empDist <- spatstat.geom::nndist(X = pDat$x, Y = pDat$y, k = 1)
        empMean <- mean(empDist)

      randomSamp <- lapply(1:nSamps, function(i) {
        ran <- raster::sampleRandom(domain,
                                    size = min(nrow(pDat), raster::ncell(domain)),
                                    xy = TRUE)
        dist <- spatstat.geom::nndist(X = ran[, 1], Y = ran[, 2], k = 1)
        mean(dist)
      })

        indDist <- empMean / unlist(randomSamp)
        out <- data.frame(mean = mean(indDist),
                          upper = quantile(indDist, 0.95),
                          lower = quantile(indDist, 0.05),
                          Period = as.character(y),
                          identifier = i)
      } else {
        warning(paste("Too few records in period", y, "for", i))
        out <- data.frame(mean = NA, upper = NA, lower = NA,
                          Period = as.character(y), identifier = i)
      }

      return(out)
    })

    result_list[[i]] <- do.call("rbind", index)
  }

  data <- do.call("rbind", result_list)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = as.numeric(Period), y = mean,
                                          ymin = lower, ymax = upper,
                                          colour = identifier, fill = identifier)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_ribbon(alpha = 0.5) +
    ggplot2::theme_linedraw() +
    ggplot2::ylab("Nearest neighbour index") +
    ggplot2::xlab("Period") +
    ggplot2::labs(fill = "", colour = "") +
    ggplot2::guides(colour = FALSE)

  return(list(data = data, plot = p))
}
