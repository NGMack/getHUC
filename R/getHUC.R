#' Obtain GIS and HUC inforamtion for new Stations
#'
#' @param sites A list of Station Codes, stream names and coordinates.
#' @param year Label the year these site are to sampled, just used for naming export file.
#'
#' @return a csv file of StationCodes and coordinates is produced with GIS information necessary for creating new sites in SWAMP databse
#'
#' @export
#'
#' @examples
#' sites <- load("~/R/huc/data/test_sites.rda")
#' getHUC(sites = test_sites, year = 2022)
#' required column names for sites are: StationCode, Name, lat, long
getHUC<- function(sites, year) {
  load("~/R/huc/data/calwater.rda")
  sites1 <- sf::st_transform(sf::st_as_sf(sites, coords = c('long', 'lat'), crs = 4326, remove = FALSE), 3310)
  sites2 <- sf::st_intersection(sites1, calwater)
  sites3 <- sf::st_drop_geometry(sites2)
  sites4 <- dplyr::mutate(sites3, HUC = stringr::str_sub(sites3$CALWNUM, start = 2, end = 4))
  sites5 <- dplyr::select(sites4, StationCode, Name, lat, long, HUC, CALWNUM, COUNTY, HUNAME, LEVEL3)
  sites_huc <- sites5
  rm(sites1,sites2,sites3,sites4,sites5)
  return(write.csv(sites_huc, paste('~/R/huc/data/sites_huc_', year, '.csv', sep = '')))
}
