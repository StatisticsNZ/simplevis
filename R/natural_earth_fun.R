# Natural earth functions

#' ne_bounds
#' 
#' @description Extract a vector of bounds from Natural Earth
#'
#' @param country A country name from Natural Earth.
#' @param state A state (or region) name within a country from Natural Earth or TRUE to specify all state boundaries. Must be used in combination with the applicable country specified.
#' @param continent A continent name from Natural Earth.
#' @param scale Scale of map to extract the bounds from with "small", "medium" or "large" options. Defaults to "small".
#' @param crs Coordinate reference system of bounds. Defaults to 4326.
#'
#' @return A vector of the bounding box.
#' @export
#'
#' @examples 
#' ne_bounds("Papua New Guinea")
#' ne_bounds("Australia", state = TRUE)
#' ne_bounds("Australia", state = "Tasmania")
ne_bounds <- function(country = NULL, state = NULL, continent = NULL, scale = "small", crs = 4326) {
  
  if(is.null(country) & is.null(continent) & is.null(state)) {
    stop("Please provide a country, state or continent.")
  }
  else({
    if(!is.null(continent)) {
      sf <- rnaturalearth::ne_countries(continent = continent, scale = scale, returnclass = "sf")
    } 
    else if(!is.null(country)) {
      if(is.null(state)) sf <- rnaturalearth::ne_countries(country = country, scale = scale, returnclass = "sf")
      else if(!is.null(state)) sf <- rnaturalearth::ne_states(country = country, returnclass = "sf") %>% filter(.data$name == state)
    }
  })
  
  if(crs != 4326) sf <- sf::st_transform(sf, crs)
  
  bounds <- as.vector(sf::st_bbox(sf))
  
  return(bounds)
}

#' ne_boundary 
#'
#' @description Extract a boundary from Natural Earth.
#' 
#' @param country A country name from Natural Earth.
#' @param state A state or region name within a country from Natural Earth. Must be used in combination with the applicable country specified.
#' @param continent A continent name from Natural Earth.
#' @param scale Scale of map to extract the bounds from with "small", "medium" or "large" options. Defaults to "large". Not applicable to where a state is extracted.
#' @param crs Coordinate reference system of bounds. Defaults to 4326.
#'
#' @return An sf object.
#' @export
#'
#' @examples
#' ggplot_sf(ne_boundary("Papua New Guinea"))
#' ggplot_sf(ne_boundary("Australia", state = TRUE))
#' ggplot_sf(ne_boundary("Australia", state = "Tasmania"))
ne_boundary <- function(country = NULL, state = NULL, continent = NULL, scale = "large", crs = 4326) {
  
  if(is.null(country) & is.null(continent) & is.null(state)) {
    stop("Please provide a country, state or continent.")
  }
  else({
    if(!is.null(continent)) {
      sf <- rnaturalearth::ne_countries(continent = continent, scale = scale, returnclass = "sf")
    } 
    else if(!is.null(country)) {
      if(is.null(state)) sf <- rnaturalearth::ne_countries(country = country, scale = scale, returnclass = "sf")
      else if(!is.null(state)) {
        if(state == TRUE) {
          sf <- rnaturalearth::ne_states(country = country, returnclass = "sf")  
        }
        else sf <- rnaturalearth::ne_states(country = country, returnclass = "sf") %>% filter(.data$name %in% state)
      }
    }
  })
  
  if(crs != 4326) sf <- sf::st_transform(sf, crs) 
  
  return(sf)
}
