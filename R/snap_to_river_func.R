#' Function to snap points to a dense polyline
#'
#' @param x a point shapefile that contains the points to be snapped
#' (must be of class sf)
#' @param y a polyline shapefile that contains the river network
#' (must be of class sf)
#' @param max_dist maximum distance for snapping points
#'
#' @return a data frame that mirrors x, but with the geometries snapped to y
#' (the result of st_distance(x, y) is a vector of 0s)
#' @export
#' 
#' @details this function needs a dense polyline shapefile
# (i.e. a shapefile that has small -short- lines).
# This shapefile can be built in ArcGIS using the functions to generate
# equally spaced points along a polyline and then to
# split line at points (points should be sampled with 100 - 500 m intervals )
#'
#' @examples
#' 
snap_to_river <- function(x, y, max_dist = 1000){
  
  ## NOTE: This function is needed because sf is not good at snapping
  
  # Simplify y to check distance
  y_simple <- y %>% st_union()
  # Make the river shapefile dense: extract points at each vertex
  y_points <- y %>%
    st_as_sf %>%
    st_cast("POINT") %>%
    mutate(id = 1:nrow(.))
  # Make the river shapefile dense: add splits at each vertex
  y_complex <- lwgeom::st_split(y, y_points) %>%
    st_collection_extract(.,"LINESTRING") %>%
    st_as_sf() %>%
    mutate(length = st_length(.))
  ## Check the distribution length of the simplified reaches
  #hist(y_complex$length, nclass = 300)
  # Initialize output list
  out <- list()
  # Loop over x rows
  for (i in 1:nrow(x)){
    # Select the point that will be snapped
    x_i.snap <- x[i,]
    # Extract the nearest feature
    nf <- y_complex[st_nearest_feature(x[i,], y_complex),]
    # Check the distance between the point to snap and the nearest feature
    # if yes, then snap, otherwise do not snap and keep the original point
    if ( as.numeric(st_distance(x[i,], nf)) < max_dist) {
      # # Snap the point to the nearest feature NOTE: st_snap is not working properly
      # x_i.snap <- st_snap(x[i,], nf %>% st_cast("POINT"), tolerance = max_dist)
      # Transform the nearest polyline to points
      nf.points <- st_cast(nf, "POINT")
      # Select the point that has the minimum distance to the point to snap
      nf.points.min <- nf.points[ which.min(st_distance(x[i,],nf.points)),]
      # Substitute the geometry of the point to snap with
      # the geometry of the closest point in the nearest feature (nf) object
      x_i.snap$geometry <- nf.points.min$geometry
      # # Check the distance
      # st_distance(x_i.snap, y_simple)
    }
    # Create output data frame
    out[[i]] <- x_i.snap
    #cat(paste0(i, " "))
  }
  # out_x should contain the same metadata of dams
  out_x <- do.call(rbind, out)
  return(out_x)
}