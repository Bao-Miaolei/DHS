getNationalSamples <- function(pSamp, loc, regMap){
  # Convert the national boundary map to sf format
  # regMap_sf <- st_as_sf(regMap)
  regMap_sf <- st_union(regMap)
  
  # Assign each point to a administrative region
  gridP = data.frame(Longitude = loc[, 1],
                     Latitude  = loc[, 2])
  gridP_sf = st_as_sf(gridP, coords = c("Longitude", "Latitude"), crs = st_crs(regMap))
  
  gridP_sf = st_transform(gridP_sf, st_crs(regMap))
  mask <- st_intersection(gridP_sf, regMap_sf)
  
  inside_indices <- which(st_contains(regMap_sf, gridP_sf, sparse = FALSE))
  filtered_pSamp <- pSamp[, inside_indices, drop = FALSE]
  
  return(list(filtered_pSamp = filtered_pSamp, inside_indices = inside_indices))
}