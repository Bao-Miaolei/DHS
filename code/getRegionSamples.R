getRegionSamples <- function(pSamp, loc, regMap, urbRaster){
  if ("NAME_2" %in% names(regMap)){
    regMap$admin2.name.full <- paste0(regMap$NAME_1, "_", regMap$NAME_2)
  } 
  regMap$regionID <- 1:nrow(regMap)
  
  # Assign each point to a administrative region
  gridP = data.frame(Longitude = loc[, 1],
                     Latitude  = loc[, 2])
  gridP_sf = st_as_sf(gridP, coords = c("Longitude", "Latitude"), crs = st_crs(regMap))
  
  gridP_sf = st_transform(gridP_sf, st_crs(regMap))
  
  mask = st_join(gridP_sf, regMap)
  
  mask = st_drop_geometry(mask["regionID"])
  
  # Get indicies of each region
  regIdx = list()
  for(i in 1:nrow(regMap)){
    regIdx = c(regIdx, list(idx = which(mask == i)))
  }
  
  # Initialize storage for each region
  regVal = list()
  for(i in 1:nrow(regMap)){
    regVal =  c(regVal, list(idx = vector('numeric', length = 0)))
  }
  
  # Interpolate to desired grid
  gridPop = getPopulation(loc = loc, regMap = regMap, urbRaster = urbRaster)
  
  # Generate aggregation for each sample
  nSamp = dim(pSamp)[1]
  for(iSamp in 1:nSamp){
    
    # Use simple integration scheme for each region
    for(i in 1:nrow(regMap)){
      tmpVal = pSamp[iSamp, regIdx[[i]]]
      tmpDensVal = gridPop[regIdx[[i]]]
      tmpIntVal = sum(tmpVal*tmpDensVal, na.rm = TRUE)/sum(tmpDensVal, na.rm = TRUE)
      regVal[[i]] = c(regVal[[i]], tmpIntVal)
    }    
  }
  
  return(list(regVal = regVal, regIdx = as.vector(mask)))
}