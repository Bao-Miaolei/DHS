getPopulation = function(loc, regMap, urbRaster){
  regMoll = st_transform(regMap, crs = st_crs(urbRaster))
  
  ext1 = extent(regMoll)
  ext1@xmin = ext1@xmin-0.5
  ext1@xmax = ext1@xmax+0.5
  ext1@ymin = ext1@ymin-0.5
  ext1@ymax = ext1@ymax+0.5
  crRaster = crop(urbRaster, ext1)
  #crRaster = mask(crRaster, regMoll)
  
  # Transform locations to correct projection
  gridP = data.frame(Longitude = loc[, 1],
                     Latitude =  loc[, 2])
  gridP = st_as_sf(gridP, coords = c("Longitude", "Latitude"), crs = st_crs(regMap))
  
  gridP_moll = st_transform(gridP, st_crs(urbRaster))
  loc.proj = st_coordinates(gridP_moll)
  
  # Extract grid from raster
  locRaster = coordinates(crRaster)
  xCor = matrix(locRaster[, 1], ncol = dim(crRaster)[1])
  yCor = matrix(locRaster[, 2], ncol = dim(crRaster)[1])
  val = matrix(values(crRaster), ncol = dim(crRaster)[1])
  
  # Nearest neighbour interpolation
  dx = xCor[2,1]-xCor[1,1]
  dy = yCor[1,2]-yCor[1,1]
  xIdx = round((loc.proj[,1]-xCor[1,1])/dx+1)
  xIdx[(xIdx < 1) | (xIdx > dim(crRaster)[2])] = NA
  yIdx = round((loc.proj[,2]-yCor[1,1])/dy+1)
  yIdx[(yIdx < 1) | (yIdx > dim(crRaster)[1])] = NA
  
  # Get values on grid
  val.int = val[dim(val)[1]*(yIdx-1) + xIdx]
  
  return(val.int)
}