local.plot.field <- function(field, proj) {
  stopifnot(length(field) == proj$n)  # Ensure the field length matches the mesh
  field.proj = inla.mesh.project(proj, field)  # Project field onto the grid
  
  # Convert the projected mesh to a data.frame for ggplot
  proj_df <- expand.grid(x = proj$x, y = proj$y)
  proj_df$z <- as.vector(field.proj)
  
  return(proj_df)  # Return the data.frame
}