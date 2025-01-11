# data and geo are results from surveyPrev

createSPDE <- function(data, geo, max.edge = 0.25, offset = -0.15, verbose = FALSE, constr = FALSE){
  # Extract data locations
  merged_data <- merge(data, geo, by.x = "cluster", by.y = "DHSCLUST", all.x = TRUE)
  modt<- merged_data[!(is.na(merged_data$LONGNUM)), ]
  
  # Get cluster level information
  c.dat.tmp <- modt %>%
    group_by(cluster) %>%
    mutate(n = length(cluster)) %>%
    mutate(sum_value = sum(value,na.rm = T)) %>%
    ungroup()%>%
    distinct(cluster, .keep_all = TRUE)
  
  loc.mesh = as.matrix(c.dat.tmp[, c("LONGNUM", "LATNUM")])
  
  mesh = inla.mesh.2d(loc.domain = loc.mesh, 
                      max.edge = max.edge, 
                      offset = offset)
  
  distance_matrix <- dist(loc.mesh)
  diameter <- max(distance_matrix)
  
  prior.range = c(diameter / 5, 0.5)
  prior.sigma = c(1, 0.5)
  
  spde = inla.spde2.pcmatern(mesh = mesh,
                             prior.range = prior.range,
                             prior.sigma = prior.sigma)
  
  # Create A matrix
  A = inla.spde.make.A(mesh, loc = loc.mesh)
  
  nObs = nrow(c.dat.tmp)
  eIdx = 1:nObs
  stk.est = inla.stack(tag = "est",
                       data = list(y = c.dat.tmp$sum_value, Ntrials = c.dat.tmp$n),
                       # A = list(A, 1),
                       # effects = list(list(sIdx = 1:mesh$n),
                       #                list(eIdx = eIdx))
                       A = list(A, 1, 1),
                       effects = list(list(sIdx = 1:mesh$n),
                                      list(eIdx = eIdx),
                                      list(beta0 = rep(1, nObs)))
  )
  
  # Formula
  formula = y ~ 0 + beta0 + f(sIdx, model = spde) +
    f(eIdx, model = "iid", hyper = list(theta = list(prior = "pc.prec", params = c(3, 0.05))) )
  fam = "binomial"
  
  # Fit model
  res = inla(formula = formula, 
             family = fam,
             Ntrial = c.dat.tmp$n,
             data = inla.stack.data(stk.est, spde = spde),
             control.predictor = list(A = inla.stack.A(stk.est)),
             control.compute = list(config = TRUE),
             verbose = verbose)
  
  return(list(mesh = mesh, inlaRes = res, spde = spde, stk = stk.est, loc = loc.mesh, tmp = c.dat.tmp))
}