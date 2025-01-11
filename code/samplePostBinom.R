samplePostBinom <- function(res, loc.pred, nSamp){
  # Create projection matrix
  Aproj = inla.spde.make.A(res$mesh, loc = loc.pred)
  
  # Create matrix to store posterior samples
  pSamp = matrix(0, nrow = nSamp, ncol = dim(loc.pred)[1])
  
  # Indicies
  # corr = 0
  
  idxSpace = res$inlaRes$misc$configs$contents$start[3]
  idxIntercept = res$inlaRes$misc$configs$contents$start[5]
  
  nIntNug = 100
  nugSamp = matrix(rnorm(dim(loc.pred)[1]*nIntNug), ncol = nIntNug)
  
  postStmp = inla.posterior.sample(n = nSamp, res$inlaRes, intern = TRUE)
  # Iterate
  for(iSamp in 1:nSamp){
    print(iSamp)
    # Sample posterior
    
    postS = postStmp[[iSamp]]
    # Extract intercept
    intercept = postS$latent[idxIntercept]
    
    # Project spatial effect
    xMesh = postS$latent[idxSpace:(idxSpace+res$mesh$n-1)]
    xSpace = as.matrix(Aproj%*%xMesh)
    
    # Sample without integrating out iid noise
    xPred = intercept + xSpace
    
    # Generate new iid noise
    sigN = exp(-0.5*postS$hyperpar[3])
    pPredInt = 0*xPred
    for(idxSample in 1:nIntNug){
      xPredInt = xPred+sigN*nugSamp[,idxSample]
      pPredInt = pPredInt + exp(xPredInt)/(1+exp(xPredInt))
    }
    pPredInt = pPredInt/nIntNug

    # Store sample
    pSamp[iSamp, ] = pPredInt
  }
  
  return(pSamp)
}