aggregation_admin1 <- function(RegionSamples, admin.info, res, CI = 0.95){
  imod <- res$inlaRes
  
  admin.info <- admin.info$data
  admin_name_table <- admin.info
  nregion <- dim(admin_name_table)[1]
  admin_name_table$sID <- 1: dim(admin_name_table)[1] #sID is sorted by admin1.name then admin2.name alphabetically.
  
  c.dat.tmp <- res$tmp
  c.dat.tmp[(dim(c.dat.tmp)[1]+1):(dim(c.dat.tmp)[1]+nregion), "admin1.name"] <- admin_name_table[,which(colnames(admin_name_table)== "admin1.name")]
  c.dat.tmp$ID <- 1:dim(c.dat.tmp)[1]
  c.dat.tmp$sID <-admin_name_table$sID[match(as.data.frame(c.dat.tmp)[,which(colnames(c.dat.tmp)== "admin1.name")],
                                             admin_name_table[,which(colnames(admin_name_table)== "admin1.name")])]
  
  admin1.bb.res<- data.frame(cbind(
    admin1.name=admin.info$admin1.name[tail(c.dat.tmp$sID,n=nregion)],
    mean = sapply(RegionSamples$regVal, function(x) mean(unlist(x), na.rm = TRUE)),
    median = sapply(RegionSamples$regVal, function(x) median(unlist(x), na.rm = TRUE)),
    sd= sapply(RegionSamples$regVal, function(x) sd(unlist(x), na.rm = TRUE)),
    var = sapply(RegionSamples$regVal, function(x) sd(unlist(x), na.rm = TRUE))^2,
    lower = sapply(RegionSamples$regVal, function(x) {
      quantile(unlist(x), probs = c((1 - CI) / 2, 1 - (1 - CI) / 2), na.rm = TRUE)})[1,]),
    upper = sapply(RegionSamples$regVal, function(x) {
      quantile(unlist(x), probs = c((1 - CI) / 2, 1 - (1 - CI) / 2), na.rm = TRUE)})[2,])
  
  admin1.bb.res$mean<-as.numeric (admin1.bb.res$mean)
  admin1.bb.res$median<-as.numeric (admin1.bb.res$median)
  admin1.bb.res$sd<-as.numeric (admin1.bb.res$sd)
  admin1.bb.res$var<-admin1.bb.res$sd^2
  admin1.bb.res$lower<-as.numeric (admin1.bb.res$lower)
  admin1.bb.res$upper<-as.numeric (admin1.bb.res$upper)
  admin1.bb.res$cv=admin1.bb.res$sd/admin1.bb.res$mean
  
  cm=list(res.admin1=admin1.bb.res,inla=imod,admin1_post=RegionSamples)
  attr(cm,"class")="SPDE"
  attr(cm,"domain.names") <- admin.info$admin1.name
  return(cm)
}