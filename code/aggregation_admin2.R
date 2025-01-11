aggregation_admin2 <- function(RegionSamples, admin.info, res, CI = 0.95){
  imod <- res$inlaRes
  
  admin.info <- admin.info2$data
  admin.info$admin2<- as.numeric(factor(admin.info$admin2.name.full))
  
  admin_name_table<-admin.info
  nregion <- dim(admin_name_table)[1]
  admin_name_table$sID <- 1: dim(admin_name_table)[1] #sID is sorted by admin1.name then admin2.name alphabetically.
  
  c.dat.tmp <- res$tmp
  c.dat.tmp[(dim(c.dat.tmp)[1]+1):(dim(c.dat.tmp)[1]+nregion),"admin2.name.full"] <- admin_name_table[,which(colnames(admin_name_table)=="admin2.name.full")]
  c.dat.tmp$ID <- 1:dim(c.dat.tmp)[1]
  c.dat.tmp$sID <-admin_name_table$sID[match(as.data.frame(c.dat.tmp)[,which(colnames(c.dat.tmp)== "admin2.name.full")],
                                             admin_name_table[,which(colnames(admin_name_table)== "admin2.name.full")])]
  
  admin2.bb.res<- data.frame(cbind(
    admin2.name.full=admin.info$admin2.name.full[tail(c.dat.tmp$sID,n=nregion)],
    mean = sapply(RegionSamples$regVal, function(x) mean(unlist(x), na.rm = TRUE)),
    median = sapply(RegionSamples$regVal, function(x) median(unlist(x), na.rm = TRUE)),
    sd= sapply(RegionSamples$regVal, function(x) sd(unlist(x), na.rm = TRUE)),
    var = sapply(RegionSamples$regVal, function(x) sd(unlist(x), na.rm = TRUE))^2,
    lower = sapply(RegionSamples$regVal, function(x) {
      quantile(unlist(x), probs = c((1 - CI) / 2, 1 - (1 - CI) / 2), na.rm = TRUE)})[1,]),
    upper = sapply(RegionSamples$regVal, function(x) {
      quantile(unlist(x), probs = c((1 - CI) / 2, 1 - (1 - CI) / 2), na.rm = TRUE)})[2,])
  
  
  admin2.bb.res$mean<-as.numeric (admin2.bb.res$mean)
  admin2.bb.res$median<-as.numeric (admin2.bb.res$median)
  admin2.bb.res$sd<-as.numeric (admin2.bb.res$sd)
  admin2.bb.res$var <- admin2.bb.res$sd^2
  admin2.bb.res$lower<-as.numeric (admin2.bb.res$lower)
  admin2.bb.res$upper<-as.numeric (admin2.bb.res$upper)
  admin2.bb.res$cv=admin2.bb.res$sd/admin2.bb.res$mean      
  
  admin2.bb.res<-left_join(admin2.bb.res,distinct(admin.info),by="admin2.name.full")
  
  cm=list(res.admin2=admin2.bb.res, inla=imod,
          admin2_post=RegionSamples)
  attr(cm,"class")="SPDE"
  attr(cm,"domain.names") <- admin.info$admin2.name.full
  return(cm)
}







