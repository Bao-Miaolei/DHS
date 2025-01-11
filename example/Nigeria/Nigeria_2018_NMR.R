## Load libraries
library(INLA)
library(tidyverse)
library(sf)
library(surveyPrev)
library(raster)
library(SUMMER)
library(gridExtra)

## Load data
rm(list = ls())
load("Nigeria/NMR.RData")

# Get functions
# source()

## Get SPDE results
res <- createSPDE(data = data, geo = geo,
                  max.edge = 0.25, 
                  offset = -0.15,
                  verbose = FALSE, constr = FALSE)

## Create prediction grid
pgrid0 <- inla.mesh.projector(res$mesh, xlim = c(1, 15), ylim = c(3, 14),
                              dims = c(1000, 1000))

## Plot continuous spatial surface
mesh_proj_spatial_mean <- local.plot.field(res$inlaRes$summary.random[["sIdx"]][["mean"]], pgrid0)
ggplot() +
  geom_raster(data = mesh_proj_spatial_mean, aes(x = x, y = y, fill = z)) + 
  scale_fill_viridis_c() +  # Choose a color scale (viridis in this case)
  coord_fixed(ratio = 1) +  # Fix aspect ratio
  labs(title = "Continuous Spatial Surface", fill = "spatial") +
  # Add the administrative regions overlay
  geom_sf(data = poly.adm2, fill = NA, color = "black") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Adjust legend position

###############################################################################
# Aggregation                                                                 #
###############################################################################

## Draw posterior samples
pSamp <- samplePostBinom(res = res,
                         loc.pred = pgrid0$lattice$loc,
                         nSamp = 100)

poly.adm0 <- st_as_sf(geodata::gadm(country = country_code, level=0, path=tempdir()))

NationalSamples <- getNationalSamples(pSamp, 
                                      loc = pgrid0$lattice$loc, 
                                      regMap = poly.adm0)

pMean = colMeans(NationalSamples$filtered_pSamp)
pSD   = sqrt(colMeans(NationalSamples$filtered_pSamp^2)-pMean^2)

CI = 0.95
pWidth = apply(NationalSamples$filtered_pSamp, 2, function(x) {
  upper = quantile(x, probs = 1 - (1 - CI) / 2, na.rm = TRUE)
  lower = quantile(x, probs = (1 - CI) / 2, na.rm = TRUE)
  return(upper - lower)
})

## Plot mean
mesh_proj <- mesh_proj_spatial_mean[NationalSamples$inside_indices,]
mesh_proj$z = pMean
ggplot() +
  geom_raster(data = mesh_proj, aes(x = x, y = y, fill = z)) + 
  scale_fill_viridis_c(name = "Mean", direction = -1) +  # Choose a color scale (viridis in this case)
  coord_fixed(ratio = 1) +  # Fix aspect ratio
  labs(title = "Continuous Prediction Surface, Mean", fill = "DPT3") +
  # Add the administrative regions overlay
  geom_sf(data = poly.adm2, fill = NA, color = "black") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Adjust legend position

## Plot CV
mesh_proj$z = pSD/pMean*100
ggplot() +
  geom_raster(data = mesh_proj, aes(x = x, y = y, fill = z)) + 
  scale_fill_viridis_c(name = "CV", option = "G", direction = -1) +  # Choose a color scale (viridis in this case)
  coord_fixed(ratio = 1) +  # Fix aspect ratio
  labs(title = "Continuous Prediction Surface, CV", fill = "%") +
  # Add the administrative regions overlay
  geom_sf(data = poly.adm2, fill = NA, color = "black") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Adjust legend position

## Plot width
mesh_proj$z = pWidth
ggplot() +
  geom_raster(data = mesh_proj, aes(x = x, y = y, fill = z)) + 
  scale_fill_gradientn(colours = rev(viridisLite::plasma(10)[3:10])) +
  coord_fixed(ratio = 1) +  # Fix aspect ratio
  labs(title = "Continuous Prediction Surface, 95% Width", fill = "95% CI\nwidth") +
  # Add the administrative regions overlay
  geom_sf(data = poly.adm2, fill = NA, color = "black") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Adjust legend position

## Get population density map
urbRaster = raster("nga_ppp_2018_1km_Aggregated.tif")

## Assign samples to regions
RegionSamples_admin1 <- getRegionSamples(pSamp = pSamp,
                                         loc = pgrid0$lattice$loc,
                                         regMap = poly.adm1,
                                         urbRaster = urbRaster)

RegionSamples_admin2 <- getRegionSamples(pSamp = pSamp,
                                         loc = pgrid0$lattice$loc,
                                         regMap = poly.adm2,
                                         urbRaster = urbRaster)

## Aggregation
out_spde_admin1 <- aggregation_admin1(RegionSamples_admin1, admin.info = admin.info1, res = res)
out_spde_admin2 <- aggregation_admin2(RegionSamples_admin2, admin.info = admin.info2, res = res)

## Visualization
###### Get estimates from suvreyPrev
out_admin1 <- res_ad1$res.admin1[, c("admin1.name", "direct.est", "cv", "direct.lower", "direct.upper")] 
colnames(out_admin1)[c(2, 4, 5)] <- c("mean", "lower", "upper")
out_admin1$model <- "Direct Estimates"
out_admin1$width <- out_admin1$upper - out_admin1$lower

out1 <-  res_ad2$res.admin2[, c("admin2.name.full", "direct.est", "cv", "direct.lower", "direct.upper")]
colnames(out1)[c(2, 4, 5)] <- c("mean", "lower", "upper")
out1$model <- "Direct Estimates"
out2 <- smth_res_ad2_bym2$res.admin2[, c("admin2.name.full", "mean", "cv", "lower", "upper")]
out2$model <- "Fay-Herriot Model"
out3 <- cl_res_ad2$res.admin2[, c("admin2.name.full", "mean", "cv", "lower", "upper")]
out3$model <- "Unstratified Cluster-level Model"
out <- rbind(out2, out3)
out$model <- factor(out$model, levels = unique(out$model))
out$width <- out$upper - out$lower


###### Admin1: Direct Estimates vs SPDE
scatterPlot(res1=res_ad1$res.admin1, res2=out_spde_admin1$res.admin1,
            value1="direct.est", value2="mean",
            by.res1="admin1.name", by.res2="admin1.name",
            title="Direct Estimates vs SPDE",
            label1="Direct Estimates", label2="SPDE")

out_spde_admin1 <- out_spde_admin1$res.admin1[, c("admin1.name", "mean", "cv", "lower", "upper")]
out_spde_admin1$width <- out_spde_admin1$upper - out_spde_admin1$lower
out_spde_admin1$model <- "SPDE"

g1_admin1 <- mapPlot(data = rbind(out_spde_admin1, out_admin1), geo = poly.adm1,
                     by.data = "admin1.name", by.geo = "NAME_1", is.long = TRUE, 
                     variable = "model", value = "mean", legend.label = "Mean",
                     direction = -1, ncol = 4, size = .05, border = "gray50")
g2_admin1 <- mapPlot(data = rbind(out_spde_admin1, out_admin1), geo = poly.adm1,
                     by.data = "admin1.name", by.geo = "NAME_1", is.long = TRUE, 
                     variable = "model", value = "cv", legend.label = "CV", ncol = 4,
                     size = .05, border = "gray50") + scale_fill_viridis_c("CV", option = "G", direction = -1)
g3_admin1 <- mapPlot(data = rbind(out_spde_admin1, out_admin1), geo = poly.adm1,
                     by.data = "admin1.name", by.geo = "NAME_1", is.long = TRUE, 
                     variable = "model", value = "width", legend.label = "CV", ncol = 4,
                     size = .05, border = "gray50") + 
  # scale_fill_viridis_c("95% CI\nwidth", option = "B", direction = -1)
  scale_fill_gradientn(colours = rev(viridisLite::plasma(10)[3:10]))
  # scale_fill_viridis_c("95% CI\nwidth", option = "B", direction = -1)
# grid.arrange(g1_admin1, g2_admin1, g3_admin1, ncol = 2)
ggsave(g1_admin1, filename= paste0("admin1_mean",".png"), width = 6, height = 6)
ggsave(g2_admin1, filename= paste0("admin1_cv",".png"), width = 6, height = 6)
ggsave(g3_admin1, filename= paste0("admin1_width",".png"), width = 6, height = 6)

g1_admin1 <- mapPlot(data = rbind(out_spde_admin1), geo = poly.adm1,
                     by.data = "admin1.name", by.geo = "NAME_1", is.long = TRUE, 
                     variable = "model", value = "mean", legend.label = "Mean",
                     direction = -1, ncol = 4, size = .05, border = "gray50")
g2_admin1 <- mapPlot(data = rbind(out_spde_admin1), geo = poly.adm1,
                     by.data = "admin1.name", by.geo = "NAME_1", is.long = TRUE, 
                     variable = "model", value = "cv", legend.label = "CV", ncol = 4,
                     size = .05, border = "gray50") + scale_fill_viridis_c("CV", option = "G", direction = -1)
g3_admin1 <- mapPlot(data = rbind(out_spde_admin1), geo = poly.adm1,
                     by.data = "admin1.name", by.geo = "NAME_1", is.long = TRUE, 
                     variable = "model", value = "cv", legend.label = "CV", ncol = 4,
                     size = .05, border = "gray50") + scale_fill_viridis_c("95% CI\nwidth", option = "B", direction = -1)

combined_plot <- grid.arrange(g1_admin1, g2_admin1, g3_admin1, ncol = 3)
ggsave("combined_plot.png", plot = combined_plot, width = 12, height = 4)

###### Admin2: Unstratified Cluster-Level Model vs SPDE, Fay-Herriot Model vs SPDE
scatterPlot(res1=cl_res_ad2$res.admin2, res2=out_spde_admin2$res.admin2,
            value1="mean", value2="mean",
            by.res1="admin2.name.full", by.res2="admin2.name.full",
            title="Unstratified Cluster-Level Model vs SPDE",
            label1="Unstratified Cluster-Level Model", label2="SPDE")

scatterPlot(res1=smth_res_ad2_bym2$res.admin2, res2=out_spde_admin2$res.admin2,
            value1="mean", value2="mean", 
            by.res1="admin2.name.full", by.res2="admin2.name.full",
            title="Fay-Herriot Model vs SPDE",
            label1="Fay-Herriot Model", label2="SPDE")


out_spde_admin2 <- out_spde_admin2$res.admin2[, c("admin2.name.full", "mean", "cv", "lower", "upper")]
out_spde_admin2$width <- out_spde_admin2$upper - out_spde_admin2$lower
out_spde_admin2$model <- "SPDE"

poly.adm2$admin2.name.full <- paste0(poly.adm2$NAME_1, "_", poly.adm2$NAME_2)
g1_admin2 <- mapPlot(data = rbind(out_spde_admin2, out), geo = poly.adm2, 
                     by.data = "admin2.name.full", 
                     by.geo = "admin2.name.full", 
                     is.long = TRUE,
                     variable = "model", value = "mean", legend.label = "Mean",
                     direction = -1, ncol = 4, size = .05)
g2_admin2 <- mapPlot(data = rbind(out_spde_admin2, out), 
                     geo = poly.adm2, by.data = "admin2.name.full", 
                     by.geo = "admin2.name.full", is.long = TRUE, 
                     variable = "model", value = "cv", legend.label = "CV", ncol = 4,
                     size = .05) + scale_fill_viridis_c("CV", option = "G", direction = -1)
g3_admin2 <- mapPlot(data = rbind(out_spde_admin2, out), 
                     geo = poly.adm2, by.data = "admin2.name.full", 
                     by.geo = "admin2.name.full", is.long = TRUE, 
                     variable = "model", value = "width", legend.label = "CV", ncol = 4,
                     size = .05) + scale_fill_gradientn(colours = rev(viridisLite::plasma(10)[3:10]))
ggsave(g1_admin2, filename= paste0("admin2_mean",".png"), width = 8, height = 8)
ggsave(g2_admin2, filename= paste0("admin2_cv",".png"), width = 8, height = 8)
ggsave(g3_admin2, filename= paste0("admin2_width",".png"), width = 8, height = 8)


g1_admin2 <- mapPlot(data = rbind(out_spde_admin2), geo = poly.adm2, 
                     by.data = "admin2.name.full", 
                     by.geo = "admin2.name.full", 
                     is.long = TRUE,
                     variable = "model", value = "mean", legend.label = "Mean",
                     direction = -1, ncol = 4, size = .05)
g2_admin2 <- mapPlot(data = rbind(out_spde_admin2), 
                     geo = poly.adm2, by.data = "admin2.name.full", 
                     by.geo = "admin2.name.full", is.long = TRUE, 
                     variable = "model", value = "cv", legend.label = "CV", ncol = 4,
                     size = .05) + scale_fill_viridis_c("CV", option = "G", direction = -1)
g3_admin2 <- mapPlot(data = rbind(out_spde_admin2), 
                     geo = poly.adm2, by.data = "admin2.name.full", 
                     by.geo = "admin2.name.full", is.long = TRUE, 
                     variable = "model", value = "cv", legend.label = "CV", ncol = 4,
                     size = .05) + scale_fill_viridis_c("95% CI\nwidth", option = "B", direction = -1)
grid.arrange(g1_admin2, g2_admin2, g3_admin2, ncol = 2)









