#*****************************************************************************************************************************************************************************************************************************************************************
### RSF - Demo of density vs. distance ----
#*****************************************************************************************************************************************************************************************************************************************************************

### 1. Add distance to houses and private cabins ----
library(NinaR)
library(rgrass7)
library(raster)
library(rgdal)
library(sp)

grassConnect()
use_sp()

execGRASS("g.mapset", flags = "l")
execGRASS("g.list", parameters = list(type = "raster"))
execGRASS("g.list", parameters = list(type = "vector"))

set_region <- "wild_reindeer_areas"

execGRASS("g.region", parameters = list(vector=set_region, align="houses_10000@p_prodchange_envpoints"))

areas <- readVECT(vname="wild_reindeer_areas")
rast <- raster(readRAST("houses_10000@p_prodchange_envpoints", plugin=FALSE))

execGRASS("g.list", parameters = list(type = "raster"))
execGRASS("g.list", parameters = list(type = "vector"))

plot(rast)
plot(areas, add=T)

execGRASS("v.to.rast", parameters = list(input="houses@p_prodchange_envpoints", output="houses_rast", use="cat"), flags=c("overwrite"))
execGRASS("r.grow.distance", parameters = list(input="houses_rast", distance="dist_houses"), flags=c("overwrite"))

dist_houses <- raster(readRAST("dist_houses", plugin=FALSE))
plot(dist_houses)
plot(areas, add=T)

execGRASS("v.to.rast", parameters = list(input="private_cabins@p_prodchange_envpoints", output="private_cabins_rast", use="cat"), flags=c("overwrite"))
execGRASS("r.grow.distance", parameters = list(input="private_cabins_rast", distance="dist_private_cabins"), flags=c("overwrite"))

dist_private_cabins <- raster(readRAST("dist_private_cabins", plugin=FALSE))
plot(dist_private_cabins)
plot(areas, add=T)

execGRASS("v.to.rast", parameters = list(input="pub_cabins_summer_high@p_prodchange_envpoints", output="pub_cabins_summer_high_rast", use="cat"), flags=c("overwrite"))
execGRASS("r.grow.distance", parameters = list(input="pub_cabins_summer_high_rast", distance="dist_pub_cabins_summer_high"), flags=c("overwrite"))

dist_pub_cabins <- raster(readRAST("dist_pub_cabins_summer_high", plugin=FALSE))
plot(dist_pub_cabins)
plot(areas, add=T)

filename <- "wrein_dat_sub_20200518V2.rda"
load(file=paste0("/data/R/Prosjekter/Rein/RSF_output/", filename)) #servers, e.g. ninrstudio03

dat <- dat$sum
names(dat)
dat <- dat[dat$herd!="Sollia",] # remove Sollia, as we have only 1 animal and we likely have wrong avaialbility

dat$NORUTreclass <- ifelse(dat$norut<9, "11forest", ifelse(dat$norut<12, "bog", ifelse(dat$norut<21, "mountain", ifelse(dat$norut<22, "glacier", ifelse(dat$norut<23, "water", ifelse(dat$norut<25,"other", NA))))))
dat$NORUTreclass <- ifelse(dat$NORUTreclass=="mountain", dat$norut, dat$NORUTreclass) 
tapply(dat$use, dat$NORUTreclass, mean)  # explore the proportion of each used point in the NORUT groups
tapply(dat$use, dat$NORUTreclass, length) # explore the number 
sum(is.na(dat$NORUTreclass))

dist_houses <- raster(readRAST("dist_houses", plugin=FALSE))
plot(dist_houses)
plot(areas, add=T)

dist_private_cabins <- raster(readRAST("dist_private_cabins", plugin=FALSE))
plot(dist_private_cabins)
plot(areas, add=T)

dist_pub_cabins <- raster(readRAST("dist_pub_cabins_summer_high", plugin=FALSE))
plot(dist_pub_cabins)
plot(areas, add=T)

dat$dist_houses <- extract(dist_houses, SpatialPoints(dat[,c("x33", "y33")]))
dat$dist_private_cabins <- extract(dist_private_cabins, SpatialPoints(dat[,c("x33", "y33")]))
dat$dist_pub_cabins <- extract(dist_pub_cabins, SpatialPoints(dat[,c("x33", "y33")]))

save(dat, file="/data/R/Prosjekter/Rein/tmp/ZoI_dataset.rda")


### 2. Add different density shapes of houses and private cabins ----
# * test shapes ======
feat <- SpatialPointsDataFrame(coords=data.frame(x=100000,y=6700000), data=data.frame(id="a"))
writeVECT(feat, vname="tmp", v.in.ogr_flags=c("o","t","overwrite"), driver="SQLite")
execGRASS("v.to.rast", parameters = list(input="tmp", output="tmp_rast", use="val"), flags="overwrite")
execGRASS("r.mapcalc", parameters = list(expression="tmp_rast = if(isnull(tmp_rast), 0, tmp_rast)"), flags="overwrite")
halflife=2500
scal=halflife*0.85 
execGRASS("r.resamp.filter", parameters = list(input="tmp_rast", output=paste0("tmp_", scal), filter="normal,box", radius=c(scal, 3*scal)), flags=c("overwrite"))

rast <- raster(readRAST(vname=paste0("tmp_", scal, "@u_bram.van.moorter"), plugin=FALSE))
plot(rast)
transect <- SpatialLinesDataFrame(SpatialLines(list(Lines(Line(cbind(x=c(100000, 110000),y=c(6700000, 6700000))), ID="a"))), data=data.frame(id="A"), match.ID = F)
plot(transect, add=T)
vals <- extract(rast, transect)[[1]]
plot(seq(0, 10000, length.out = length(vals)), vals/max(vals), type="l"); abline(v=halflife); abline(h=0.5)

scal=2*halflife
execGRASS("r.resamp.filter", parameters = list(input="tmp_rast", output=paste0("tmp_", scal), filter="bartlett", radius=scal), flags=c("overwrite"))
rast <- raster(readRAST(vname=paste0("tmp_", scal, "@u_bram.van.moorter"), plugin=FALSE))
#plot(rast)
transect <- SpatialLinesDataFrame(SpatialLines(list(Lines(Line(cbind(x=c(100000, 110000),y=c(6700000, 6700000))), ID="a"))), data=data.frame(id="A"), match.ID = F)
#plot(transect, add=T)
vals2 <- extract(rast, transect)[[1]]
plot(seq(0, 10000, length.out = length(vals)), vals2/max(vals2), type="l"); abline(v=halflife); abline(h=0.5)
lines(seq(0, 10000, length.out = length(vals)), vals/max(vals), col="grey")

scal=2*halflife
scal
execGRASS("r.mfilter", parameters = list(input="tmp_rast", output=paste0("tmp_", scal), 
                                         filter=gsub("XXX", scal, "/home/NINA.NO/bram.van.moorter/Mounted/Egenutvikling/12084000_egenutvikling_bram_van_moorter/reindoc_rproj/Data/Env_Data/filters/thresholdXXX.txt")), flags=c("overwrite"))
rast <- raster(readRAST(vname=paste0("tmp_", scal, "@u_bram.van.moorter"), plugin=FALSE))
plot(rast)
transect <- SpatialLinesDataFrame(SpatialLines(list(Lines(Line(cbind(x=c(100000, 110000),y=c(6700000, 6700000))), ID="a"))), data=data.frame(id="A"), match.ID = F)
#plot(transect, add=T)
vals2 <- extract(rast, transect)[[1]]
plot(seq(0, 10000, length.out = length(vals)), vals2/max(vals2), type="l"); abline(v=halflife); abline(v=halflife*2); abline(h=0.5)
lines(seq(0, 10000, length.out = length(vals)), vals/max(vals), col="grey")


# * Gaussian ======
# ** private cabins #####
scales <- c(100, 250, 500, 1000, 2500, 5000, 10000, 20000)
halflife <- scales/2
i=3
scal <- halflife[i]*0.85
execGRASS("r.mapcalc", parameters = list(expression="private_cabins_rast = if(isnull(private_cabins_rast), 0, private_cabins_rast)"), flags="overwrite")
execGRASS("r.resamp.filter", parameters = list(input="private_cabins_rast", output=paste0("private_cabins_V2_", scal), 
                                               filter="normal,box", radius=c(scal, 3*scal)), flags=c("overwrite"))
dens_private_cabins <- raster(readRAST(paste0("private_cabins_V2_", scal), plugin=FALSE))
plot(dens_private_cabins)
plot(areas, add=T)

i=1
for (i in c(1:length(scales))){
  scal <- (scales[i]/2)*0.85
  execGRASS("r.resamp.filter", parameters = list(input="private_cabins_rast", output="tmp", filter="normal,box", radius=c(scal, 3*scal)), flags=c("overwrite"))
  dens_private_cabins <- raster(readRAST("tmp", plugin=FALSE))
  dat$dens_private_cabinsV2 <- extract(dens_private_cabins, SpatialPoints(dat[,c("x33", "y33")]))
  names(dat)[ncol(dat)] <- paste0("dens_private_cabinsV2_", scales[i])
}

save(dat, file="/data/R/Prosjekter/Rein/tmp/ZoI_dataset.rda")

# ** public cabins #####
execGRASS("r.mapcalc", parameters = list(expression="pub_cabins_summer_high_rast = if(isnull(pub_cabins_summer_high_rast), 0, pub_cabins_summer_high_rast)"), flags="overwrite")
execGRASS("r.resamp.filter", parameters = list(input="pub_cabins_summer_high_rast", output=paste0("pub_cabins_summer_high_V2_", scal), 
                                               filter="normal,box", radius=c(scal, 3*scal)), flags=c("overwrite"))
dens_pub_cabins_summer_high <- raster(readRAST(paste0("pub_cabins_summer_high_V2_", scal), plugin=FALSE))
plot(dens_pub_cabins_summer_high)
plot(areas, add=T)

i=1
for (i in c(1:length(scales))){
  scal <- (scales[i]/2)*0.85
  execGRASS("r.resamp.filter", parameters = list(input="pub_cabins_summer_high_rast", output="tmp", filter="normal,box", radius=c(scal, 3*scal)), flags=c("overwrite"))
  dens_pub_cabins <- raster(readRAST("tmp", plugin=FALSE))
  dat$dens_pub_cabinsV2 <- extract(dens_pub_cabins, SpatialPoints(dat[,c("x33", "y33")]))
  names(dat)[ncol(dat)] <- paste0("dens_pub_cabinsV2_", scales[i])
}

save(dat, file="/data/R/Prosjekter/Rein/tmp/ZoI_dataset.rda")

# ** houses #####
i=3
scal <- halflife[i]*0.85
execGRASS("r.mapcalc", parameters = list(expression="houses_rast = if(isnull(houses_rast), 0, houses_rast)"), flags="overwrite")
execGRASS("r.resamp.filter", parameters = list(input="houses_rast", output=paste0("houses_V2_", scal), 
                                               filter="normal,box", radius=c(scal, 3*scal)), flags=c("overwrite"))
dens_houses <- raster(readRAST(paste0("houses_V2_", scal), plugin=FALSE))
plot(dens_houses)
plot(areas, add=T)

i=1
for (i in c(1:length(scales))){
  scal <- (scales[i]/2)*0.85
  execGRASS("r.resamp.filter", parameters = list(input="houses_rast", output="tmp", filter="normal,box", radius=c(scal, 3*scal)), flags=c("overwrite"))
  dens_houses <- raster(readRAST("tmp", plugin=FALSE))
  dat$dens_housesV2 <- extract(dens_houses, SpatialPoints(dat[,c("x33", "y33")]))
  names(dat)[ncol(dat)] <- paste0("dens_housesV2_", scales[i])
}

save(dat, file="/data/R/Prosjekter/Rein/tmp/ZoI_dataset.rda")

# * Treshold ======
# ** private cabins #####
i=2
scales <- c(100, 250, 500, 1000, 2500, 5000, 10000, 20000)
(scal <- scales[i])
execGRASS("r.mfilter", parameters = list(input="private_cabins_rast", output="tmp", 
                                         filter=gsub("XXX", scal, "/home/NINA.NO/bram.van.moorter/Mounted/Egenutvikling/12084000_egenutvikling_bram_van_moorter/reindoc_rproj/Data/Env_Data/filters/thresholdXXX.txt")), flags=c("overwrite"))
dens_private_cabins <- raster(readRAST("tmp", plugin=FALSE))
plot(dens_private_cabins)
plot(areas, add=T)

i=1
for (i in c(1:length(scales))){
  scal <- scales[i]
  execGRASS("r.mfilter", parameters = list(input="private_cabins_rast", output="tmp", 
                                           filter=gsub("XXX", scal, "/home/NINA.NO/bram.van.moorter/Mounted/Egenutvikling/12084000_egenutvikling_bram_van_moorter/reindoc_rproj/Data/Env_Data/filters/thresholdXXX.txt")), flags=c("overwrite"))
  dens_private_cabins <- raster(readRAST("tmp", plugin=FALSE))
  dat$dens_private_cabinsV3 <- extract(dens_private_cabins, SpatialPoints(dat[,c("x33", "y33")]))
  names(dat)[ncol(dat)] <- paste0("dens_private_cabinsV3_", scales[i])
}
plot(dens_private_cabins)
save(dat, file="/data/R/Prosjekter/Rein/tmp/ZoI_dataset.rda")
names(dat)

# ** public cabins #####
for (i in c(1:length(scales))){
  scal <- scales[i]
  execGRASS("r.mfilter", parameters = list(input="pub_cabins_summer_high_rast", output="tmp", 
                                           filter=gsub("XXX", scal, "/home/NINA.NO/bram.van.moorter/Mounted/Egenutvikling/12084000_egenutvikling_bram_van_moorter/reindoc_rproj/Data/Env_Data/filters/thresholdXXX.txt")), flags=c("overwrite"))
  dens_pub_cabins <- raster(readRAST("tmp", plugin=FALSE))
  dat$dens_pub_cabinsV3 <- extract(dens_pub_cabins, SpatialPoints(dat[,c("x33", "y33")]))
  names(dat)[ncol(dat)] <- paste0("dens_pub_cabinsV3_", scales[i])
}

save(dat, file="/data/R/Prosjekter/Rein/tmp/ZoI_dataset.rda")

# ** houses #####
for (i in c(1:length(scales))){
  scal <- scales[i]
  execGRASS("r.mfilter", parameters = list(input="houses_rast", output="tmp", 
                                           filter=gsub("XXX", scal, "/home/NINA.NO/bram.van.moorter/Mounted/Egenutvikling/12084000_egenutvikling_bram_van_moorter/reindoc_rproj/Data/Env_Data/filters/thresholdXXX.txt")), flags=c("overwrite"))
  dens_houses <- raster(readRAST("tmp", plugin=FALSE))
  dat$dens_housesV3 <- extract(dens_houses, SpatialPoints(dat[,c("x33", "y33")]))
  names(dat)[ncol(dat)] <- paste0("dens_housesV3_", scales[i])
}
save(dat, file="/data/R/Prosjekter/Rein/tmp/ZoI_dataset.rda")
names(dat)

# * Cone - Bartlett ======
# add the 20K version too
# ** private cabins #####
(scal <- 20000)
execGRASS("r.resamp.filter", parameters = list(input="private_cabins_rast", output="tmp", filter="bartlett", radius=scal), flags=c("overwrite"))
dens_private_cabins <- raster(readRAST("tmp", plugin=FALSE))
plot(dens_private_cabins)
plot(areas, add=T)
dat$private_cabins_20000 <- extract(dens_private_cabins, SpatialPoints(dat[,c("x33", "y33")]))

save(dat, file="/data/R/Prosjekter/Rein/tmp/ZoI_dataset.rda")
names(dat)

# ** public cabins #####
execGRASS("r.resamp.filter", parameters = list(input="pub_cabins_summer_high_rast", output="tmp", filter="bartlett", radius=scal), flags=c("overwrite"))
dens_pub_cabins <- raster(readRAST("tmp", plugin=FALSE))
dat$pub_cabins_summer_high_20000 <- extract(dens_pub_cabins, SpatialPoints(dat[,c("x33", "y33")]))

save(dat, file="/data/R/Prosjekter/Rein/tmp/ZoI_dataset.rda")


### 4. Fit models ----
load(file="/data/R/Prosjekter/Rein/tmp/ZoI_dataset.rda")
names(dat)
dat <- dat[dat$herd=="Hardangervidda",]


library(pbapply)
library(ResourceSelection)
library(survival)


results <- expand.grid(public_cabin_zoi = c(100, 250, 500, 1000, 2500, 5000, 10000, 20000), 
                       private_cabin_zoi=c(100, 250, 500, 1000, 2500, 5000, 10000, 20000),
                       public_cabin_shape = c(1,2,3,4,5,6), private_cabin_shape = c(1,2,3,4,5,6))

results$AIC <- NA

head(results)
nrow(results)


i=100
  pb <- txtProgressBar(min = 0, max = nrow(results), style = 3)
for (i in c(1:nrow(results))){
  ZoI_h <- results$public_cabin_zoi[i]
  ZoI_pc <- results$private_cabin_zoi[i]
  
  dat$ZoI_pub_cabins <- ifelse(dat$dist_pub_cabins>ZoI_h, 0, 1-(dat$dist_pub_cabins/ZoI_h))
  if (results$public_cabin_shape[i]==2) dat$ZoI_pub_cabins <- exp(-0.5 * (dat$dist_pub_cabins/((ZoI_h/2)*0.85))^2)
  if (results$public_cabin_shape[i]==3) dat$ZoI_pub_cabins <- ifelse(dat$dist_pub_cabins>ZoI_h, 0, 1)
  if (results$public_cabin_shape[i]==4) dat$ZoI_pub_cabins <- dat[,match(paste0("pub_cabins_summer_high_", ZoI_h), names(dat))]
  if (results$public_cabin_shape[i]==5) dat$ZoI_pub_cabins <- dat[,match(paste0("dens_pub_cabinsV2_", ZoI_h), names(dat))]
  if (results$public_cabin_shape[i]==6) dat$ZoI_pub_cabins <- dat[,match(paste0("dens_pub_cabinsV3_", ZoI_h), names(dat))]
  
  dat$ZoI_private_cabins <- ifelse(dat$dist_private_cabins>ZoI_pc, 0, 1-(dat$dist_private_cabins/ZoI_pc))
  if (results$private_cabin_shape[i]==2) dat$ZoI_private_cabins <- exp(-0.5 * (dat$dist_private_cabins/((ZoI_pc/2)*0.85))^2)
  if (results$private_cabin_shape[i]==3) dat$ZoI_private_cabins <- ifelse(dat$dist_private_cabins>ZoI_h, 0, 1)
  if (results$private_cabin_shape[i]==4) dat$ZoI_private_cabins <- dat[,match(paste0("private_cabins_", ZoI_pc), names(dat))]
  if (results$private_cabin_shape[i]==5) dat$ZoI_private_cabins <- dat[,match(paste0("dens_private_cabinsV2_", ZoI_pc), names(dat))]
  if (results$private_cabin_shape[i]==6) dat$ZoI_private_cabins <- dat[,match(paste0("dens_private_cabinsV3_", ZoI_pc), names(dat))]
  
#  mod <- rsf(use ~ ZoI_pub_cabins + ZoI_private_cabins + NORUTreclass + norway_pca_klima_axis1 + norway_pca_klima_axis2 + norway_pca_klima_axis3 + norway_pca_klima_axis4, 
#             m=0, B=0, data = dat)
#  mod <- glm(use ~ ZoI_pub_cabins + ZoI_private_cabins + NORUTreclass + norway_pca_klima_axis1 + norway_pca_klima_axis2 + norway_pca_klima_axis3 + norway_pca_klima_axis4, 
#             data = dat, family=binomial)
  
  mod <- coxph(Surv(rep(1, length(use)), use) ~ ZoI_pub_cabins + ZoI_private_cabins + NORUTreclass + 
          poly(norway_pca_klima_axis1, 2) + poly(norway_pca_klima_axis2,2) + norway_pca_klima_axis3 + norway_pca_klima_axis4, data = dat)
  results$AIC[i] <- AIC(mod)
  
  setTxtProgressBar(pb, i)
}

#save(results, file="/data/R/Prosjekter/Rein/tmp/ZoI_results_rsf.rda")
load(file="/data/R/Prosjekter/Rein/tmp/ZoI_results_rsf.rda")
  
  
results[which.min(results$AIC),]

results$dAIC <- results$AIC - min(results$AIC)
results$relLL <- exp(-0.5 * results$dAIC)
results$AICweights <- results$relLL/sum(results$relLL)


head(results[order(-results$AICweights),])

# ** output model selection table #######
library(xtable)
moddf <- head(results[order(-results$AICweights),], 10)
moddf <- moddf[,c(1,3,3,2,4,4,5,6,8)]
names(moddf) <- c("ZoI public cabin", "Shape public cabin", "Cumulative public cabin", "ZoI private cabin", "Shape private cabin", "Cumulative private cabin", "AIC", "dAIC", "Akaike weight")
moddf[,2] <- ifelse(moddf[,2] %in% c(1,4), "linear", ifelse(moddf[,2] %in% c(2,5), "Gaussian", "threshold"))
moddf[,5] <- ifelse(moddf[,5] %in% c(1,4), "linear", ifelse(moddf[,5] %in% c(2,5), "Gaussian", "threshold"))
moddf[,3] <- ifelse(moddf[,3] < 4, "nearest", "all")
moddf[,6] <- ifelse(moddf[,6] < 4, "nearest", "all")
moddf[,1] <- as.character(moddf[,1])
moddf[,4] <- as.character(moddf[,4])
moddf$AIC <- round(moddf$AIC, 0)
moddf$AIC <- as.character(moddf$AIC)
moddf$dAIC <- round(moddf$dAIC, 2)
moddf$dAIC <- as.character(moddf$dAIC)
moddf[,ncol(moddf)] <- format(round(moddf[,ncol(moddf)], 4), nsmall=4)
moddf
xtable(moddf)


# * Re-fit best model ====
library(survival)
(i=which.min(results$AIC)) #2232
ZoI_h <- results$public_cabin_zoi[i]
ZoI_pc <- results$private_cabin_zoi[i]

dat$ZoI_pub_cabins <- ifelse(dat$dist_pub_cabins>ZoI_h, 0, 1-(dat$dist_pub_cabins/ZoI_h))
if (results$public_cabin_shape[i]==2) dat$ZoI_pub_cabins <- exp(-0.5 * (dat$dist_pub_cabins/((ZoI_h/2)*0.85))^2)
if (results$public_cabin_shape[i]==3) dat$ZoI_pub_cabins <- ifelse(dat$dist_pub_cabins>ZoI_h, 0, 1)
if (results$public_cabin_shape[i]==4) dat$ZoI_pub_cabins <- dat[,match(paste0("pub_cabins_summer_high_", ZoI_h), names(dat))]
if (results$public_cabin_shape[i]==5) dat$ZoI_pub_cabins <- dat[,match(paste0("dens_pub_cabinsV2_", ZoI_h), names(dat))]
if (results$public_cabin_shape[i]==6) dat$ZoI_pub_cabins <- dat[,match(paste0("dens_pub_cabinsV3_", ZoI_h), names(dat))]

dat$ZoI_private_cabins <- ifelse(dat$dist_private_cabins>ZoI_pc, 0, 1-(dat$dist_private_cabins/ZoI_pc))
if (results$private_cabin_shape[i]==2) dat$ZoI_private_cabins <- exp(-0.5 * (dat$dist_private_cabins/((ZoI_pc/2)*0.85))^2)
if (results$private_cabin_shape[i]==3) dat$ZoI_private_cabins <- ifelse(dat$dist_private_cabins>ZoI_h, 0, 1)
if (results$private_cabin_shape[i]==4) dat$ZoI_private_cabins <- dat[,match(paste0("private_cabins_", ZoI_pc), names(dat))]
if (results$private_cabin_shape[i]==5) dat$ZoI_private_cabins <- dat[,match(paste0("dens_private_cabinsV2_", ZoI_pc), names(dat))]
if (results$private_cabin_shape[i]==6) dat$ZoI_private_cabins <- dat[,match(paste0("dens_private_cabinsV3_", ZoI_pc), names(dat))]

mod <- coxph(Surv(rep(1, length(use)), use) ~ ZoI_pub_cabins + ZoI_private_cabins + NORUTreclass + 
               poly(norway_pca_klima_axis1, 2) + poly(norway_pca_klima_axis2,2) + norway_pca_klima_axis3 + norway_pca_klima_axis4, data = dat)
summary(mod)


# ** output summary table #######
moddf <- as.data.frame(summary(mod)$coefficients)
moddf$var_names <- rownames(moddf)
moddf$var_names[grepl("reclass12", moddf$var_names)] <- "exposed ridges"
moddf$var_names[grepl("reclass13", moddf$var_names)] <- "grass ridges"
moddf$var_names[grepl("reclass14", moddf$var_names)] <- "heather ridges"
moddf$var_names[grepl("reclass15", moddf$var_names)] <- "lichen"
moddf$var_names[grepl("reclass16", moddf$var_names)] <- "heather"
moddf$var_names[grepl("reclass17", moddf$var_names)] <- "heathland"
moddf$var_names[grepl("reclass18", moddf$var_names)] <- "meadows"
moddf$var_names[grepl("reclass19", moddf$var_names)] <- "early snowbed"
moddf$var_names[grepl("reclass20", moddf$var_names)] <- "late snowbed"
moddf$var_names <- gsub("norway_pca_klima_axis", "pc", moddf$var_names)
moddf$var_names <- gsub("NORUTreclass", "", moddf$var_names)
moddf <- moddf[,c(6, 1,3,5)]
moddf[,2] <- as.character(round(moddf[,2], 2))
moddf[,3] <- as.character(round(moddf[,3], 2))
moddf[,4] <- as.character(round(moddf[,4], 4))
rownames(moddf) <- NULL
moddf
xtable(moddf)


### 5. Plot theoretical footprint ----
(i=which.min(results$AIC)) #2232
results[i,]

### * private cabins =====
beta = coefficients(mod)[2]
ZoI = results$private_cabin_zoi[i]

feat <- SpatialPointsDataFrame(coords=data.frame(x=100000,y=6700000), data=data.frame(id="a"))
writeVECT(feat, vname="tmp", v.in.ogr_flags=c("o","t","overwrite"), driver="SQLite")
execGRASS("v.to.rast", parameters = list(input="tmp", output="tmp_rast", use="val"), flags="overwrite")
execGRASS("r.mapcalc", parameters = list(expression="tmp_rast = if(isnull(tmp_rast), 0, tmp_rast)"), flags="overwrite")
(scal <- ZoI)
execGRASS("r.mfilter", parameters = list(input="tmp_rast", output="tmp", 
                                         filter=gsub("XXX", scal, "/home/NINA.NO/bram.van.moorter/Mounted/Egenutvikling/12084000_egenutvikling_bram_van_moorter/reindoc_rproj/Data/Env_Data/filters/thresholdXXX.txt")), flags=c("overwrite"))
rast <- raster(readRAST(vname=paste0("tmp@u_bram.van.moorter"), plugin=FALSE))
plot(rast)

# crop
rast <- crop(rast, extent(100000-25000, 100000+25000, 6700000-25000, 6700000+25000))
plot(rast)
min(values(rast))     
exp(min(values(rast)))

# multiply with beta
values(rast) <- exp(values(rast)*beta)
plot(rast)
rast

# footprint
rast2 <- aggregate(rast, 10, mean)
values(rast2) <- values(rast2)^100
rast2
plot(rast2)
sum(1-values(rast2))

# 3D plot
z = raster::as.matrix(rast2)
x = seq(-(extent(rast2)@xmax - extent(rast2)@xmin)/2,(extent(rast2)@xmax - extent(rast2)@xmin)/2,length.out=dim(rast2)[1])
y = seq(-(extent(rast2)@ymax - extent(rast2)@ymin)/2,(extent(rast2)@ymax - extent(rast2)@ymin)/2,length.out=dim(rast2)[2])
nrz <- nrow(z)
ncz <- ncol(z)
persp(x,y,z, theta=45, phi=40, r=2, shade=0.4, axes=TRUE,scale=TRUE, box=T, 
      nticks=4, ticktype="detailed", zlim = c(0.95, 1))

nrz <- nrow(z)
ncz <- ncol(z)
jet.colors <- colorRampPalette( c("red", "light blue") )
nbcol <- 100
color <- jet.colors(nbcol)
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)
persp(x,y,z, theta=-25, phi=30, r=2, shade=0.4, axes=TRUE,scale=TRUE, box=T, 
      nticks=4, ticktype="detailed", zlim = c(0.99999985, 1) , col=color[facetcol])
persp(x,y,z, theta=-25, phi=30, r=2, shade=0.4, axes=TRUE,scale=TRUE, box=F, 
      nticks=4, ticktype="detailed", zlim = c(0.99985, 1) , col=color[facetcol])


### * public cabins =====
beta = coefficients(mod)[1]
ZoI = results$public_cabin_zoi[i]

# compute density
feat <- SpatialPointsDataFrame(coords=data.frame(x=100000,y=6700000), data=data.frame(id="a"))
writeVECT(feat, vname="tmp", v.in.ogr_flags=c("o","t","overwrite"), driver="SQLite")
execGRASS("v.to.rast", parameters = list(input="tmp", output="tmp_rast", use="val"), flags="overwrite")
execGRASS("r.mapcalc", parameters = list(expression="tmp_rast = if(isnull(tmp_rast), 0, tmp_rast)"), flags="overwrite")
scal=(ZoI/2)*0.85 
execGRASS("r.resamp.filter", parameters = list(input="tmp_rast", output=paste0("tmp_", scal), filter="normal,box", radius=c(scal, 3*scal)), flags=c("overwrite"))

rast <- raster(readRAST(vname=paste0("tmp_", scal, "@u_bram.van.moorter"), plugin=FALSE))
plot(rast)

# crop
rast <- crop(rast, extent(100000-25000, 100000+25000, 6700000-25000, 6700000+25000))
plot(rast)
min(values(rast))     
exp(min(values(rast)))

# multiply with beta
values(rast) <- exp(values(rast)*beta)
plot(rast)
rast
sum(values(rast)-1)

# footprint
rast2 <- aggregate(rast, 10, mean)
rast2
plot(rast2)
sum(values(rast2)-1)

# 3D plot
z = raster::as.matrix(rast2)
x = seq(-(extent(rast2)@xmax - extent(rast2)@xmin)/2,(extent(rast2)@xmax - extent(rast2)@xmin)/2,length.out=dim(rast2)[1])
y = seq(-(extent(rast2)@ymax - extent(rast2)@ymin)/2,(extent(rast2)@ymax - extent(rast2)@ymin)/2,length.out=dim(rast2)[2])
nrz <- nrow(z)
ncz <- ncol(z)
persp(x,y,z, theta=45, phi=40, r=2, shade=0.4, axes=TRUE,scale=TRUE, box=T, 
      nticks=4, ticktype="detailed", zlim = c(0.95, 1))

nrz <- nrow(z)
ncz <- ncol(z)
jet.colors <- colorRampPalette( c("red", "light blue") )
nbcol <- 100
color <- jet.colors(nbcol)
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)
persp(x,y,z, theta=-25, phi=30, r=2, shade=0.4, axes=TRUE,scale=TRUE, box=T, 
      nticks=4, ticktype="detailed", zlim = c(0.95, 1) , col=color[facetcol])
persp(x,y,z, theta=-25, phi=30, r=2, shade=0.4, axes=TRUE,scale=TRUE, box=F, 
      nticks=4, ticktype="detailed", zlim = c(0.95, 1) , col=color[facetcol])


### 6. Plot empirical footprint ----
con_params <- list()
con_params$usr = "myusername"
con_params$pwd = "mypassword"
con_params$usr = "bram.van.moorter"
con_params$pwd = "bram"

library(RPostgreSQL)
drv <- dbDriver("PostgreSQL") # driver
con <- dbConnect(drv, dbname="gisdata", host="gisdata-db.nina.no",  
                 port="5432", user=con_params$usr, password=con_params$pwd) # define connection

rs <- dbSendQuery(con, paste0("SELECT points_id, 
                                ST_X(ST_Transform(geom_e33, 25833)) as x33, ST_Y(ST_Transform(geom_e33, 25833)) as y33, * FROM sam_tools.norway_grid_env WHERE wild_reindeer_area='Hardangervidda';"))
grd <- fetch(rs,-1)
dbClearResult(rs)

grd$geom_e33 <- NULL
str(grd)

(i=which.min(results$AIC)) #2232
ZoI_h <- results$public_cabin_zoi[i]
ZoI_pc <- results$private_cabin_zoi[i]

# * private cabins =====
(scal <- ZoI_pc)
execGRASS("r.mfilter", parameters = list(input="private_cabins_rast", output="tmp", 
                                         filter=gsub("XXX", scal, "/home/NINA.NO/bram.van.moorter/Mounted/Egenutvikling/12084000_egenutvikling_bram_van_moorter/reindoc_rproj/Data/Env_Data/filters/thresholdXXX.txt")), flags=c("overwrite"))
dens_private_cabins <- raster(readRAST("tmp", plugin=FALSE))
plot(dens_private_cabins)
plot(areas, add=T)

grd$ZoI_private_cabins <- extract(dens_private_cabins, SpatialPoints(grd[,c("x33", "y33")]))
min(grd$ZoI_private_cabins, na.rm=T)

# * public cabins =====
scal=(ZoI_h/2)*0.85 
execGRASS("r.mapcalc", parameters = list(expression="pub_cabins_summer_high_rast = if(isnull(pub_cabins_summer_high_rast), 0, pub_cabins_summer_high_rast)"), flags="overwrite")
execGRASS("r.resamp.filter", parameters = list(input="pub_cabins_summer_high_rast", output=paste0("pub_cabins_summer_high_V2_", scal), 
                                               filter="normal,box", radius=c(scal, 3*scal)), flags=c("overwrite"))
dens_pub_cabins_summer_high <- raster(readRAST(paste0("pub_cabins_summer_high_V2_", scal), plugin=FALSE))
plot(dens_pub_cabins_summer_high)
plot(areas, add=T)

grd$ZoI_pub_cabins <- extract(dens_pub_cabins_summer_high, SpatialPoints(grd[,c("x33", "y33")]))
min(grd$ZoI_pub_cabins, na.rm=T)

# * prediction =====
grd$NORUTreclass <- ifelse(grd$norut<9, "11forest", ifelse(grd$norut<12, "bog", ifelse(grd$norut<21, "mountain", ifelse(grd$norut<22, "glacier", ifelse(grd$norut<23, "water", ifelse(grd$norut<25,"other", NA))))))
grd$NORUTreclass <- ifelse(grd$NORUTreclass=="mountain", grd$norut, grd$NORUTreclass) 

formula(mod)

d <- grd[,c("ZoI_pub_cabins", "ZoI_private_cabins", "NORUTreclass", "norway_pca_klima_axis1", "norway_pca_klima_axis2", "norway_pca_klima_axis3", "norway_pca_klima_axis4")]
d$use <- rep(0, nrow(d))
d$points_id <- grd$points_id
nrow(d)

nrow(d <- na.omit(d))

mm <- as.data.frame(model.matrix(formula(mod), data=d))

sum(1-exp(mm$ZoI_pub_cabins*coefficients(mod)[1]))
sum(1-exp(mm$ZoI_private_cabins*coefficients(mod)[2]))

head(mm)

dat2 <- na.omit(dat[,c("use", "ZoI_pub_cabins", "ZoI_private_cabins", "NORUTreclass", "norway_pca_klima_axis1", "norway_pca_klima_axis2", "norway_pca_klima_axis3", "norway_pca_klima_axis4")])
tmp <- poly(dat2$norway_pca_klima_axis1, 2)
tmp <- predict(tmp, d$norway_pca_klima_axis1)
mm$"poly(norway_pca_klima_axis1, 2)1" <- tmp[,1]
mm$"poly(norway_pca_klima_axis1, 2)2" <- tmp[,2]
tmp <- poly(dat2$norway_pca_klima_axis2, 2)
tmp <- predict(tmp, d$norway_pca_klima_axis2)
mm$"poly(norway_pca_klima_axis2, 2)1" <- tmp[,1]
mm$"poly(norway_pca_klima_axis2, 2)2" <- tmp[,2]

mm <- mm[,-1]

(coefs <- coefficients(mod))
predvals <- (as.matrix(mm) %*% coefs)
d$linpred <- predvals[,1]
head(d)

grd$linpred <- NA
grd[(grd$points_id %in% d$points_id), c("linpred")] <- d[, c("linpred")]

# Convert to raster
grd[,c("x33", "y33")] <- grd[,c("x33", "y33")]-50
rast <- rasterFromXYZ(grd[,c("x33", "y33", "linpred")])  #Convert first two columns as lon-lat and third as value         
plot(rast)

rast_total <- rast

### no infra
coefs[1:2] <- 0
predvals <- (as.matrix(mm) %*% coefs)
d$linpred <- predvals[,1]
grd$linpred <- NA
grd[(grd$points_id %in% d$points_id), c("linpred")] <- d[, c("linpred")]
grd[,c("x33", "y33")] <- grd[,c("x33", "y33")]-50
rast <- rasterFromXYZ(grd[,c("x33", "y33", "linpred")])  #Convert first two columns as lon-lat and third as value         
plot(rast)
rast_nature <- rast

### tourist cabins only
(coefs <- coefficients(mod))
coefs[c(2:length(coefs))] <- 0
coefs
predvals <- (as.matrix(mm) %*% coefs)
d$linpred <- predvals[,1]
grd$linpred <- NA
grd[(grd$points_id %in% d$points_id), c("linpred")] <- d[, c("linpred")]
grd[,c("x33", "y33")] <- grd[,c("x33", "y33")]-50
rast <- rasterFromXYZ(grd[,c("x33", "y33", "linpred")])  #Convert first two columns as lon-lat and third as value         
plot(rast)
rast_infra1 <- rast

### private cabins only
(coefs <- coefficients(mod))
coefs[c(1, 3:length(coefs))] <- 0
coefs
predvals <- (as.matrix(mm) %*% coefs)
d$linpred <- predvals[,1]
grd$linpred <- NA
grd[(grd$points_id %in% d$points_id), c("linpred")] <- d[, c("linpred")]
grd[,c("x33", "y33")] <- grd[,c("x33", "y33")]-50
rast <- rasterFromXYZ(grd[,c("x33", "y33", "linpred")])  #Convert first two columns as lon-lat and third as value         
plot(rast)
rast_infra2 <- rast

rast_total_exp <- exp(rast_total)
rast_nature_exp <- exp(rast_nature)
rast_infra1_exp <- exp(rast_infra1)
rast_infra2_exp <- exp(rast_infra2)

plot(rast_infra1_exp)
plot(rast_infra2_exp)
plot(rast_total_exp)
plot(rast_nature_exp)

test <- rast_infra1_exp
values(test) <- values(rast_infra1_exp)*values(rast_infra2_exp)*values(rast_nature_exp)
plot(test)

plot(rast_total_exp)

mean(1-values(rast_infra_exp1), na.rm=T)

sum(values(rast_infra1_exp)*values(rast_nature_exp), na.rm=T) / sum(values(rast_nature_exp), na.rm=T)
sum(values(rast_infra2_exp)*values(rast_nature_exp), na.rm=T) / sum(values(rast_nature_exp), na.rm=T)

sum(values(rast_infra1_exp)*values(rast_infra2_exp)*values(rast_nature_exp), na.rm=T) / sum(values(rast_infra2_exp)*values(rast_nature_exp), na.rm=T)
sum(values(rast_infra2_exp)*values(rast_infra1_exp)*values(rast_nature_exp), na.rm=T) / sum(values(rast_infra1_exp)*values(rast_nature_exp), na.rm=T)


sum(1-values(rast_infra1_exp), na.rm=T)


exp(min(grd$ZoI_private_cabins, na.rm=T)*coefficients(mod)[1])
plot(rast_infra_exp)
plot(areas[areas$name_area=="Hardangervidda",], add=T, lwd=2)


### * Plots =====
library(scales)
#install.packages("jcolors")
library(jcolors)

### ** gpslocs ######
plot(areas[areas$name_area=="Hardangervidda",], lwd=2, main="gps locations")
plot(SpatialPoints(dat[dat$use==1,c("x33", "y33")]), add=T, pch=16, col=alpha("black", 0.4), cex=0.5)

### ** tourist cabins #####
rast <- rasterFromXYZ(grd[,c("x33", "y33", "pub_cabins_summer_high_100")])  #Convert first two columns as lon-lat and third as value         
rast <- aggregate(rast, fac=10)
plot(rast)
values(rast) <- ifelse(values(rast)>0, 1, 0)
plot(rast, col=c("white", "black"), main="Tourist cabins")
plot(areas[areas$name_area=="Hardangervidda",], add=T, lwd=2)

### ** private cabins ######
rast <- rasterFromXYZ(grd[,c("x33", "y33", "private_cabins_100")])  #Convert first two columns as lon-lat and third as value         
rast <- aggregate(rast, fac=10)
plot(rast)
values(rast) <- ifelse(values(rast)>0, 1, 0)
plot(rast, col=c("white", "black"), main="Private cabins")
plot(areas[areas$name_area=="Hardangervidda",], add=T, lwd=2)

### ** natural suitability ######
pal11 <- jcolors("pal11")
tmp <- rast_nature_exp
intercept <- max(values(tmp), na.rm=T)
values(tmp) <- values(tmp)/intercept
values(tmp)[1] <- 0
values(tmp)[length(values(tmp))] <- 1
plot(tmp, main="Natural suitability", col=pal11, zlim=c(0,1))

### ** footprint tourist cabins #######
pal12 <- jcolors("pal12")
tmp <- 1-rast_infra1_exp
values(tmp)[1] <- 0
values(tmp)[length(values(tmp))] <- 1
plot(tmp, main="Footprint tourist cabins", col=pal12, zlim=c(0,1))
range(tmp)

### ** footprint private cabins #######
pal12 <- jcolors("pal12")
tmp <- 1-rast_infra2_exp
values(tmp)[1] <- 0
values(tmp)[length(values(tmp))] <- 1
plot(tmp, main="Footprint private cabins", col=pal12, zlim=c(0,1))
range(1-rast_infra2_exp)

### ** Total suitability #######
pal11 <- jcolors("pal11")
tmp <- rast_total_exp
values(tmp) <- values(tmp)/intercept
plot(tmp, main="Total suitability", col=pal11, zlim=c(0,1))

### ** Loss due to tourist cabins ######
pal12 <- jcolors("pal12")
tmp <- rast_nature_exp
values(tmp) <- values(rast_nature_exp)/intercept-values(rast_nature_exp)*values(rast_infra1_exp)/intercept
plot(tmp, main="Loss due to tourist cabins", col=pal12)
plot(tmp, main="Loss due to tourist cabins", col=pal12, zlim=c(0,1))

### ** Loss due to private cabins ########
pal12 <- jcolors("pal12")
tmp <- rast_nature_exp
values(tmp) <- values(rast_nature_exp)/intercept-values(rast_nature_exp)*values(rast_infra2_exp)/intercept
plot(tmp, main="Loss due to private cabins", col=pal12, zlim=c(0,1))


### 8. Some general figures ----
library(ggplot2)
library(reshape2)
library(ggimage)

### Basic shapes ====
dat <- data.frame(dists=seq(0, 8000, length.out=100))
ZoI <-5000

dat$threshold <- ifelse(dat$dists>ZoI, 0, 1)
dat$linear <- ifelse(dat$dists>ZoI, 0, 1-(dat$dists/ZoI))
dat$gaussian <- exp(-0.5 * (dat$dists/((ZoI/2)*0.85))^2)

melted <- melt(dat, id.vars = c('dists'), value.name = "influence")
head(melted)
# rename
names(melted) <- c('distance', 'shape', 'influence')

(plt <- ggplot(data=melted, aes(x=distance, y=influence, shape=factor(shape))) + theme_classic() +
  geom_line(aes(linetype=factor(shape, levels=c("threshold", "linear", "gaussian"))), size=0.75) +
  scale_linetype_manual(name="shape", values=c("solid","dashed", "dotted"))+
  geom_image(x=0, y=0.01, aes(image= "/data/R/Prosjekter/Rein/RSF_output/figs/house_icon.png"))+
  labs(title="", x="Distance (m)", y = "Influence"))

ggsave(plt, file=paste0("/data/R/Prosjekter/Rein/RSF_output/figs/ZoI_shapes.jpg"), 
       width = 15, height = 10, units = "cm", dpi=300)


### Include density ====
dat <- data.frame(lattitude=seq(0, 11000, length.out=1000))
h1 <- abs(dat$lattitude-500)
h2 <- abs(dat$lattitude-4500)
h3 <- abs(dat$lattitude-7000)
h4 <- abs(dat$lattitude-7500)

ZoI <- 2500

dat$threshold_nearest <- apply(cbind(ifelse(h1>ZoI, 0, 1), ifelse(h2>ZoI, 0, 1), 
                                 ifelse(h3>ZoI, 0, 1), ifelse(h4>ZoI, 0, 1)), 1, max)
dat$threshold_cumulative <- apply(cbind(ifelse(h1>ZoI, 0, 1), ifelse(h2>ZoI, 0, 1), 
                                 ifelse(h3>ZoI, 0, 1), ifelse(h4>ZoI, 0, 1)), 1, sum)
dat$linear_nearest <- apply(cbind(ifelse(h1>ZoI, 0, 1-(h1/ZoI)), ifelse(h2>ZoI, 0, 1-(h2/ZoI)), 
                              ifelse(h3>ZoI, 0, 1-(h3/ZoI)), ifelse(h4>ZoI, 0, 1-(h4/ZoI))), 1, max)
dat$linear_cumulative <- apply(cbind(ifelse(h1>ZoI, 0, 1-(h1/ZoI)), ifelse(h2>ZoI, 0, 1-(h2/ZoI)), 
                              ifelse(h3>ZoI, 0, 1-(h3/ZoI)), ifelse(h4>ZoI, 0, 1-(h4/ZoI))), 1, sum)
dat$gaussian_nearest <- apply(cbind(exp(-0.5 * (h1/((ZoI/2)*0.85))^2), exp(-0.5 * (h2/((ZoI/2)*0.85))^2), 
                                exp(-0.5 * (h3/((ZoI/2)*0.85))^2), exp(-0.5 * (h4/((ZoI/2)*0.85))^2)), 1, max)
dat$gaussian_cumulative <- apply(cbind(exp(-0.5 * (h1/((ZoI/2)*0.85))^2), exp(-0.5 * (h2/((ZoI/2)*0.85))^2), 
                                exp(-0.5 * (h3/((ZoI/2)*0.85))^2), exp(-0.5 * (h4/((ZoI/2)*0.85))^2)), 1, sum)

melted <- melt(dat, id.vars = c('lattitude'), value.name = "influence")
melted$shape <- unlist(lapply(as.character(melted$variable), function(x){strsplit(x, split="_")[[1]][1]}))
melted$cum <- unlist(lapply(as.character(melted$variable), function(x){strsplit(x, split="_")[[1]][2]}))
head(melted)

ggplot(data=melted, aes(x=lattitude, y=influence)) + theme_classic() +
  geom_line() +
  labs(title="", x="Distance (m)", y = "Influence")+
  facet_grid(factor(cum, levels=c("nearest", "cumulative"))~factor(shape, levels=c("threshold", "linear", "gaussian")))

plt <- ggplot(data=melted, aes(x=lattitude, y=influence)) + theme_classic() +
    geom_line() +
    labs(title="", x="Lattitude", y = "Influence")+
    geom_image(x=500, y=0.01, aes(image= "/data/R/Prosjekter/Rein/RSF_output/figs/house_icon.png"))+
    geom_image(x=4500, y=0.01, aes(image= "/data/R/Prosjekter/Rein/RSF_output/figs/house_icon.png"))+
    geom_image(x=7000, y=0.01, aes(image= "/data/R/Prosjekter/Rein/RSF_output/figs/house_icon.png"))+
    geom_image(x=7500, y=0.01, aes(image= "/data/R/Prosjekter/Rein/RSF_output/figs/house_icon.png"))+
    facet_grid(factor(cum, levels=c("nearest", "cumulative"))~factor(shape, levels=c("threshold", "linear", "gaussian")))
plt

ggsave(plt, file=paste0("/data/R/Prosjekter/Rein/RSF_output/figs/ZoI_density.jpg"), 
       width = 15, height = 10, units = "cm", dpi=300)


# 2D Gaussian Kernal plot =====

## Generate x and y coordinates as sequences
x = seq(-10,10,0.5)
y = seq(-10,10,0.5)

# An empty matrix z
z = matrix(data=NA, nrow=length(x), ncol=length(x))

### Gaussian kernal generation to fill the z matrix.
sigma = 2.0
mux = 10.0
muy = 5.0
A = 0.7

for(i in 1:length(x))
{
  for(j in 1:length(y))
  {
    
    z[i,j] = A * (1/(2*pi*sigma^2)) * exp( -((x[i]-mux)^2 + (y[j]-muy)^2)/(2*sigma^2)) 
  }
}


mux = 10.0
muy = -5.0
A = 0.7

for(i in 1:length(x))
{
  for(j in 1:length(y))
  {
    
    z[i,j] = z[i,j] + A * (1/(2*pi*sigma^2)) * exp( -((x[i]-mux)^2 + (y[j]-muy)^2)/(2*sigma^2)) 
  }
}
mux = 10.0
muy = 6.0
A = 0.8

for(i in 1:length(x))
{
  for(j in 1:length(y))
  {
    
    z[i,j] = z[i,j] + A * (1/(2*pi*sigma^2)) * exp( -((x[i]-mux)^2 + (y[j]-muy)^2)/(2*sigma^2)) 
  }
}




mux = -0.0
muy = -2.0
A = 1.2

for(i in 1:length(x))
{
  for(j in 1:length(y))
  {
    
    z[i,j] = z[i,j] + A * (1/(2*pi*sigma^2)) * exp( -((x[i]-mux)^2 + (y[j]-muy)^2)/(2*sigma^2)) 
  }
}

image(z)

### Now z is a matrix of dimension length(x) * length(y)

# Plotting surface with persp() function of default "Graphics" library in R.
## (Note: At this point, just give any x,y vector and z matrix of terrain to 
##       get your plot)

nrz <- nrow(z)
ncz <- ncol(z)
jet.colors <- colorRampPalette( c("light blue", "red") )
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
persp(x,y,z, theta=90, phi=15, r=2, shade=0.4, axes=TRUE,scale=TRUE, box=F, 
      nticks=5, ticktype="detailed", zlim = c(0,0.2) , col=color[facetcol])


# APPENDIX: some model testing ----

mod1 <- glm(use ~ lake + norway_pca_klima_axis1 + norway_pca_klima_axis2 + norway_pca_klima_axis3 + norway_pca_klima_axis4, 
            data = dat[dat$herd=="Hardangervidda",], family=binomial(link = "logit"))

coefficients(mod1)
mod <- glm(use ~ lake + norway_pca_klima_axis1 + norway_pca_klima_axis2 + norway_pca_klima_axis3 + norway_pca_klima_axis4, 
           data = dat[dat$herd=="Hardangervidda",], family=binomial(link = "log"))

mod <- glm(use ~ lake + norway_pca_klima_axis1 + norway_pca_klima_axis2 + norway_pca_klima_axis3 + norway_pca_klima_axis4, 
           data = dat[dat$herd=="Hardangervidda",], family=binomial(link = "log"), start=coefficients(mod1))

library(logbin)

mod <- logbin(use ~ lake + norway_pca_klima_axis1 + norway_pca_klima_axis2 + norway_pca_klima_axis3 + norway_pca_klima_axis4, 
              data = dat[dat$herd=="Hardangervidda",])
summary(mod)

library(pbapply)
library(ResourceSelection)
mod_rsf <- rsf(use ~ lake + norway_pca_klima_axis1 + norway_pca_klima_axis2 + norway_pca_klima_axis3 + norway_pca_klima_axis4, 
               m=0, B=0, data = dat[dat$herd=="Hardangervidda",], inits=coefficients(mod1))
mod_rsf <- rsf(use ~ lake + norway_pca_klima_axis1 + norway_pca_klima_axis2 + norway_pca_klima_axis3 + norway_pca_klima_axis4, 
               m=0, B=99, data = dat[dat$herd=="Hardangervidda",], inits=coefficients(mod1))
summary(mod_rsf)
AIC(mod_rsf)
CAIC(mod_rsf)
