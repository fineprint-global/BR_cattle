################################
# calculate B inverse
################################

regions <- read.csv2("Regions.csv")

year <- 2013

X_fabio <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_X.rds"))
if(year<1995){
  load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/1995_L.RData"))
  load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/1995_x.RData"))
} else {
  load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_x.RData"))
  load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/", year, "_L.RData"))
}
D_inv <- L
rm(L); gc()

B <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/hybrid/",year,"_B.rds"))
B <- t(t(B)/x)
B[!is.finite(B)] <- 0
B[B<0] <- 0

# delete non-BR & non-cattle inputs
B[!items$Cattle, ] <- 0
B[rep(regions$Country, each = 120)!="Brazil", ] <- 0
# leontief matrix = identity matrix - technology matrix = 0-B
B <- -B


A_inv <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/", year, "_L_price.rds"))
B_inv <- -A_inv %*% B %*% D_inv
saveRDS(B_inv, paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/",year,"_B_inv_price.rds"))


A_inv <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/", year, "_L_mass.rds"))
B_inv <- -A_inv %*% B %*% D_inv
saveRDS(B_inv, paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/",year,"_B_inv_mass.rds"))