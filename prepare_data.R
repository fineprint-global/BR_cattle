################
# BR cattle
################

agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}

require(Matrix) # Necessary for forked processes
items <- read.csv2("Items.csv")
regions <- read.csv2("Regions.csv")

# Years to calculate hybridised FABIO for
year <- 2013

################
# produce BR-cattle version of FABIO
################
Z <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z.rds"))
X <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
Y <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Y.rds"))

print(paste(year,"read"))

Y[!items$Cattle, ] <- 0
Y[rep(regions$Country, each = 120)!="Brazil", ] <- 0

saveRDS(Y, paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/", year, "_Y.rds"))

# Z <- reduce_matrix(Z,X)
A <- t(t(Z)/X)
A[!is.finite(A)] <- 0
A[A<0] <- 0

A[items$Com.Group=="Live animals" & !items$Cattle, ] <- 0

diag(A)[diag(A)==1] <- 1-1e-10

L <- diag(nrow(A))-A
L <- solve(L, tol = 1.0e-22)

saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/", year, "_L.rds"))


################
# calculations food
################

Y <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/", year, "_Y.rds"))
L <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/", year, "_L.rds"))
E <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))
X <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))

e <- E$Landuse / X
e[!is.finite(e)] <- 0
MP <- e * L


FP <- MP %*% Y
colnames(FP) <- rep(1:4,192)
FP <- agg(FP)


FP <- MP %*% Y
rownames(FP) <- rep(regions$Country, each = 120)
colnames(FP) <- colnames(Y)
library(tidyverse)
FP_countries <- FP %>% 
  as_tibble(rownames=NA) %>% 
  tibble::rownames_to_column(var = "country") %>% 
  group_by(country) %>% 
  summarise_all(funs(sum))
  # transmute(country = country, sum = rowSums(select(., -country)))


FP <- MP %*% Y
colnames(FP) <- rep(regions$Country, each = 4)
FP <- as.data.frame(colSums(agg(FP)))


################
# calculations non-food
################

regions <- read.csv2("Regions.csv")
regions_exio_fao <- read.csv2("Regions_FAO-EXIO.csv", stringsAsFactors = FALSE)
regions_exio <- unique(regions_exio_fao[,3:5])
regions_exio$EXIOcode <- as.numeric(regions_exio$EXIOcode)
regions_exio <- regions_exio[order(regions_exio$EXIOcode)[1:49],]

X_fabio <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
E <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))
A_inv <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/", year, "_L.rds"))
load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_Y.RData"))
if(year<1995){
  load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/1995_L.RData"))
  load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/1995_x.RData"))
} else {
  load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_x.RData"))
  load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/", year, "_L.RData"))
}
D_inv <- L
rm(L); gc()

B <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/hybrid/",year,"_B_120.rds"))
B <- t(t(B)/x)
B[!is.finite(B)] <- 0
B[B<0] <- 0

# delete non-BR & non-cattle inputs
B[!items$Cattle, ] <- 0
B[rep(regions$Country, each = 120)!="Brazil", ] <- 0

B <- -B
B_inv <- -A_inv %*% B %*% D_inv

e <- E$Landuse / X_fabio
e[!is.finite(e)] <- 0
MP_B <- e * B_inv


# country by country
FP_B <- MP_B %*% Y
colnames(FP_B) <- rep(regions_exio$EXIOregion,each = 7)
FP_B <- t(agg(FP_B))
colnames(FP_B) <- rep(regions$Country, each = 120)
FP_B <- t(agg(FP_B))


# by product
FP_B <- MP_B %*% diag(rowSums(Y))
colnames(FP_B) <- rep(1:200, 49)
FP_B <- t(agg(FP_B))
FP_B <- as.data.frame(rowSums(FP_B))


