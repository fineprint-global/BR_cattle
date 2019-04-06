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
items <- items[items$X120,]
regions <- read.csv2("Regions.csv")

# Years to calculate hybridised FABIO for
year <- 2013

################
# produce BR-cattle version of FABIO
################
Y <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_Y.rds"))
Y[!items$Cattle, ] <- 0
Y[rep(regions$Country, each = 120)!="Brazil", ] <- 0
saveRDS(Y, paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/", year, "_Y.rds"))

# price allocation
X <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_X.rds"))
Z <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_Z_price.rds"))
A <- t(t(Z)/X)
A[!is.finite(A)] <- 0
A[A<0] <- 0
A[items$Com.Group=="Live animals" & !items$Cattle, ] <- 0
diag(A)[diag(A)==1] <- 1-1e-10
L <- diag(nrow(A))-A
L <- solve(L, tol = 1.0e-22)
saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/", year, "_L_price.rds"))

# mass allocation
Z <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_Z_mass.rds"))
A <- t(t(Z)/X)
A[!is.finite(A)] <- 0
A[A<0] <- 0
A[items$Com.Group=="Live animals" & !items$Cattle, ] <- 0
diag(A)[diag(A)==1] <- 1-1e-10
L <- diag(nrow(A))-A
L <- solve(L, tol = 1.0e-22)
saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/", year, "_L_mass.rds"))

