################
# aggregate
################
agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}

items <- read.csv2("Items.csv")
regions <- read.csv2("Regions.csv")

# Years to calculate hybridised FABIO for
year <- 2013

require(Matrix) # Necessary for forked processes

Z <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z.rds"))
X <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
Y <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Y.rds"))
E <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))

colnames(Z) <- rep(regions$Country, each = 120)
colnames(Z)[colnames(Z)!="Brazil"] <- "ROW"
colnames(Z) <- paste0(colnames(Z), "_", rep(items$Item,192))
Z <- t(agg(Z))
colnames(Z) <- rep(regions$Country, each = 120)
colnames(Z)[colnames(Z)!="Brazil"] <- "ROW"
colnames(Z) <- paste0(colnames(Z), "_", rep(items$Item,192))
Z <- t(agg(Z))

X <- t(as.matrix(X))
colnames(X) <- rep(regions$Country, each = 120)
colnames(X)[colnames(X)!="Brazil"] <- "ROW"
colnames(X) <- paste0(colnames(X), "_", rep(items$Item,192))
X <- as.vector(agg(X))

colnames(Y) <- rep(regions$Country, each = 4)
colnames(Y)[colnames(Y)!="Brazil"] <- "ROW"
colnames(Y) <- paste0(colnames(Y), "_", rep(1:4,192))
Y <- t(agg(Y))
colnames(Y) <- rep(regions$Country, each = 120)
colnames(Y)[colnames(Y)!="Brazil"] <- "ROW"
colnames(Y) <- paste0(colnames(Y), "_", rep(items$Item,192))
Y <- t(agg(Y))

E <- t(as.matrix(E[,c("Landuse","Biomass")]))
colnames(E) <- rep(regions$Country, each = 120)
colnames(E)[colnames(E)!="Brazil"] <- "ROW"
colnames(E) <- paste0(colnames(E), "_", rep(items$Item,192))
E <- agg(E)

# write.csv2(Y, paste0(year, "_Y.csv"))
# write.csv2(Z, paste0(year, "_Z.csv"))
# write.csv2(X, paste0(year, "_X.csv"))
# write.csv2(E, paste0(year, "_E.csv"))


Y[!items$Cattle, ] <- 0
Y[1:120, ] <- 0

A <- t(t(Z)/X)
A[!is.finite(A)] <- 0
A[A<0] <- 0

A[items$Com.Group=="Live animals" & !items$Cattle, ] <- 0

L <- diag(nrow(A))-A
L <- solve(L, tol = 1.0e-22)

MP <- E[1,] / X
MP[!is.finite(MP)] <- 0
MP <- MP * L
FP <- MP %*% Y
rownames(FP) <- rep(regions$Country, each = 120)
colnames(FP) <- colnames(Y)

sum(FP)

library(tidyverse)

FP_countries <- FP %>% 
  as_tibble(rownames=NA) %>% 
  tibble::rownames_to_column(var = "country") %>% 
  group_by(country) %>% 
  summarise_all(funs(sum)) %>% 
  transmute(country = country, sum = rowSums(select(., -country)))

FP_countries$country


