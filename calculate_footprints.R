################################
# calculations food
################################
# rm(list = ls())
library(tidyverse)
mount_wu_share()
path <- "../wu_share/WU/Projekte/GRU/01_Projekte/0651_MF-GLOBE/02_Activities & Work packages/BR_cattle/"

agg <- function(x){
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)}

items <- read.csv2("items_fabio.csv") %>% 
  filter(X120)
regions <- read.csv2("regions_fabio.csv")
items_exio <- read.csv2("items_exio.csv")
regions_exio <- read.csv2("regions_fao-exio.csv", stringsAsFactors = FALSE) %>% 
  select(3:5) %>% 
  unique() %>% 
  mutate(EXIOcode = as.numeric(EXIOcode)) %>% 
  filter(!is.na(EXIOcode)) %>% 
  arrange(EXIOcode)

year <- 2013

# load data
X <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_X.rds"))
E <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_E.rds"))
Y_fabio <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/", year, "_Y.rds"))
L_price <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/", year, "_L_price.rds"))
L_mass <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/", year, "_L_mass.rds"))
load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_Y.RData"))
Y_exio <- Y; rm(Y)
B_inv_price <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/", year, "_B_inv_price.rds"))
B_inv_mass <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/cattle/", year, "_B_inv_mass.rds"))

# prepare extension
e <- E$Landuse / X
e[!is.finite(e)] <- 0

# prepare final demand
# filter food
Y_fabio <- Y_fabio[,((0:191)*4)+1]
colnames(Y_fabio) <- regions$Country
# aggregate countries
colnames(Y_exio) <- rep(regions_exio$EXIOregion, each=7)
Y_exio <- agg(Y_exio)


#-------------------------------------
# calculate price-based footprints
#-------------------------------------
MP_price <- e * L_price

# FOOD - products consumed by country
data <- data.frame(region = rep(regions$Country, each = 120),
                   item = rep(items$Item, 192),
                   type = "food",
                   BR_cattle = L_price["17_Cattle, Buffaloes",],
                   Y = rowSums(Y_fabio))
data$cattleFP <- data$BR_cattle * data$Y
data$landFP <- data$cattleFP * sum(MP_price[,"17_Cattle, Buffaloes"])
# data <- cbind(data,Y_fabio/rowSums(Y_fabio))
data <- data[data$landFP>1e-4,] # delete rows with values < 1 m² (1e-4 ha)
data_price <- data

# NONFOOD - products consumed by country
data <- data.frame(region = rep(regions_exio$EXIOregion, each = 200),
                   item = rep(items_exio$Item, 49),
                   type = "nonfood",
                   BR_cattle = B_inv_price["17_Cattle, Buffaloes",],
                   Y = rowSums(Y_exio))
data$cattleFP <- data$BR_cattle * data$Y
data$landFP <- data$cattleFP * sum(MP_price[,"17_Cattle, Buffaloes"])
# data <- cbind(data,Y_exio/rowSums(Y_exio))
data <- data[data$landFP>1e-4,] # delete rows with values < 1 m² (1e-4 ha)
data_price <- rbind(data_price,data)

#-------------------------------------
# calculate mass-based footprints
#-------------------------------------
MP_mass <- e * L_mass

# FOOD - products consumed by country
data <- data.frame(region = rep(regions$Country, each = 120),
                   item = rep(items$Item, 192),
                   type = "food",
                   BR_cattle = L_mass["17_Cattle, Buffaloes",],
                   Y = rowSums(Y_fabio))
data$cattleFP <- data$BR_cattle * data$Y
data$landFP <- data$cattleFP * sum(MP_mass[,"17_Cattle, Buffaloes"])
# data <- cbind(data,Y_fabio/rowSums(Y_fabio))
data <- data[data$landFP>1e-4,] # delete rows with values < 1 m² (1e-4 ha)
data_mass <- data

# NONFOOD - products consumed by country
data <- data.frame(region = rep(regions_exio$EXIOregion, each = 200),
                   item = rep(items_exio$Item, 49),
                   type = "nonfood",
                   BR_cattle = B_inv_mass["17_Cattle, Buffaloes",],
                   Y = rowSums(Y_exio))
data$cattleFP <- data$BR_cattle * data$Y
data$landFP <- data$cattleFP * sum(MP_mass[,"17_Cattle, Buffaloes"])
# data <- cbind(data,Y_exio/rowSums(Y_exio))
data <- data[data$landFP>1e-4,] # delete rows with values < 1 m² (1e-4 ha)
data_mass <- rbind(data_mass,data)

sum(data_mass$landFP[data_mass$type=="nonfood"]) / sum(data_mass$landFP[data_mass$type=="food"])
sum(data_price$landFP[data_price$type=="nonfood"]) / sum(data_price$landFP[data_price$type=="food"])


# # country by country
# FP <- MP %*% Y
# colnames(FP) <- rep(regions$Country)
# FP <- t(agg(FP))
# colnames(FP) <- rep(regions$Country, each = 120)
# FP <- t(agg(FP))
# data.table::fwrite(data.table::data.table(FP, keep.rownames = TRUE), 
#                    paste0(path,"FP_",year,"_food_country_by_country.csv"), sep = ";")
# 
# 
# # products consumed by country
# Ybra <- Y[rep(regions$Country, each = 120)=="Brazil",]
# MPbra <- colSums(MP[,rep(regions$Country, each = 120)=="Brazil"])
# FP <- MPbra * Ybra
# FP <- FP[rowSums(FP)>0,]
# data.table::fwrite(data.table::data.table(t(FP), keep.rownames = TRUE), 
#                    paste0(path,"FP_",year,"_food_product_by_country.csv"), sep = ";")
# 
# 
# 
# FP <- MP %*% Y
# rownames(FP) <- rep(regions$Country, each = 120)
# colnames(FP) <- colnames(Y)
# library(tidyverse)
# FP_countries <- FP %>% 
#   as_tibble(rownames=NA) %>% 
#   tibble::rownames_to_column(var = "country") %>% 
#   group_by(country) %>% 
#   summarise_all(funs(sum))
# # transmute(country = country, sum = rowSums(select(., -country)))
# 
# 
