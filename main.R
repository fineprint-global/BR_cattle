################################
# calculate footprints
################################
# rm(list = ls())
library(tidyverse)
library(Matrix)
source("footprint_functions.R")
mount_wu_share()
path <- "../wu_share/WU/Projekte/GRU/07_Publikationen/2020/FABIO_hybrid_deforestation/"

# load classifications
items <- read.csv2("items_fabio.csv")
regions <- read.csv2("regions_fabio.csv")
items_exio <- read.csv2("items_exio.csv")
regions_concordance <- read.csv2("regions_fao-exio.csv", stringsAsFactors = FALSE)
regions_exio <- read.csv2("regions_fao-exio.csv", stringsAsFactors = FALSE) %>% 
  select(3:5) %>% 
  unique() %>% 
  mutate(EXIOcode = as.numeric(EXIOcode)) %>% 
  filter(!is.na(EXIOcode)) %>% 
  arrange(EXIOcode)
nrreg <- nrow(regions)
nrcom <- nrow(items)
index_fabio <- data.frame(region = rep(regions$Country, each = nrcom),
                          item = rep(items$Item, nrreg),
                          type = "food")
index_exio <- data.frame(region = rep(regions_exio$EXIOregion, each = 200),
                         item = rep(items_exio$Item, 49),
                         type = "nonfood")

# define year
year <- 2013

# load data
X <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
E <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))
Y_fabio <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/", year, "_Y.rds"))
L_price <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/", year, "_L_price.rds"))
L_mass <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/", year, "_L_mass.rds"))
L_price[L_price<0] <- 0
L_mass[L_mass<0] <- 0
load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_Y.RData"))
Y_exio <- Y; rm(Y)
B_inv_price <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/hybrid/", year, "_B_inv_price.rds"))
B_inv_mass <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/hybrid/", year, "_B_inv_mass.rds"))

# prepare extension
e <- E$Landuse / X
e[!is.finite(e)] <- 0
e[e<0] <- 0

# prepare final demand
# filter food
Y_fabio <- Y_fabio[,((0:191)*4)+1]
colnames(Y_fabio) <- regions_concordance$EXIOregion[match(regions$Country,regions_concordance$Country)]
Y_fabio <- Y_fabio[,colnames(Y_fabio) %in% regions_exio$EXIOregion]
Y_fabio <- agg(Y_fabio)

# aggregate countries
colnames(Y_exio) <- rep(regions_exio$EXIOregion, each=7)
Y_exio <- agg(Y_exio)

# calculate land use multipliers
MP_price <- e * L_price
MP_mass <- e * L_mass

# define country and commodity
country <- c("IDN", "CHN", "BRA")[3]
commodity <- c("Oil, palm fruit", "Soyabeans", "Cattle")[3]

# run calculations
data_price <- product_flows(country = country, commodity = commodity, allocation = "price")
data_mass <- product_flows(country = country, commodity = commodity, allocation = "mass")

data.table::fwrite(data_price, paste0(path,"results/",year,"_",country,"_",commodity,"_price.csv"), sep = ",")
data.table::fwrite(data_mass, paste0(path,"results/",year,"_",country,"_",commodity,"_mass.csv"), sep = ",")




