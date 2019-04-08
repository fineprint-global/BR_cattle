################################
# calculate footprints
################################
# aggregate function
agg <- function(x){
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)}

# footprint function
product_flows <- function(country, commodity, allocation, precision = 1e-4){
  commodity = paste0(which(regions$ISO==country),"_",commodity)
  if(allocation=="price"){
    # FOOD - products consumed by country
    data <- data.frame(region = rep(regions$Country, each = 120),
                       item = rep(items$Item, 192),
                       type = "food",
                       commodity = L_price[commodity,],
                       Y = rowSums(Y_fabio))
    data$comFP <- data$commodity * data$Y
    data$landFP <- data$comFP * sum(MP_price[,commodity])
    data <- cbind(data,Y_fabio/rowSums(Y_fabio))
    data <- data[data$landFP>precision,] # delete rows with values < 1 m² (1e-4 ha)
    data_price <- data
    
    # NONFOOD - products consumed by country
    data <- data.frame(region = rep(regions_exio$EXIOregion, each = 200),
                       item = rep(items_exio$Item, 49),
                       type = "nonfood",
                       commodity = B_inv_price[commodity,],
                       Y = rowSums(Y_exio))
    data$comFP <- data$commodity * data$Y
    data$landFP <- data$comFP * sum(MP_price[,commodity])
    data <- cbind(data,Y_exio/rowSums(Y_exio))
    data <- data[data$landFP>precision,] # delete rows with values < 1 m² (1e-4 ha)
    data_price <- rbind(data_price,data)
    return(data_price)
  } else if(allocation=="mass"){
    # FOOD - products consumed by country
    data <- data.frame(region = rep(regions$Country, each = 120),
                       item = rep(items$Item, 192),
                       type = "food",
                       commodity = L_mass[commodity,],
                       Y = rowSums(Y_fabio))
    data$comFP <- data$commodity * data$Y
    data$landFP <- data$comFP * sum(MP_mass[,commodity])
    data <- cbind(data,Y_fabio/rowSums(Y_fabio))
    data <- data[data$landFP>precision,] # delete rows with values < 1 m² (1e-4 ha)
    data_mass <- data
    
    # NONFOOD - products consumed by country
    data <- data.frame(region = rep(regions_exio$EXIOregion, each = 200),
                       item = rep(items_exio$Item, 49),
                       type = "nonfood",
                       commodity = B_inv_mass[commodity,],
                       Y = rowSums(Y_exio))
    data$comFP <- data$commodity * data$Y
    data$landFP <- data$comFP * sum(MP_mass[,commodity])
    data <- cbind(data,Y_exio/rowSums(Y_exio))
    data <- data[data$landFP>precision,] # delete rows with values < 1 m² (1e-4 ha)
    data_mass <- rbind(data_mass,data)
    return(data_mass)
  }
}




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
