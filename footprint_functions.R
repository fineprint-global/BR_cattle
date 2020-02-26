################################
# calculate footprints
################################
# aggregate function
agg <- function(x){
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)}

# footprint function
product_flows <- function(country, commodity, allocation, precision = 1e-4){
  # commodity = paste0(which(regions$ISO==country),"_",commodity)
  com_id <- index$ISO==country & index$item==commodity
  if(allocation=="price"){
    # FOOD - products consumed by country
    FP <- L_price[com_id,] * Y_fabio * sum(MP_price[,com_id])
    FP <- cbind(index_fabio[rowSums(FP)>precision,],FP[rowSums(FP)>precision,])
    FP_price <- FP
    
    # NONFOOD - products consumed by country
    FP <- B_inv_price[com_id,] * Y_exio * sum(MP_price[,com_id])
    FP <- cbind(index_exio[rowSums(FP)>precision,],FP[rowSums(FP)>precision,])
    FP_price <- rbind(FP_price,FP)
    return(FP_price)
  } else if(allocation=="mass"){
    # FOOD - products consumed by country
    FP <- L_mass[com_id,] * Y_fabio * sum(MP_mass[,com_id])
    FP <- cbind(index_fabio[rowSums(FP)>precision,],FP[rowSums(FP)>precision,])
    FP_mass <- FP
    
    # NONFOOD - products consumed by country
    FP <- B_inv_mass[com_id,] * Y_exio * sum(MP_mass[,com_id])
    FP <- cbind(index_exio[rowSums(FP)>precision,],FP[rowSums(FP)>precision,])
    FP_mass <- rbind(FP_mass,FP)
    return(FP_mass)
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
