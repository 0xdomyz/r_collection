library(ggmap)

getGeoDetails <- function(address){
    geo_reply = geocode(address, output = 'all', messaging = TRUE,
        override_limit=TRUE)
    answer <- data.frame(
        lat = NA, long = NA, accuracy = NA,
        formatted_address = NA, address = NA, address_type = NA,
        status = NA
    )
    answer$status <- geo_reply$status

    if (geo_reply$status != "OK"){
        return(answer)
    }
    answer$lat <- geo_reply$results[[1]]$geometry$location$lat
    answer$long <- geo_reply$results[[1]]$geometry$location$lng
    if (length(geo_reply$results[[1]]$types) > 0){
        answer$accuracy <- geo_reply$results[[1]]$types[[1]]
    }
    answer$address_type <- paste(geo_reply$results[[1]]$types, collapse = ',')
    answer$formatted_address <- geo_reply$results[[1]]$formatted_address

    return(answer)
}

dt$gg_ma_lat = NA
dt$gg_ma_long = NA
dt$gg_ma_accuracy = NA
dt$gg_ma_format = NA
dt$gg_ma_type = NA

for (i in seq(1:nrow(dt))) {
    if(is.na(dt[i,'strt_name']) == F){
        answer = getGeoDetails(dt$address_concat[i])
        dt$gg_ma_lat = answer$lat
        dt$gg_ma_long = answer$long
        dt$gg_ma_accuracy = answer$accuracy
        dt$gg_ma_format = answer$formatted_address
        dt$gg_ma_type = answer$address_type
    }
}
