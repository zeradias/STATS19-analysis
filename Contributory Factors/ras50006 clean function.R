library(readODS)
#n_or_p = 'percent' or 'number'
#vehicles_or_pedestrain = 'Vehicles' or 'Pedestrain Casualities'
ras50006_clean = function(address,year,n_or_p,vehicles_or_pedestrain){
  col_name = c('Factor with lower code','Factor with higher code')
  contri = read_ods(address, sheet = year, skip = 6)
  veh_ped = vehicles_or_pedestrain
  if (veh_ped == 'Vehicles'){
    vehicles_row = c(1:21)
    contri = contri[vehicles_row,]
  }
  if (veh_ped == 'Pedestrain Casualities'){
    pedestrain_row = c(23:28)
    contri = contri[pedestrain_row,]
  }
  number_percet = n_or_p
  if (number_percet == 'number'){
    contri = contri[,-4]
    col_name = c(col_name,'Number')
  }
  if (number_percet == 'percent'){
    contri = contri[,-3]
    col_name = c(col_name,'Percentage')
  }
  contri = contri[-1,]
  row.names(contri) = c(1:nrow(contri))
  contri[,3] = round(as.numeric(contri[,3]),2)
  names(contri) = col_name
  Year = rep(2019-year,nrow(contri))
  contri$Year = Year
  v_or_p = rep(vehicles_or_pedestrain,nrow(contri))
  contri$vehicles_or_pedestrain = v_or_p
  return(contri)
}
address = c('C:/Users/zeradias/Desktop/Dissertation/ODS/ras50006.ods')
test = ras50006_clean(address,2,'percent','Vehicles')