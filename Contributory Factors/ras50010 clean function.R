library(readODS)
library(reshape2)
ras50010_clean = function(address,year){
  col_name = c('Contributory factor attributed to vehicle','Exceeding speed limit','Travelling too fast for conditions')
  crossf_speed = read_ods(address, sheet = year, skip = 7)
  crossf_speed = crossf_speed[,-2]
  crossf_speed = crossf_speed[1:19,]
  crossf_speed[,2]= round(crossf_speed[,2]*100,2)
  crossf_speed[,3]= round(as.numeric(crossf_speed[,3])*100,2)
  names(crossf_speed) = col_name
  crossf_speed = melt(crossf_speed,variable.name="Speed factors",value.name="Percentage")
  Year = rep(2019-year,nrow(crossf_speed))
  crossf_speed$Year = Year
  #data cleaning
  if (year != 2) {
    crossf_speed[which(crossf_speed$`Contributory factor attributed to vehicle` == 
                                    "Driver vision affected by road layout (eg. winding road, hill crest)"),1] = 
      "Road layout (eg. bend, winding road, hill crest)"
    crossf_speed[which(crossf_speed$`Contributory factor attributed to vehicle` == 
                                    "Road layout contributed (eg. bend, hill, narrow carriageway)"),1] = 
      "Road layout (eg. bend, hill, narrow road)"
    crossf_speed[which(crossf_speed$`Contributory factor attributed to vehicle` ==
                                    "Traffic calming (eg. road humps/chicanes)"),1] =
      "Traffic calming (eg. road humps, chicane)"
    crossf_speed[which(crossf_speed$`Contributory factor attributed to vehicle` == 
                                    "Impaired by alcohol"),1] = 
      "Driver/Rider impaired by alcohol"
    crossf_speed[which(crossf_speed$`Contributory factor attributed to vehicle` == 
                                    "Impaired by drugs (illicit or medicinal)"),1] =
      "Driver/Rider impaired by drugs (illicit or medicinal)"
    
    crossf_speed[which(crossf_speed$`Contributory factor attributed to vehicle` == 
                                    "Driver vision affected by road layout (eg. winding road, hill crest)"),1] = 
      "Road layout (eg. bend, winding road, hill crest)"
    crossf_speed[which(crossf_speed$`Contributory factor attributed to vehicle` == 
                                    "Road layout contributed (eg. bend, hill, narrow carriageway)"),1] = 
      "Road layout (eg. bend, hill, narrow road)"
    crossf_speed[which(crossf_speed$`Contributory factor attributed to vehicle` ==
                                    "Traffic calming (eg. road humps/chicanes)"),1] =
      "Traffic calming (eg. road humps, chicane)"
  }
  return(crossf_speed)
}
address = c('C:/Users/zeradias/Desktop/Dissertation/ODS/ras50010.ods')
test = ras50010_clean(address,2)
nrow(test)
