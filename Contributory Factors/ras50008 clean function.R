library(readODS)
ras50008_clean = function(address,year,n_or_p){
  col_name = c('Speed Factor','Killed','Serious','Slight','Total')
  contri = read_ods(address, sheet = year, skip = 7)
  contri = contri[1:3,]
  contri = contri[,-2]
  percent = seq(3,length(contri),3)
  percent = c(1,percent)
  number = seq(2,length(contri),3)
  number = c(1,number)
  number_percet = n_or_p
  if (number_percet == 'number'){
    contri_n = contri[,number]
  }
  if (number_percet == 'percent'){
    contri_n = contri[,percent]
  }
  contri_n = contri_n[-1,]
  
  for (i in 2:length(contri_n)) {
    contri_n[,i] = round(as.numeric(contri_n[,i]),2)
  }
  names(contri_n) = col_name
  contri_n$`Speed Factor`[contri_n$`Speed Factor` == 
                            'Travelling too fast for conditions2'] = c('Travelling too fast for conditions')
  Year = rep(2019-year,nrow(contri_n))
  contri_n$Year = Year
  return(contri_n)
}
address = c('C:/Users/zeradias/Desktop/Dissertation/ODS/ras50008.ods')
test = ras50008_clean(address,2,'percent')
nrow(test)
