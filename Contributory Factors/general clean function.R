#get names of main factors and subfactors
main_factor = c('Road environment contributed',
                'Vehicle defects',
                'Injudicious action',
                'Driver/Rider error or reaction',
                'Impairment or distraction',
                'Behaviour or inexperience',
                'Vision affected by external factors',
                'Pedestrian only (casualty or uninjured)',
                'Special Codes'
)
sub_factor_1 = c('Poor or defective road surface',
                 'Deposit on road (eg. oil, mud, chippings)',
                 'Slippery road (due to weather)',
                 'Inadequate or masked signs or road markings',
                 'Defective traffic signals',
                 'Traffic calming (eg. road humps, chicane)',
                 'Temporary road layout (eg. contraflow)',
                 'Road layout (eg. bend, hill, narrow road)',
                 'Animal or object in carriageway',
                 'Slippery inspection cover or road marking'
)
sub_factor_2 = c('Tyres illegal, defective or under inflated',
                 'Defective lights or indicators',
                 'Defective brakes',
                 'Defective steering or suspension',
                 'Defective or missing mirrors',
                 'Overloaded or poorly loaded vehicle or trailer'
)
sub_factor_3 = c('Disobeyed automatic traffic signal',
                 'Disobeyed \'Give Way\' or \'Stop\' sign or markings',
                 'Disobeyed double white lines',
                 'Disobeyed pedestrian crossing facility',
                 'Illegal turn or direction of travel',
                 'Exceeding speed limit',
                 'Travelling too fast for conditions',
                 'Following too close',
                 'Vehicle travelling along pavement',
                 'Cyclist entering road from pavement'
)
sub_factor_4 = c('Junction overshoot',
                 'Junction restart (moving off at junction)',
                 'Poor turn or manoeuvre',
                 'Failed to signal or misleading signal',
                 'Driver/Rider failed to look properly',
                 'Driver/Rider failed to judge other person¡¯s path or speed',
                 'Too close to cyclist, horse rider or pedestrian',
                 'Sudden braking',
                 'Swerved',
                 'Loss of control'
)
sub_factor_5 = c('Driver/Rider impaired by alcohol',
                 'Driver/Rider impaired by drugs (illicit or medicinal)',
                 'Fatigue',
                 'Uncorrected, defective eyesight',
                 'Driver/Rider illness or disability, mental or physical',
                 'Not displaying lights at night or in poor visibility',
                 'Rider wearing dark clothing',
                 'Driver using mobile phone',
                 'Distraction in vehicle',
                 'Distraction outside vehicle'
)
sub_factor_6 = c('Aggressive driving',
                 'Driver/Rider careless, reckless or in a hurry',
                 'Driver/Rider nervous, uncertain or panic',
                 'Driving too slow for conditions or slow veh (eg tractor)',
                 'Learner or inexperienced driver/rider',
                 'Inexperience of driving on the left',
                 'Unfamiliar with model of vehicle'
)
sub_factor_7 = c('Stationary or parked vehicle(s)',
                 'Vegetation',
                 'Road layout (eg. bend, winding road, hill crest)',
                 'Buildings, road signs, street furniture',
                 'Dazzling headlights',
                 'Dazzling sun',
                 'Rain, sleet, snow, or fog',
                 'Spray from other vehicles',
                 'Visor or windscreen dirty, scratched or frosted etc.',
                 'Vehicle blind spot'
)
sub_factor_8 = c('Crossing road masked by stationary or parked vehicle',
                 'Pedestrian failed to look properly',
                 'Pedestrian failed to judge vehicle¡¯s path or speed',
                 'Pedestrian wrong use of pedestrian crossing facility',
                 'Dangerous action in carriageway (eg. playing)',
                 'Pedestrian impaired by alcohol',
                 'Pedestrian impaired by drugs (illicit or medicinal)',
                 'Pedestrian careless, reckless or in a hurry',
                 'Pedestrian wearing dark clothing at night',
                 'Pedestrian disability or illness, mental or physical'
)
sub_factor_9 = c('Stolen vehicle',
                 'Vehicle in course of crime',
                 'Emergency vehicle on a call',
                 'Vehicle door opened or closed negligently',
                 'Other'
)
sub_factor = cbind(t(sub_factor_1),t(sub_factor_2),t(sub_factor_3),t(sub_factor_4),
                   t(sub_factor_5),t(sub_factor_6),t(sub_factor_7),t(sub_factor_8),t(sub_factor_9)) %>% t()

###########
#ras file cleaning funciton for ras50001,03,05,07,12,13
#year = 2 means 2017, year = 3 means 2016, etc.
#n_or_p = 'percent' or 'number'
library(readODS)
ras_sameset_clean = function(address,year,factors,col_name,n_or_p){
  contri = read_ods(address, sheet = year, skip = 5)
  percent = seq(3,length(contri),3)
  percent = c(1,percent)
  number = seq(2,length(contri),3)
  number = c(1,number)
  number_percent = n_or_p
  if (number_percent == 'number'){
    contri_n = contri[,number]
  }
  if (number_percent == 'percent'){
    contri_n = contri[,percent]
  }
  contri_n = contri_n[-1,]
  names(contri_n) = col_name
  #data clean for ras50007
  contri_n$`Contributory Factors`[contri_n$`Contributory Factors` == 
                                    'Special codes'] = c('Special Codes')
  index = contri_n$`Contributory Factors` %in% factors
  index_contri = contri_n[which(index == TRUE),]
  row.names(index_contri) = c(1:nrow(index_contri))
  for (i in 2:length(index_contri)) {
    index_contri[,i] = round(as.numeric(index_contri[,i]),2)
  }
  Year = rep(2019-year,nrow(index_contri))
  index_contri$Year = Year
  return(index_contri)
}

#function test for ras 50001
address = 'C:/Users/zeradias/Desktop/Dissertation/ODS/ras50001.ods'
col_name = c('Contributory Factors', 'Fatal accidents','Serious accidents',
             'Slight accidents', 'All accidents')
test = ras_same_clean(address,2,main_factor,col_name,'percent')
nrow(test)
test = ras_same_clean(address,2,sub_factor,col_name,'percent')
nrow(test)

#function test for ras 50003
address = c('C:/Users/zeradias/Desktop/Dissertation/ODS/ras50003.ods')
col_name = c('Contributory Factors','Motorways','A roads','B roads','Other roads','All roads')
test = ras_same_clean(address,2,main_factor,col_name,'percent')
nrow(test)
test = ras_same_clean(address,2,sub_factor,col_name,'percent')
nrow(test)

#function test for ras 50005
address = 'C:/Users/zeradias/Desktop/Dissertation/ODS/ras50005.ods'
col_name = c('Contributory Factors', 'Pedal cycle','Motorcycle',
             'Car', 'Bus or Coach','Van/Light goods','HGV','All vehicles')
test = ras_same_clean(address,2,main_factor,col_name,'percent')
nrow(test)
test = ras_same_clean(address,2,sub_factor,col_name,'percent')
nrow(test)

#function test for ras 50007
address = 'C:/Users/zeradias/Desktop/Dissertation/ODS/ras50007.ods'
col_name = c('Contributory Factors', 'Killed','Seriously injured',
             'Slightly injured', 'All casualties')
test = ras_same_clean(address,2,main_factor,col_name,'percent')
nrow(test)
test = ras_same_clean(address,2,sub_factor,col_name,'percent')
nrow(test)

#function test for ras 50012
address = 'C:/Users/zeradias/Desktop/Dissertation/ODS/ras50012.ods'
col_name = c('Contributory Factors', 'North East','North West',
             'Yorkshire and the Humber', 'East Midlands','West Midlands','East of England',
             'South East','London','South West','England','Wales','Scotland','Great Britain')
test = ras_same_clean(address,2,main_factor,col_name,'percent')
nrow(test)
test = ras_same_clean(address,2,sub_factor,col_name,'percent')
nrow(test)

#function test for ras 50013
address = 'C:/Users/zeradias/Desktop/Dissertation/ODS/ras50013.ods'
col_name = c('Contributory Factors', 'North East','North West',
             'Yorkshire and the Humber', 'East Midlands','West Midlands','East of England',
             'South East','London','South West','England','Wales','Scotland','Great Britain')
test = ras_same_clean(address,2,main_factor,col_name,'percent')
nrow(test)
test = ras_same_clean(address,2,sub_factor,col_name,'percent')
nrow(test)