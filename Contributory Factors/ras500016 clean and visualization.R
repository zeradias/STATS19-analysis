library(readODS)
library(geojsonsf)
library(tmap)
library(tidyverse)
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
ras50016_mf_percentage = function(address,year,regions_poly){
  contri = read_ods(address, sheet = year, skip = 5)
  contri = contri[1:217,]
  geo_main_factor = rep(NA,217) %>% as.data.frame()
  geo_main_factor[,1] = apply(contri[,sub_factor_1],1,sum)
  geo_main_factor[,2] = apply(contri[,sub_factor_2],1,sum)
  geo_main_factor[,3] = apply(contri[,sub_factor_3],1,sum)
  geo_main_factor[,4] = apply(contri[,sub_factor_4],1,sum)
  geo_main_factor[,5] = apply(contri[,sub_factor_5],1,sum)
  geo_main_factor[,6] = apply(contri[,sub_factor_6],1,sum)
  geo_main_factor[,7] = apply(contri[,sub_factor_7],1,sum)
  geo_main_factor[,8] = apply(contri[,sub_factor_8],1,sum)
  geo_main_factor[,9] = apply(contri[,sub_factor_9],1,sum)
  names(geo_main_factor) = main_factor
  main_contri = cbind(contri[,1:2],geo_main_factor)
  region_no_geo = st_drop_geometry(regions_poly)
  region_names = regions_poly$EER13NM
  region_names[which(region_names == 'Eastern')] = 'East of England'
  index = main_contri$`Region/Local Authority1` %in% region_names
  main_contri = main_contri[which(index == TRUE),]
  per_contri = main_contri
  for (j in 3:length(main_contri)) {
    for (i in 1:nrow(main_contri)) {
      per_contri[i,j] = round(main_contri[i,j]/sum(main_contri[i,3:length(main_contri)])*100,2)
    }
  }
  Year = rep(2019-year,nrow(per_contri))
  per_contri$Year = Year  
  return(per_contri)
}
#get 2015 - 2017 data from ras50016
address = 'C:/Users/zeradias/Desktop/Dissertation/ODS/ras50016.ods'
regions_poly = geojsonsf::geojson_sf(
  "https://raw.githubusercontent.com/martinjc/UK-GeoJSON/master/json/electoral/gb/eer.json")
per_contri_2017 = ras50016_mf_percentage(address,2,regions_poly)
per_contri_2016 = ras50016_mf_percentage(address,3,regions_poly)
per_contri_2015 = ras50016_mf_percentage(address,4,regions_poly)
per_contri_3y = per_contri_2017[,-12]
per_contri_3y[,3:11] = round((per_contri_2017[,3:11]+per_contri_2016[,3:11]+per_contri_2015[,3:11])/3,2)

#data visualization
per_contri_3y_sf = st_as_sf(per_contri_3y, st_geometry(regions_poly))
tmap_mode('view')
tm_shape(per_contri_3y_sf) + tm_polygons(main_factor[1], n=5,palette="YlGn")
tm_shape(per_contri_3y_sf) + tm_polygons(main_factor[2], n=5,palette="YlGn")
tm_shape(per_contri_3y_sf) + tm_polygons(main_factor[3], n=5,palette="YlGn")
tm_shape(per_contri_3y_sf) + tm_polygons(main_factor[4], n=5,palette="YlGn")
tm_shape(per_contri_3y_sf) + tm_polygons(main_factor[5], n=5,palette="YlGn")
tm_shape(per_contri_3y_sf) + tm_polygons(main_factor[6], n=11,palette="YlGn")
tm_shape(per_contri_3y_sf) + tm_polygons(main_factor[7], n=5,palette="YlGn")
tm_shape(per_contri_3y_sf) + tm_polygons(main_factor[8], n=5,palette="YlGn")
tm_shape(per_contri_3y_sf) + tm_polygons(main_factor[9], n=5,palette="YlGn")