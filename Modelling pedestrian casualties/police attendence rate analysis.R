library(tidyverse)
library(stats19)
#########loading data###########
Sys.setlocale('LC_ALL','English')
crashes  = get_stats19(year = 2017,type = 'accident', ask = FALSE)
casualties = get_stats19(year = 2017, type = "cas", ask = FALSE)
vehicles = get_stats19(year = 2017, type = "vehicles", ask = FALSE)

jointable = inner_join(crashes, casualties)
jointable = inner_join(jointable,vehicles)

Sys.setlocale('LC_ALL','English')
crashes_2  = get_stats19(year = 2016,type = 'accident', ask = FALSE)
casualties_2 = get_stats19(year = 2016, type = "cas", ask = FALSE)
vehicles_2 = get_stats19(year = 2016, type = "vehicles", ask = FALSE)
join_2016 = inner_join(crashes_2,casualties_2)
join_2016 = inner_join(join_2016,vehicles_2)

crashes_3  = get_stats19(year = 2015,type = 'accident', ask = FALSE)
casualties_3 = get_stats19(year = 2015, type = "cas", ask = FALSE)
vehicles_3 = get_stats19(year = 2015, type = "vehicles", ask = FALSE)
join_2015 = inner_join(crashes_3,casualties_3)
join_2015 = inner_join(join_2015,vehicles_3)

join_3y = full_join(jointable,join_2015)
join_3y = full_join(join_3y, join_2016)
summary(is.na(join_3y))

#####
#get police attdence records
number = 0
test = jointable %>% filter(did_police_officer_attend_scene_of_accident == 1)
number[1] = length(unique(test$accident_index))  
test = join_2016 %>% filter(did_police_officer_attend_scene_of_accident == 1)
number[2] = length(unique(test$accident_index)) 
test = join_2015 %>% filter(did_police_officer_attend_scene_of_accident == 1)
number[3] = length(unique(test$accident_index)) 
number_2 = 0
test = jointable %>% filter(did_police_officer_attend_scene_of_accident == 2)
number_2[1] = length(unique(test$accident_index)) 
test = join_2016 %>% filter(did_police_officer_attend_scene_of_accident == 2)
number_2[2] = length(unique(test$accident_index))
test = join_2015 %>% filter(did_police_officer_attend_scene_of_accident == 2)
number_2[3] = length(unique(test$accident_index)) 
number_3 = 0
test = jointable %>% filter(did_police_officer_attend_scene_of_accident == 3)
number_3[1] = length(unique(test$accident_index)) 
test = join_2016 %>% filter(did_police_officer_attend_scene_of_accident == 3)
number_3[2] = length(unique(test$accident_index))
test = join_2015 %>% filter(did_police_officer_attend_scene_of_accident == 3)
number_3[3] = length(unique(test$accident_index))
sum_acc = 0
sum_acc[1] = length(unique(jointable$accident_index))
sum_acc[2] = length(unique(join_2016$accident_index))
sum_acc[3] = length(unique(join_2015$accident_index))
year = c('2017','2016','2015')
percent = round(number/sum_acc*100,2)

police_attend = data.frame(year = year, police_attended = number, all_accident = sum_acc,percentage = percent)
(1-sum_acc[1]/sum_acc[3])*100
#bar charts
ggplot(data = police_attend, mapping = aes(x = year, y = police_attended, group = 1)) + 
  geom_bar(stat = 'identity',fill = 'steelblue') +
  geom_text(aes(label = police_attended, vjust = -0.3, hjust = 0.5), show.legend = FALSE) 
police_attend_reshape = melt(police_attend)
ggplot(police_attend_reshape,aes(x=year,y=value,fill=variable))+geom_bar(position="dodge",stat="identity") + 
  geom_text(aes(x=year,y=value,label=value, vjust = -0.4),
            position=position_dodge(width=1),show.legend = F)

###
#attendce rate of city
city_attendence = join_3y[!duplicated(join_3y$accident_index),] %>% format_sf() %>%
  dplyr::select(local_authority_district,did_police_officer_attend_scene_of_accident) %>% 
  group_by(local_authority_district) %>%  dplyr::summarise(
    Total = n(),
    police_attended = sum(did_police_officer_attend_scene_of_accident == 1),
    not_attended = sum(did_police_officer_attend_scene_of_accident == 2),
    self_rep = sum(did_police_officer_attend_scene_of_accident == 3))
city_attendence$attendence_rate = round(city_attendence$police_attended/city_attendence$Total*100,2)
city_attendence %>% dplyr::select(local_authority_district, Total, attendence_rate) %>% 
  arrange(desc(Total)) %>% head(5)
#map graph
ggplot(city_attendence, aes(colour = attendence_rate)) +
  geom_sf() 

###
# severity attendence rate
severity_attendence = join_3y[!duplicated(join_3y$accident_index),] %>% 
  dplyr::select(casualty_severity,did_police_officer_attend_scene_of_accident) %>% 
  group_by(casualty_severity) %>%  dplyr::summarise(
    Total = n(),
    police_attended = sum(did_police_officer_attend_scene_of_accident == 1),
    not_attended = sum(did_police_officer_attend_scene_of_accident == 2),
    self_rep = sum(did_police_officer_attend_scene_of_accident == 3))
severity_attendence$attendence_rate = round(severity_attendence$police_attended/severity_attendence$Total*100,2)
severity_attendence %>% dplyr::select(casualty_severity, Total, attendence_rate) %>% 
  arrange(desc(Total)) %>% head(5)

###
# casualty class attendence rate
cc_attendence = join_3y[!duplicated(join_3y$accident_index),] %>% 
  dplyr::select(casualty_class,did_police_officer_attend_scene_of_accident) %>% 
  group_by(casualty_class) %>%  dplyr::summarise(
    Total = n(),
    police_attended = sum(did_police_officer_attend_scene_of_accident == 1),
    not_attended = sum(did_police_officer_attend_scene_of_accident == 2),
    self_rep = sum(did_police_officer_attend_scene_of_accident == 3))
cc_attendence$attendence_rate = round(cc_attendence$police_attended/cc_attendence$Total*100,2)
cc_attendence %>% dplyr::select(casualty_class, Total, attendence_rate) %>% 
  arrange(desc(Total))

###
# week attendence rate
week_atten = join_3y[!duplicated(join_3y$accident_index),] %>%
  dplyr::select(day_of_week,did_police_officer_attend_scene_of_accident) %>% 
  group_by(day_of_week) %>%  dplyr::summarise(
    Total = n(),
    police_attended = sum(did_police_officer_attend_scene_of_accident == 1),
    not_attended = sum(did_police_officer_attend_scene_of_accident == 2),
    self_rep = sum(did_police_officer_attend_scene_of_accident == 3))
week_atten$attendence_rate = round(week_atten$police_attended/week_atten$Total*100,2)
write.csv(week_atten, file = "C:/Users/zeradias/Desktop/Dissertation/RAS50 Generated Table/week_attend.csv")
week_atten$day_of_week <- factor(week_atten$day_of_week,order = TRUE,levels = c("Sunday","Monday","Tuesday",
                                                                                'Wednesday', 'Thursday', 'Friday',
                                                                                'Saturday'))
ggplot(data = week_atten, mapping = aes(x = day_of_week, y = attendence_rate, group = 1)) + 
  geom_line(col = 'Red')
week_atten %>% dplyr::select(day_of_week, Total, attendence_rate) %>% 
  arrange(desc(Total))

###
# hour attendence rate
t_atten = join_3y[!duplicated(join_3y$accident_index),] %>% 
  dplyr::select(time,did_police_officer_attend_scene_of_accident) %>% 
  group_by(time) %>%  dplyr::summarise(
    Total = n(),
    police_attended = sum(did_police_officer_attend_scene_of_accident == 1),
    not_attended = sum(did_police_officer_attend_scene_of_accident == 2),
    self_rep = sum(did_police_officer_attend_scene_of_accident == 3))
t_atten$attendence_rate = round(t_atten$police_attended/t_atten$Total*100,2)

###
# year attendence rate
y_atten = join_3y[!duplicated(join_3y$accident_index),] %>% 
  dplyr::select(date,did_police_officer_attend_scene_of_accident) %>% 
  group_by(date) %>%  dplyr::summarise(
    Total = n(),
    police_attended = sum(did_police_officer_attend_scene_of_accident == 1),
    not_attended = sum(did_police_officer_attend_scene_of_accident == 2),
    self_rep = sum(did_police_officer_attend_scene_of_accident == 3))
y_atten$attendence_rate = round(y_atten$police_attended/y_atten$Total*100,2)