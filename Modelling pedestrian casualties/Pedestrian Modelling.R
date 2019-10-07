library(data.table)
library('randomForest')
library(arulesViz)


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

#get pedestrian casualties data
pedestrian = join_3y %>% filter(casualty_type == 'Pedestrian' & casualty_class == 'Pedestrian')
pedestrian_analysis = pedestrian %>% dplyr::select(date,day_of_week, time, longitude, latitude, casualty_severity,
                                                   casualty_imd_decile, pedestrian_crossing_human_control,
                                                   pedestrian_location,pedestrian_crossing_physical_facilities,
                                                   pedestrian_movement,age_band_of_casualty,
                                                   vehicle_manoeuvre,first_point_of_impact,weather_conditions,
                                                   road_surface_conditions,light_conditions, sex_of_casualty) %>% na.omit()
#omit missing data
test = pedestrian_analysis %>% 
  filter(pedestrian_crossing_human_control != 'Data missing or out of range' & 
           pedestrian_location != 'Data missing or out of range' &
           pedestrian_crossing_physical_facilities != 'Data missing or out of range' &
           pedestrian_movement != 'Data missing or out of range' &
           age_band_of_casualty != 'Data missing or out of range' &
           weather_conditions != 'Unknown' &
           road_surface_conditions != 'Data missing or out of range'  &
           sex_of_casualty != 'Data missing or out of range') %>%
  dplyr::select(casualty_severity, pedestrian_location, 
                sex_of_casualty,
                pedestrian_crossing_physical_facilities,
                pedestrian_movement,age_band_of_casualty,
                light_conditions, 
                road_surface_conditions) %>% 
  as.data.frame() 


##########################################
############KSI random forest############
##########################################
test$casualty_severity[which(test$casualty_severity == 'Fatal')] = c('KSI')
test$casualty_severity[which(test$casualty_severity == 'Serious')] = c('KSI')
n_fatal = test %>% filter(casualty_severity == 'KSI') %>% nrow()
set.seed(100)
slight_data = test %>% filter(casualty_severity == 'Slight')
slight_idx = sample(nrow(slight_data), 1*n_fatal*0.5) #proportion of 'Slight' sample
t_slight_data = slight_data[slight_idx,]
fatal_data = test %>% filter(casualty_severity == 'KSI')
fatal_idx = sample(nrow(fatal_data), n_fatal*0.5) 
t_fatal_data = fatal_data[fatal_idx,]
train_data = full_join(t_slight_data,t_fatal_data)


v_slight_data = slight_data[-slight_idx,]
v_fatal_data = fatal_data[-fatal_idx,]
validation_data = full_join(v_slight_data,v_fatal_data)
train_data$isTest = rep(0,nrow(train_data))
validation_data$isTest = rep(1,nrow(validation_data))
train_fac=train_data %>% mutate_if(is.character, as.factor)
test_fac=validation_data %>% mutate_if(is.character, as.factor)
totalData <- rbind(train_fac, test_fac)
for (f in 1:length(names(totalData))) {
  levels(train_fac[, f]) <- levels(test_fac[, f])
}
test.new <- totalData[totalData$isTest==1,]
train.new <- totalData[totalData$isTest==0,]
summary(test.new$casualty_severity)
summary(train.new$casualty_severity)


current_model = randomForest(casualty_severity ~ ., data=train.new[,-length(train.new)])
pred = predict(current_model, newdata = test.new[,-length(test.new)], "class")
accuracy(current_model,test.new[,-length(test.new)])
plot(pred)
current_model
importance(current_model,type = 2)
##################1:2 K/S########################
n_fatal = test %>% filter(casualty_severity == 'KSI') %>% nrow()
set.seed(100)
slight_data = test %>% filter(casualty_severity == 'Slight')
slight_idx = sample(nrow(slight_data), 2*n_fatal*0.5) #proportion of 'Slight' sample
t_slight_data = slight_data[slight_idx,]
fatal_data = test %>% filter(casualty_severity == 'KSI')
fatal_idx = sample(nrow(fatal_data), n_fatal*0.5) 
t_fatal_data = fatal_data[fatal_idx,]
train_data = full_join(t_slight_data,t_fatal_data)


v_slight_data = slight_data[-slight_idx,]
v_fatal_data = fatal_data[-fatal_idx,]
validation_data = full_join(v_slight_data,v_fatal_data)
train_data$isTest = rep(0,nrow(train_data))
validation_data$isTest = rep(1,nrow(validation_data))
train_fac=train_data %>% mutate_if(is.character, as.factor)
test_fac=validation_data %>% mutate_if(is.character, as.factor)
totalData <- rbind(train_fac, test_fac)
for (f in 1:length(names(totalData))) {
  levels(train_fac[, f]) <- levels(test_fac[, f])
}
test.new <- totalData[totalData$isTest==1,]
train.new <- totalData[totalData$isTest==0,]
summary(test.new$casualty_severity)
summary(train.new$casualty_severity)

current_model = randomForest(casualty_severity ~ ., data=train.new[,-length(train.new)])
pred = predict(current_model, newdata = test.new[,-length(test.new)], "class")
accuracy(current_model,test.new[,-length(test.new)])
plot(pred)
current_model

##############################2:1 S/K################################

n_fatal = test %>% filter(casualty_severity == 'KSI') %>% nrow()
set.seed(100)
slight_data = test %>% filter(casualty_severity == 'Slight')
slight_idx = sample(nrow(slight_data), 0.5*n_fatal*0.5) #proportion of 'Slight' sample
t_slight_data = slight_data[slight_idx,]
fatal_data = test %>% filter(casualty_severity == 'KSI')
fatal_idx = sample(nrow(fatal_data), n_fatal*0.5)
t_fatal_data = fatal_data[fatal_idx,]
train_data = full_join(t_slight_data,t_fatal_data)


v_slight_data = slight_data[-slight_idx,]
v_fatal_data = fatal_data[-fatal_idx,]
validation_data = full_join(v_slight_data,v_fatal_data)
train_data$isTest = rep(0,nrow(train_data))
validation_data$isTest = rep(1,nrow(validation_data))
train_fac=train_data %>% mutate_if(is.character, as.factor)
test_fac=validation_data %>% mutate_if(is.character, as.factor)
totalData <- rbind(train_fac, test_fac)
for (f in 1:length(names(totalData))) {
  levels(train_fac[, f]) <- levels(test_fac[, f])
}
test.new <- totalData[totalData$isTest==1,]
train.new <- totalData[totalData$isTest==0,]
summary(test.new$casualty_severity)
summary(train.new$casualty_severity)

current_model = randomForest(casualty_severity ~ ., data=train.new[,-length(train.new)])
pred = predict(current_model, newdata = test.new[,-length(test.new)], "class")
accuracy(current_model,test.new[,-length(test.new)])
plot(pred)
current_model

##########1:1 for all######
n_fatal = test %>% nrow()
train_idx = sample(nrow(test), 1*n_fatal*0.5) #proportion of sample
train_data = test[train_idx,]
validation_data = test[-train_idx,]
train_data$isTest = rep(0,nrow(train_data))
validation_data$isTest = rep(1,nrow(validation_data))
train_fac=train_data %>% mutate_if(is.character, as.factor)
test_fac=validation_data %>% mutate_if(is.character, as.factor)
totalData <- rbind(train_fac, test_fac)
for (f in 1:length(names(totalData))) {
  levels(train_fac[, f]) <- levels(test_fac[, f])
}
test.new <- totalData[totalData$isTest==1,]
train.new <- totalData[totalData$isTest==0,]
summary(test.new$casualty_severity)
summary(train.new$casualty_severity)

current_model = randomForest(casualty_severity ~ ., data=train.new[,-length(train.new)])
pred = predict(current_model, newdata = test.new[,-length(test.new)], "class")
accuracy(current_model,test.new[,-length(test.new)])
plot(pred)
current_model

#### multiplot function
#a function used to put multi ggplot graphs together 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#######################################
######Association Rules Modelling######
#######################################
test = pedestrian_analysis %>% filter(pedestrian_crossing_human_control != 'Data missing or out of range' & 
                                        pedestrian_location != 'Data missing or out of range' &
                                        pedestrian_crossing_physical_facilities != 'Data missing or out of range' &
                                        pedestrian_movement != 'Data missing or out of range' &
                                        age_band_of_casualty != 'Data missing or out of range' ) %>% 
  dplyr::select(casualty_severity, pedestrian_location,age_band_of_casualty,sex_of_casualty,
                pedestrian_crossing_physical_facilities, pedestrian_crossing_human_control,
                pedestrian_movement,weather_conditions,
                road_surface_conditions,light_conditions) %>% 
  as.data.frame() 
class(test$age_band_of_casualty)
summary(is.na(pedestrian$road_surface_conditions))

unique(test$age_band_of_casualty)
test$casualty_severity[which(test$casualty_severity == 'Fatal')] = c('KSI')
test$casualty_severity[which(test$casualty_severity == 'Serious')] = c('KSI')

ar = test #%>% filter(casualty_severity == 'Slight')
unique(ar$casualty_severity)
freq_sets <- eclat(ar,parameter=list(support=0.05,maxlen=5))#get frequent set

inspect(sort(freq_sets)[1:10])#Top 10 most frequent items


inspect(sort(freq_sets,by="support")[1:10]) #Top 10 items with the highest support
rules=apriori(ar,parameter=list(support=0.001,confidence=0.5))
rules
summary(rules)
inspect(sort(rules,by="support")[1:10])

inspect(unique(rhs(rules)))
x=subset(rules,subset=rhs%in%"casualty_severity=Slight"&lift>1.2&confidence>0.8)
summary(x)
inspect(sort(x,by="confidence"))
par(mfcol=c(2,2))
a = ggplot(x@quality,aes(support,lift,col = confidence)) + geom_point() + 
  labs(title = "Association rules for slightly injured casualties")
#plot(x, method='scatterplot',control=list(jitter=2, col = rev(brewer.pal(9, 'Greens'))), shading = 'lift')

x=subset(rules,subset=rhs%in%"casualty_severity=KSI"&lift>1.2 & confidence>0.6)
summary(x)
inspect(sort(x,by="lift"))
b = ggplot(x@quality,aes(support,lift,col = confidence)) + geom_point() + 
  labs(title = "Association rules for KSI casualties")

#plot(x, method='scatterplot',control=list(jitter=2, col = rev(brewer.pal(9, 'Greens'))), shading = 'lift')
multiplot(a,b)


