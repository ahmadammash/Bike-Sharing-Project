rm(list=ls(all=TRUE))
library(dplyr)
library(stats)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(data.table)
library(TTR)
library(scales)



#Casual in blue
#Registered in purple



#Reading the data from the csv file
data <- read.table('day.csv',header = T,sep = ",")
data %>% head()



#Plotting Casuals vs Temperature
casual_vs_temp <- data %>% ggplot(aes(x=temp,y=casual)) + 
  geom_point(color="blue4") + labs(x="Temperature",y="Number of Casual Riders",
                      title = "Number of Casual Riders vs Temperature") + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7))
casual_vs_temp
#Casuals prefer to ride at higher temperatures (around 0.5 ore more)


#Plotting Casuals vs Humidity
casual_vs_hum <- data %>% ggplot(aes(x=data$hum,y=data$casual)) + 
  geom_point(color="blue4") + labs(x="Humidity",y="Number of Casual Riders",
                      title = "Number of Casual Riders vs Humidity") + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7))
casual_vs_hum
#Casuals prefer to ride at humidity higher than 0.5


#Plotting Casuals vs Wind
casual_vs_wind <- data %>% ggplot(aes(x=data$windspeed,data$casual)) + 
  geom_point(color="blue4") + labs(x="Windspeed",y="Number of Casual Riders",
                      title = "Number of Casual Riders vs Windspeed") +  
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7))
casual_vs_wind
#Casuals prefer to ride at windspeeds between 0.1 and 0.3



#Histogram for weekdays
weekday_hist <- data %>% ggplot(aes(x=weekday)) + 
  geom_histogram() + ylim(c(0,200)) + xlim(0,7) + labs(x="Weekday",
                                           y="Number of Riders",
                                           title="Number of Riders per weekday") + 
  scale_x_continuous(n.breaks = 7,labels = c('S','M','T','W',
                                             'T','F','S')) + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) 
weekday_hist
#No preferences to a particular weekday



#Histogram for holidays
holiday_hist <- data %>% ggplot(aes(x=data$holiday)) + 
  geom_histogram() + labs(x="Holiday (Y/N)",y="Number of Riders",
                          title="Number of Riders on Holidays")  + 
  scale_x_continuous(n.breaks = 2,labels=c('Not a Holiday','Holiday')) + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7))
holiday_hist
#Almost nobody rents a bike on holidays



#Histogram for seasons
season_hist <- data %>% ggplot(aes(x=season)) + 
  geom_histogram() + ylim(c(0,250)) + labs(x="Season",y="Number of Riders",
                                          title="Number of Riders per Season") +
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) + 
  scale_x_continuous(n.breaks=4,labels=c('Spring','Summer','Fall','Winter'))
season_hist
#Spring and Summer are preferred



#Histogram for months
month_hist <- data %>% ggplot(aes(x=mnth)) + 
  geom_histogram() + 
  scale_x_continuous(breaks=c(1:12),labels = c('Jan','Feb','Mar','Apr','May'
                                                     ,'June','July','Aug','Sep','Oct',
                                                     'Nov','Dec')) +
  labs(x="Month",y="Number of Riders",title="Number of Riders per Month") + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) 
month_hist



#Totals of Casual and Registered Users
casuals_total <- data$casual %>% sum
reg_total <- data$registered %>% sum



#Number and Proportions of casual users every season
seasons_casual_agg <- aggregate(casual~season,data=data,FUN = sum) 
seasons_casual_agg <- seasons_casual_agg %>% mutate(proportion_casual = seasons_casual_agg$casual/casuals_total)
seasons_casual_agg




#Number and Proportions of casual users on holidays
holiday_casual_agg <- aggregate(casual~holiday,data=data,FUN = sum)
holiday_casual_agg <- holiday_casual_agg %>% mutate(proportion_casual = holiday_casual_agg$casual/casuals_total)
holiday_casual_agg




#Number and Proportions of casual users on weekdays
weekday_casual_agg <- aggregate(casual~weekday,data=data,FUN = sum)
weekday_casual_agg <- weekday_casual_agg %>% mutate(proportion_casual = weekday_casual_agg$casual/casuals_total)
weekday_casual_agg



#Number and Proportions of casual users each year
year_casual_agg <- aggregate(casual~yr,data = data,FUN = sum)
year_casual_agg <- year_casual_agg %>% mutate(proportion_casual = year_casual_agg$casual/casuals_total)
year_casual_agg



#Number and Proportions of casual users vs weather
weather_casual_agg <- aggregate(casual~weathersit,data = data,FUN = sum)
weather_casual_agg <- weather_casual_agg %>% mutate(proportion_casual = weather_casual_agg$casual/casuals_total)
weather_casual_agg



#Number and Proportions of registered users every season
seasons_reg_agg <- aggregate(registered~season,data = data,FUN = sum)
seasons_reg_agg <- seasons_reg_agg %>% mutate(proportion_reg = seasons_reg_agg$registered/reg_total)
seasons_reg_agg



#Number and Proportions of registered users on holidays
holiday_reg_agg <- aggregate(registered~holiday,data = data,FUN = sum)
holiday_reg_agg <- holiday_reg_agg %>% mutate(proportion_reg = holiday_reg_agg$registered/reg_total)
holiday_reg_agg



#Number and Proportions of registered users on weekdays
weekday_reg_agg <- aggregate(registered~weekday,data = data,FUN = sum)
weekday_reg_agg <- weekday_reg_agg %>% mutate(proportion_reg = weekday_reg_agg$registered/reg_total)
weekday_reg_agg




#Number and Proportions of registered users every year
year_reg_agg <- aggregate(registered~yr,data = data,FUN = sum)
year_reg_agg <- year_reg_agg %>% mutate(proportion_reg = year_reg_agg$registered/reg_total)
year_reg_agg



#Number and Proportions of registered users depending on weather
weather_reg_agg <- aggregate(registered~weathersit,data = data,FUN = sum)
weather_reg_agg <- weather_reg_agg %>% mutate(proportion_reg = weather_reg_agg$registered/reg_total)
weather_reg_agg



#Seasons merge comparison
seasons_agg <- merge(seasons_casual_agg,seasons_reg_agg)
write.csv(seasons_agg,"C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\ESSEC Foundations of Strategic Business Analytics\\SeasonsMerge.txt")



#Holiday merge comparison
holiday_agg <- merge(holiday_casual_agg,holiday_reg_agg)
write.csv(holiday_agg,"C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\ESSEC Foundations of Strategic Business Analytics\\HolidayMerge.txt")



#Weekday merge comparison
weekday_agg <- merge(weekday_casual_agg,weekday_reg_agg)
write.csv(weekday_agg,"C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\ESSEC Foundations of Strategic Business Analytics\\WeekdayMerge.txt")



#Year merge comparison
year_agg <- merge(year_casual_agg,year_reg_agg)
write.csv(year_agg,"C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\ESSEC Foundations of Strategic Business Analytics\\YearMerge.txt")



#Weather merge comparison
weather_agg <- merge(weather_casual_agg,weather_reg_agg)
write.csv(weather_agg,"C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\ESSEC Foundations of Strategic Business Analytics\\WeatherMerge.txt")



#Number and Proportions of registered users every month
month_reg_agg <- aggregate(registered~mnth,data=data,FUN = sum)
month_reg_agg <- month_reg_agg %>% mutate(proportion_reg = registered/reg_total)
write.csv(month_reg_agg,"C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\ESSEC Foundations of Strategic Business Analytics\\data.txt")



#Number and Proportions of casual users every month
month_casual_agg <- aggregate(casual~mnth,data = data,FUN = sum)
month_casual_agg <- month_casual_agg %>% mutate(proportion_casual = casual/casuals_total)
month_casual_agg
write.csv(month_casual_agg,"C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\ESSEC Foundations of Strategic Business Analytics\\data.txt")



#Scatter plots of Casuals(Temp,wind,hum)
casual_scatters <- ggarrange(casual_vs_temp,casual_vs_hum,casual_vs_wind,ncol = 2,nrow = 2)
casual_scatters



#Histograms overall (weekday,season,holiday,month)
hists <- ggarrange(weekday_hist,season_hist,holiday_hist,month_hist,ncol=2,nrow=2)
hists



#Scatter plot for Registered vs Temperature
reg_vs_temp <- data %>% ggplot(aes(x=data$temp,y=data$registered)) + geom_point(color="blueviolet") + 
  labs(x="Temperature",y="Number of Registered Users",title="Number of Registered Users as 
       a function of Temperature") + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7))
reg_vs_temp



#Scatter plot for Registered vs Humidity
reg_vs_hum <- data %>% ggplot(aes(x=data$hum,y=data$registered)) + geom_point(color="blueviolet") + 
  labs(x="Humidity",y="Number of Registered Users",title="Number of Registered Users 
       as a function of Humidity") + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7))
reg_vs_hum



#Scatter plot for Registered vs Wind
reg_vs_wind <- data %>% ggplot(aes(x=data$windspeed,y=data$registered)) + geom_point(color="blueviolet") +
  labs(x="Wind Speed",y="Number of Registered Users",title="Number of Registered Users 
       as a function of Wind Speed") + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7))
reg_vs_wind



#Scatter plots for Registered users(temp,hum,wind)
reg_scatters <- ggarrange(reg_vs_temp,reg_vs_hum,reg_vs_wind,ncol=2,nrow=2)
reg_scatters



#Comparison of Scatter plots for Registered and Casual users
casual_vs_reg_scatters <- ggarrange(casual_vs_temp,reg_vs_temp,casual_vs_hum,reg_vs_hum,casual_vs_wind,reg_vs_wind,
                                    ncol=2,nrow=3)
casual_vs_reg_scatters



#Adding overall proportions to seasons agg
seasons_agg <- seasons_agg %>% mutate(casual_overall = casual/(casual+registered)) %>% 
  mutate(reg_overall = registered/(casual+registered))
seasons_agg 
write.csv(seasons_agg,"C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\ESSEC Foundations of Strategic Business Analytics\\data.txt")



#Adding overall proportions to holiday agg
holiday_agg
holiday_agg <- holiday_agg %>% mutate(casual_overall = casual/(casual+registered)) %>% 
  mutate(reg_overall = registered/(casual+registered))
write.csv(holiday_agg,"C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\ESSEC Foundations of Strategic Business Analytics\\data.txt")


#Adding overall proportions to weather agg
weather_agg
weather_agg <- weather_agg %>% mutate(casual_overall = casual/(casual+registered)) %>% 
  mutate(reg_overall = registered/(casual+registered))
weather_agg
write.csv(weather_agg,"C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\ESSEC Foundations of Strategic Business Analytics\\data.txt")


#Adding overall proportions to weekday agg
weekday_agg <- weekday_agg %>% mutate(casual_overall = casual/(casual+registered)) %>% 
  mutate(reg_overall = registered/(casual+registered))
weekday_agg
write.csv(weekday_agg,"C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\ESSEC Foundations of Strategic Business Analytics\\data.txt")



#Adding overall proportions to year agg
year_agg <- year_agg %>% mutate(casual_overall = casual/(casual+registered)) %>% 
  mutate(reg_overall = registered/(casual+registered))
year_agg
write.csv(year_agg,"C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\ESSEC Foundations of Strategic Business Analytics\\data.txt")



#Adding overall proportions to months agg
months_agg <- merge(month_casual_agg,month_reg_agg)
months_agg <- months_agg %>% mutate(casual_overall = casual/(casual+registered)) %>% 
  mutate(reg_overall = registered/(casual+registered))
months_agg
write.csv(months_agg,"C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\ESSEC Foundations of Strategic Business Analytics\\data.txt")



#Pie chart for count of casual and registered users
pie_chart_casual_vs_reg <- pie(c(casuals_total,reg_total),labels = c("Casual Users","Registered Users"),
                               col = c("blue4","blueviolet"),title='Casual vs Registered Users Proportions')



#Regression model for Casual users
regres_casual <- lm(casual~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+hum+windspeed,data = data)
summary(regres_casual)



#Regression model for Registered users
regres_reg <- lm(registered~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+hum+windspeed,data = data)
summary(regres_reg)

#From the Casual Users Regression Summary
#Significant variables include:
#season - year - month - holiday - weekday - workingday - weathersit - temp - hum - windspeed
# ALL variables have a significant effect on the number of casual users on a specific day
#Positive effect: season - year - weekday - temperature
#Negative effect: month - holiday - working day - weather situation - humidity - windspeed

#From the Registered Users Regression Summary
#Significant variables include:
#season - year - weekday - workingday - weathersit - temp - hum - windspeed
#The difference is that holiday has an insignificant effect on the number of registered users
#Positive effect: season - year - weekday - working day - temperature
#Negative effect: month - holiday - weather situation - humidity - windspeed



#The model for the casual users has a correlation factor of 0.82
cor(regres_casual$fitted.values,data$casual)



#The model for the registered users has a correlation factor of 0.9
cor(regres_reg$fitted.values,data$registered)



#The model for the registered users predicts the dependent variable more accurately



#Bar graph for casuals by season
casual_season_bar <- seasons_agg %>% ggplot() + geom_col(aes(x=season,y=casual),fill="blue4") + 
  labs(title = "Number of Casual Users per Season",x='Season',y='Number of Casual Users') + 
  theme(plot.title = element_text(size=6.5),axis.title = element_text(size=7)) + 
  scale_x_discrete(limits = c('1'='Spring','2'='Summer','3'='Fall','4'='Winter')) +
  scale_y_continuous(labels=comma)



#Bar graph for registered users by season
reg_season_bar <- seasons_agg %>% ggplot() + geom_col(aes(x=season,y=registered),fill="blueviolet") +
  labs(title = "Number of Registered Users per Season",x='Season',y='Number of Registered Users') + 
  theme(plot.title = element_text(size=6.5),axis.title = element_text(size=7)) +
        scale_y_continuous(labels=comma) +
  scale_x_discrete(limits = c('1'='Spring','2'='Summer','3'='Fall','4'='Winter'))



#Combining the bar graphs to compare by season
casual_vs_reg_season <- ggarrange(casual_season_bar,reg_season_bar)
casual_vs_reg_season



#Bar chart for casuals by holiday
casual_holiday_bar <- holiday_agg %>% ggplot() + geom_col(aes(x=holiday,y=casual),fill="blue4") + 
  labs(title = "Number of Casual Users on Holidays",x='Holiday (Y/N)',y='Number of Casual Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) +
  scale_y_continuous(labels=comma) +
  scale_y_continuous(labels=comma) + 
  scale_x_continuous(breaks = c(0,1),labels = c('0'='Not a Holiday','1'='Holiday'))



#Bar chart for registered by holiday
reg_holiday_bar <- holiday_agg %>% ggplot() + geom_col(aes(x=holiday,y=registered),fill="blueviolet") + 
  labs(title = "Number of Registered Users on Holidays",x='Holiday (Y/N)',y='Number of Registered Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) +
  scale_y_continuous(labels=comma) + 
  scale_x_continuous(breaks = c(0,1),labels = c('0'='Not a Holiday','1'='Holiday'))
casual_vs_reg_holiday <- ggarrange(casual_holiday_bar,reg_holiday_bar)



#Combining to compare by holiday
casual_vs_reg_holiday


weather_labels = c('Good','Average','Bad')

#Bar chart for casuals by weather
casual_weather_bar <- weather_agg %>% ggplot() + geom_col(aes(x=weathersit,y=casual),fill="blue4") + 
  labs(title = "Number of Casual Users 
       as a function of Weather",x='Weather Situation',y='Number of Casual Users') + 
  theme(plot.title = element_text(size=6.5),axis.title = element_text(size=7)) +
  scale_x_discrete(limits = c('1'='Good','2'='Average','3'='Bad')) + 
  scale_y_continuous(labels=comma)



#Bar chart for registered by weather
reg_weather_bar <- weather_agg %>% ggplot() + geom_col(aes(x=weathersit,y=registered),fill="blueviolet") + 
  labs(title = "Number of Registered Users 
       as a function of Weather",x='Weather Situation',y='Number of Registered Users') + 
  theme(plot.title = element_text(size=6.5),axis.title = element_text(size=6.5)) +
  scale_x_discrete(limits = c('1'='Good','2'='Average','3'='Bad')) + 
  scale_y_continuous(labels=comma)



#Combining to compare by weather
casual_vs_reg_weather <- ggarrange(casual_weather_bar,reg_weather_bar)
casual_vs_reg_weather



#Bar chart for casuals by weekday
casual_weekday_bar <- weekday_agg %>% ggplot() + geom_col(aes(x=weekday,y=casual),fill="blue4") + 
  scale_x_discrete(limits = c(0,1,2,3,4,5,6), labels = c('S','M','T','W',
                                             'T','F','S')) + 
  labs(title = "Number of Casual Users by Day",x='Week Day',
                                          y='Number of Casual Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) +
  scale_y_continuous(labels = comma)
  
casual_weekday_bar


#Bar chart for registered by weekday
reg_weekday_bar <- weekday_agg %>% ggplot() + geom_col(aes(x=weekday,y=registered),fill="blueviolet") + 
  scale_x_discrete(limits = c(0,1,2,3,4,5,6), labels = c('S','M','T','W',
                                                         'T','F','S')) + labs(title = "Number of Registered Users by Day",
                                          x='Week Day',y='Number of Registered Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) + 
  scale_y_continuous(labels = comma)



#Combining to compare by weekday
casual_vs_reg_weekday <- ggarrange(casual_weekday_bar,reg_weekday_bar)
casual_vs_reg_weekday



#Bar chart for casuals by year
casual_year_bar <- year_agg %>% ggplot() + geom_col(aes(x=yr,y=casual),fill='blue4') + 
  labs(title = "Number of Casual Users by year",x='Year',y='Number of Casual Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) + 
  scale_x_discrete(limits=c(0,1),labels=c('0'='2011','1'='2012')) + 
  scale_y_continuous(labels=comma)



#Bar chart for registered by year
reg_year_bar <- year_agg %>% ggplot() + geom_col(aes(x=yr,y=registered),fill='blueviolet') + 
  labs(title = "Number of Registered Users by year",x='Year',y='Number of Registered Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) + 
  scale_x_discrete(limits=c(0,1),labels=c('0'='2011','1'='2012')) + 
  scale_y_continuous(labels=comma)



#Combining to compare by year
casual_vs_reg_year <- ggarrange(casual_year_bar,reg_year_bar)
casual_vs_reg_year



#Bar chart for casuals by month
casual_month_bar <- months_agg %>% ggplot() + geom_col(aes(x=mnth,y=casual),fill='blue4') + 
  scale_x_continuous(n.breaks = 12)  + labs(title = "Number of Casual Users 
                                            by Month",x='Month',y='Number of Casual Users') + 
  theme(plot.title = element_text(size=10),axis.title = element_text(size=7),
        axis.text.x = element_text(size=7,angle=45)) + 
  scale_x_discrete(limits=1:12,labels=c('1'='Jan','2'='Feb','3'='Mar','4'='Apr','5'='May','6'='June',
                                           '7'='July','8'='Aug','9'='Sep','10'='Oct','11'='Nov','12'='Dec')) + 
  scale_y_continuous(labels=comma)


#Bar chart for registered by month
reg_month_bar <- months_agg %>% ggplot() + geom_col(aes(x=mnth,y=registered),fill='blueviolet') + 
  scale_x_continuous(n.breaks = 12) + labs(title = "Number of Registered Users 
                                           by Month",x='Month',y='Number of Registered Users') + 
  theme(plot.title = element_text(size=10),axis.title = element_text(size=7),
        axis.text.x = element_text(size=7,angle=45)) + 
  scale_x_discrete(limits=1:12,labels=c('1'='Jan','2'='Feb','3'='Mar','4'='Apr','5'='May','6'='June',
                                        '7'='July','8'='Aug','9'='Sep','10'='Oct','11'='Nov','12'='Dec')) + 
  scale_y_continuous(labels=comma)



#Combining to compare by month
casual_vs_reg_month <- ggarrange(casual_month_bar,reg_month_bar) 
casual_vs_reg_month



#Viewing the aggregates (proportions and number versus an independent variable)
seasons_agg
holiday_agg
weather_agg
weekday_agg
year_agg
months_agg



#Bar chart with props for casuals by season
casual_season_prop_bar <- seasons_agg %>% ggplot() + geom_col(aes(x=season,y=casual_overall),fill='blue4') + 
  labs(title="Proportions of Casual Users by Season",x='Season',y='Proportion of Casual Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) + 
  scale_x_discrete(limits=c(1:4),labels=c('Spring','Summer','Fall','Winter'))



#Bar chart with props for registered by season
reg_season_prop_bar <- seasons_agg %>% ggplot() + geom_col(aes(x=season,y=reg_overall),fill='blueviolet') + 
  labs(title = "Proportion of Registered Users by Season",x='Season',y='Proportion of Registered Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) + 
  scale_x_discrete(limits=c(1:4),labels=c('Spring','Summer','Fall','Winter'))



#Combine to compare proportions by season
casual_reg_season_prop_bar <- ggarrange(casual_season_prop_bar,reg_season_prop_bar)
casual_reg_season_prop_bar



#Bar chart with props for casuals by holiday
casual_holiday_prop_bar <- holiday_agg %>% ggplot() + geom_col(aes(x=holiday,y=casual_overall),fill='blue4') + 
  scale_x_continuous(n.breaks = 2) + labs(title = "Proportions of Casual Users on Holidays",
                                          x='Holiday',y='Proportion of Casual Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) +
  scale_x_discrete(limits=c(0,1),labels=c('Not a Holiday','Holiday'))



#Bar chart with props for registered by holiday
reg_holiday_prop_bar <- holiday_agg %>% ggplot() + geom_col(aes(x=holiday,y=reg_overall),fill='blueviolet') + 
  scale_x_continuous(n.breaks = 2) + labs(title = "Proportions of Registered Users on Holidays",
                                          x='Holiday',y='Proportion of Registered Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7))  +
  scale_x_discrete(limits=c(0,1),labels=c('Not a Holiday','Holiday'))



#Combine to compare proportions by holiday
casual_reg_holiday_prop_bar <- ggarrange(casual_holiday_prop_bar,reg_holiday_prop_bar)
casual_reg_holiday_prop_bar



#Bar chart with props for casuals by weather
casual_weather_prop_bar <- weather_agg %>% ggplot() + geom_col(aes(x=weathersit,y=casual_overall),fill='blue4') + 
  labs(title = "Proportions of Casual Users by Weather",x='Weather Situation',
       y='Proportion of Casual Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) + 
  scale_x_discrete(limits=c(1,2,3),labels=c('Good','Average','Bad'))


#Bar chart with props for registered by weather
reg_weather_prop_bar <- weather_agg %>% ggplot() + geom_col(aes(x=weathersit,y=reg_overall),fill='blueviolet') + 
  labs(title = "Proportions of Registered Users by Weather",x='Weather Situation',
       y='Proportion of Registered Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) +
  scale_x_discrete(limits=c(1,2,3),labels=c('Good','Average','Bad'))



#Combine to compare proportions by weather
casual_reg_weather_prop_bar <- ggarrange(casual_weather_prop_bar,reg_weather_prop_bar)
casual_reg_weather_prop_bar



#Bar chart with props for casuals by weekday
casual_weekday_prop_bar <- weekday_agg %>% ggplot() + geom_col(aes(x=weekday,y=casual_overall),fill='blue4') + 
  labs(title = "Proportions of Casual Users on Weekdays",
       x='Week day',y='Proportion of Casual Users') +
  scale_x_discrete(limits=c(0:6),labels=c('S','M','T','W','T','F','S')) + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) 
  



#Bar chart with props for registered by weekday
reg_weekday_prop_bar <- weekday_agg %>% ggplot() + geom_col(aes(x=weekday,y=reg_overall),fill='blueviolet') + 
  scale_x_continuous(n.breaks = 7) + labs(title = "Proportions of Registered Users on Weekdays",
                                          x='Week day',y='Proportion of Registered Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) + 
  scale_x_discrete(limits=c(0:6),labels=c('S','M','T','W','T','F','S'))



#Combine to compare proportions by weekday
casual_reg_weekday_prop_bar <- ggarrange(casual_weekday_prop_bar,reg_weekday_prop_bar)
casual_reg_weekday_prop_bar



#Bar chart with props for casuals by year
casual_year_prop_bar <- year_agg %>% ggplot() + geom_col(aes(x=yr,y=casual_overall),fill='blue4') + 
  scale_x_continuous(n.breaks = 2) + labs(title = "Proportions of Casual Users by Year",
                                          x='Year',y='Proportion of Casual Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) +
  scale_x_discrete(limits=c(0,1),labels=c('2011','2012'))



#Bar chart with props for registered by year
reg_year_prop_bar <- year_agg %>% ggplot() + geom_col(aes(x=yr,y=reg_overall),fill='blueviolet') + 
  scale_x_continuous(n.breaks = 2) + labs(title = "Proportions of Registered Users by Year",
                                          x='Year',y='Proportion of Registered Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7)) +
  scale_x_discrete(limits=c(0,1),labels=c('2011','2012'))




#Combine to compare proportions by year
casual_reg_year_prop_bar <- ggarrange(casual_year_prop_bar,reg_year_prop_bar)
casual_reg_year_prop_bar



#Bar chart with props for casuals by month
casual_month_prop_bar <- months_agg %>% ggplot() + geom_col(aes(x=mnth,y=casual_overall),fill='blue4') + 
  scale_x_discrete(limits=c(1:12),labels=c('Jan','Feb','Mar','Apr','May','June','July',
                                           'Aug','Sep','Oct','Nov','Dec')) + 
  labs(title = "Proportions of Casual Users by Month",
                                           x='Month',y='Proportion of Casual Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7),axis.text.x = element_text(size=6,angle=45)) 



#Bar chart with props for registered by month
reg_month_prop_bar <- months_agg %>% ggplot() + geom_col(aes(x=mnth,y=reg_overall),fill='blueviolet') + 
  scale_x_discrete(limits=c(1:12),labels=c('Jan','Feb','Mar','Apr','May','June','July',
                                           'Aug','Sep','Oct','Nov','Dec')) +  
  labs(title = "Proportions of Casual Users by Month",
                                            x='Month',y='Proportion of Registered Users') + 
  theme(plot.title = element_text(size=7),axis.title = element_text(size=7),axis.text.x = element_text(size=6,angle=45)) 



#Combine to compare proportions by month
casual_reg_month_prop_bar <- ggarrange(casual_month_prop_bar,reg_month_prop_bar)
casual_reg_month_prop_bar

combined_bar_charts <- ggarrange(casual_reg_holiday_prop_bar,casual_reg_month_prop_bar,casual_reg_season_prop_bar,
                                 casual_reg_weather_prop_bar,casual_reg_weekday_prop_bar,casual_reg_year_prop_bar)
combined_bar_charts
