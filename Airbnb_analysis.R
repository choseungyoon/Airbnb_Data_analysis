install.packages("dplyr")
install.packages("ggplot2")

library(ggplot2)
library(dplyr)

wd = "/Users/seungyuncho/Desktop/Data_science_Airbnb/Airbnb_Data_analysis"
airbnb <- read.table(paste(wd,"/train_users_2.csv",sep=""), head=T, sep=",", stringsAsFactors=F)

#See variable & observation
glimpse(airbnb)
summary(airbnb)


#Set standard date
str(airbnb$date_account_created)
str(airbnb$date_first_booking)

airbnb$date_account_created <- as.Date(airbnb$date_account_created)
airbnb$date_account_created.num <- as.numeric(airbnb$date_account_created)
airbnb$date_account_created.year <- as.numeric(format(airbnb$date_account_created, "%y"))
airbnb$date_account_created.week <- as.numeric(format(airbnb$date_account_created+3, "%U"))
airbnb$date_account_created.month <- as.numeric(format(airbnb$date_account_created, "%m"))
airbnb$date_account_created.day <- as.numeric(format(airbnb$date_account_created, "%d"))
airbnb$date_account_created.weekdays <- as.numeric(as.factor(weekdays(airbnb$date_account_created)))

airbnb$date_first_booking <- as.Date(airbnb$date_first_booking,format = "%Y-%m-%d")
airbnb$date_first_booking.num <- as.numeric(airbnb$date_first_booking)
airbnb$date_first_booking.year <- as.numeric(format(airbnb$date_first_booking, "%y"))
airbnb$date_date_first_booking.week <- as.numeric(format(airbnb$date_first_booking+3, "%U"))
airbnb$date_first_booking.month <- as.numeric(format(airbnb$date_first_booking, "%m"))
airbnb$date_first_booking.day <- as.numeric(format(airbnb$date_first_booking, "%d"))
airbnb$date_first_booking.weekdays <- as.numeric(as.factor(weekdays(airbnb$date_first_booking)))


#  date_first_booking by date_account_created 
airbnb %>% 
  sample_n(2000) %>% 
  filter(age < 150) %>%
  ggplot(aes(date_account_created, date_first_booking)) + 
  geom_point(aes(size=age, col=signup_app),alpha = 0.5) + geom_smooth() +
  ggtitle("Date_first_booking by date_account_created ")

# view signup_methond
airbnb %>% ggplot(aes(signup_method)) + geom_bar()

pairs(airbnb %>% sample_n(1000))

#View age by data_account_created
airbnb %>% 
  sample_n(2000) %>% ggplot(aes(age, date_account_created)) + geom_point(alpha=.5) + geom_smooth()


#overall dest table stats
#what % of users go to which country
t = table(airbnb$country_destination) 
print(signif(t / sum(t) * 100), digits = 2)
barplot(t/sum(t))


#destination_country table stats by signup app
#At a glance, phone-signup users is slightly more likely to not make a booking (NDF)
t = table(airbnb$signup_app, airbnb$country_destination) 
print(signif(t / rowSums(t) * 100, digits=2))
d = as.data.frame(t / rowSums(t))
names(d) = c("signup_app", "destination_country", "Freq")
d %>%
  ggplot(aes(signup_app, Freq, fill=destination_country)) + geom_bar(stat="identity")



#statistics
sum(is.na(airbnb$date_account_created))
min(airbnb$date_account_created)
max(airbnb$date_account_created)
median(airbnb$date_account_created)
summary(airbnb$date_account_created)

#View count of each country_destination by data_account_created.month
fig.dac_mon <- ggplot(airbnb, aes(date_account_created.month, fill=country_destination))
fig.dac_mon <- fig.dac_mon + geom_bar(position="identity", alpha=0.5)
fig.dac_mon <- fig.dac_mon + scale_y_log10()
fig.dac_mon + facet_wrap(~country_destination)


# View count of age between 14 and 100
not.na <- !is.na(airbnb$age)
fig.age <- ggplot(airbnb[not.na,], aes(age, fill=country_destination))
fig.age <- fig.age + geom_bar(position="identity", alpha=0.5)
fig.age <- fig.age + xlim(14, 100)
fig.age <- fig.age + scale_y_log10()
fig.age + facet_wrap(~country_destination)

# View age chart between 10 and 100
airbnb %>% ggplot(aes(age)) +  xlim(10, 100) + geom_bar()

# between 10 and 70
fig.age.bx <- ggplot(airbnb, aes(x=country_destination, y=age))
fig.age.bx <- fig.age.bx + geom_boxplot(aes(colour=country_destination))
fig.age.bx <- fig.age.bx + ylim(10, 70)
fig.age.bx

#language
fig.lang.sp <- ggplot(airbnb, aes(country_destination, fill=country_destination))
fig.lang.sp <- fig.lang.sp + geom_bar(position="identity", alpha=0.5)
fig.lang.sp <- fig.lang.sp + scale_y_log10()
fig.lang.sp + facet_wrap(~language)
