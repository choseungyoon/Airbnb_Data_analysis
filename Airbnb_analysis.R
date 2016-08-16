install.packages("dplyr")
install.packages("ggplot2")

library(ggplot2)
library(dplyr)


airbnb = train_users_2

#See variable & observation
glimpse(airbnb)
summary(airbnb)

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

