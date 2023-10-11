# project-vanessa-dasuki

library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
view(airline_passenger_satisfaction)

library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
view(airline_passenger_satisfaction)

**##perform chi-square for effect of categorical features on categorical label (C2C)**

**#create a contigency table with variables Gender and Satisfaction**
gender_satisfaction.data = table(airline_passenger_satisfaction$Gender,airline_passenger_satisfaction$Satisfaction)
print(gender_satisfaction.data)
**#perform chi square test with variables gender and satisfaction**
chisq.test(gender_satisfaction.data)
**#gender_satisfaction bargraph**
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
ggplot(airline_passenger_satisfaction, aes(Satisfaction), ..count..)+
  geom_bar(aes(fill=Gender), position="dodge")
       
**#create a contingency table with variables Customer Type and Satisfaction**
customertype_satisfaction.data = table(airline_passenger_satisfaction$`Customer Type`,airline_passenger_satisfaction$Satisfaction)
print(customertype_satisfaction.data)
**#perform chi square test with variables Customer Type and satisfaction**
chisq.test(customertype_satisfaction.data)
**#customertype_satisfaction.data bar graph**
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
airline_passenger_satisfaction$customer_type<- airline_passenger_satisfaction$'Customer Type'
ggplot(airline_passenger_satisfaction, aes(Satisfaction), ..count..)+
  geom_bar(aes(fill=customer_type), position="dodge")
       
**#create a contingency table with variable Type of Travel and Satisfaction**
typeoftravel_satisfaction.data = table(airline_passenger_satisfaction$`Type of Travel`,airline_passenger_satisfaction$Satisfaction)
print(typeoftravel_satisfaction.data)
**#perform chi square test with variables Type of Travel and Satisfaction**
chisq.test(typeoftravel_satisfaction.data)
**#typeoftravel_satisfaction bar graph**
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
airline_passenger_satisfaction$type_of_travel<- airline_passenger_satisfaction$`Type of Travel`
ggplot(airline_passenger_satisfaction, aes(Satisfaction), ..count..)+
  geom_bar(aes(fill=type_of_travel), position="dodge")

**#create a contingency table with variable Class and Satisfaction**
class_satisfaction.data = table(airline_passenger_satisfaction$Class,airline_passenger_satisfaction$Satisfaction)
print(class_satisfaction.data)
**#perform chi square test with variables Class and Satisfaction**
chisq.test(class_satisfaction.data)
**#class_satisfaction bar graph**
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
ggplot(airline_passenger_satisfaction, aes(Satisfaction), ..count..)+
  geom_bar(aes(fill=Class), position="dodge")
 

## Categorical to Numerical

**#perform t-test for effect of categorical feature having 2 classes on numeric label (C2N)**
**#perform t-test with variables Age and Satisfaction**
t.test(airline_passenger_satisfaction$Age ~ airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA **
age.aov <- aov(airline_passenger_satisfaction$Age~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(age.aov)
**#create bar graph**
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
airline_passenger_satisfaction$Age <- airline_passenger_satisfaction$Age
airline_passenger_satisfaction$Satisfaction<-airline_passenger_satisfaction$Satisfaction
library(carData)
library(dplyr)


df <-airline_passenger_satisfaction %>% 
  group_by(Satisfaction) %>% 
  summarise(Age_mean = mean(Age))
df <- as.data.frame(df)
ggplot(df)+aes(x=Satisfaction,y = Age_mean)+
  geom_bar(stat = "identity")

**#perform t-test with variables Departure Delay and Satisfaction**
t.test(airline_passenger_satisfaction$`Departure Delay` ~ airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
departuredelay.aov <- aov(airline_passenger_satisfaction$`Departure Delay`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(departuredelay.aov)

library(dplyr)
df2 <- airline_passenger_satisfaction %>%
    group_by(Satisfaction) %>%
    summarise(departure_delay = mean(`Departure Delay`))
df2
df2 <- as.data.frame(df2)
ggplot(df2)+aes(x=Satisfaction, y = departure_delay)+
  geom_bar(stat = "identity")

**#perform t-test with variables Arrival Delay and Satisfaction**
t.test(airline_passenger_satisfaction$`Arrival Delay` ~ airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA**
arrivaldelay.aov <- aov(airline_passenger_satisfaction$`Arrival Delay`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(arrivaldelay.aov)
# Arrival Delay mean bar graph
library(dplyr)
airline_passenger_satisfaction <- drop_na(airline_passenger_satisfaction)

df3 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(arrival_delay = mean(`Arrival Delay`))

ggplot(df3, aes(x=Satisfaction, y = arrival_delay)) +
  geom_bar(stat = "identity") 
 
**#perform t-test with variables Departure and Arrival Time Convenience and Satisfaction**
t.test(airline_passenger_satisfaction$`Departure and Arrival Time Convenience`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
departureandarrivaltimeconvenience.aov <- aov(airline_passenger_satisfaction$`Departure and Arrival Time Convenience`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(departureandarrivaltimeconvenience.aov)
# Depature and Arrival Time Convinience mean bar graph
df4 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(departure_arrival = mean(`Departure and Arrival Time Convenience`))
df4 <- as.data.frame(df4)
ggplot(df4)+aes(x=Satisfaction, y = departure_arrival)+
  geom_bar(stat = "identity")


**#perform t-test with variables Ease of Online Booking and Satisfaction**
t.test(airline_passenger_satisfaction$`Ease of Online Booking`~airline_passenger_satisfaction$Satisfaction, data = airline_passenger_satisfaction)
#perform ANOVA
easeofonlinebooking.aov <- aov(airline_passenger_satisfaction$`Ease of Online Booking`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(easeofonlinebooking.aov)
# Ease of Online Booking mean bar graph
df5 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(ease_online = mean (`Ease of Online Booking`))
df5 <- as.data.frame(df5)
ggplot(df5)+aes(x=Satisfaction, y = ease_online)+
  geom_bar(stat = "identity")

**#perform t-test with variables Check-in Service and Satisfaction**
t.test(airline_passenger_satisfaction$`Check-in Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
checkinservice.aov <- aov(airline_passenger_satisfaction$`Check-in Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(checkinservice.aov)
# Check-in Service mean bar graph
df6 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(checkin_service = mean(`Check-in Service`))
df6 <- as.data.frame(df6)
ggplot(df6)+aes(x=Satisfaction, y = checkin_service)+
  geom_bar(stat = "identity")

**#perform t-test with variables Online Boarding and Satisfaction**
t.test(airline_passenger_satisfaction$`Online Boarding`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
onlineboarding.aov <- aov(airline_passenger_satisfaction$`Online Boarding`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(onlineboarding.aov)
**#Online Boarding mean bar graph**
df7 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(online_boarding = mean(`Online Boarding`))
df7 <- as.data.frame(df7)
ggplot(df7)+aes(x=Satisfaction, y = online_boarding)+
  geom_bar(stat = "identity")


**#perform t-test with variables Gate Location and Satisfaction**
t.test(airline_passenger_satisfaction$`Gate Location`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA 
gatelocation.aov <- aov(airline_passenger_satisfaction$`Gate Location`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(gatelocation.aov)
#Gate Location mean bar graph
df8 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(gate_location = mean(`Gate Location`))
df8 <- as.data.frame(df8)
ggplot(df8)+aes(x=Satisfaction, y = gate_location)+
  geom_bar(stat = "identity")

**#perform t-test with variables On-Board Service and Satisfaction**
t.test(airline_passenger_satisfaction$`On-board Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
onboardservice.aov <- aov(airline_passenger_satisfaction$`On-board Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(onboardservice.aov)
# On-Board Service mean bar graph
df9 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(onboard_service = mean(`On-board Service`))
df9 <- as.data.frame(df9)
ggplot(df9)+aes(x=Satisfaction, y = onboard_service)+
  geom_bar(stat = "identity")

**#perform t-test with variables Seat Comfort and Satisfaction**
t.test(airline_passenger_satisfaction$`Seat Comfort`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
seatcomfort.aov <- aov(airline_passenger_satisfaction$`Seat Comfort`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(seatcomfort.aov)    
**#Seat Comfort mean bar graph**
df10 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(seat_comfort = mean(`Seat Comfort`))
df10 <- as.data.frame(df10)
ggplot(df10)+aes(x=Satisfaction, y = seat_comfort)+
  geom_bar(stat = "identity")

**#perform t-test with variables Leg Room Service and Satisfaction**
t.test(airline_passenger_satisfaction$`Leg Room Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
legroomservice.aov <- aov(airline_passenger_satisfaction$`Leg Room Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(legroomservice.aov)
**#Leg Room Service mean bar graph**
df11 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(legroom_service = mean(`Leg Room Service`))
df11 <- as.data.frame(df11)
ggplot(df11)+aes(x=Satisfaction, y = legroom_service)+
  geom_bar(stat = "identity")

**#perform t-test with variables Cleanliness and Satisfaction**
t.test(airline_passenger_satisfaction$Cleanliness~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA**
cleanliness.aov <- aov(airline_passenger_satisfaction$Cleanliness~airline_passenger_satisfaction$Satisfaction)
summary(cleanliness.aov)
# Cleanliness mean bar graoh
df12 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(cleanliness_mean = mean(Cleanliness))
df12 <- as.data.frame(df12)
ggplot(df12)+aes(x=Satisfaction, y = cleanliness_mean)+  
  geom_bar(stat = "identity")
  
**#perform t-test with variables Food and Drink and Satisfaction**
t.test(airline_passenger_satisfaction$`Food and Drink`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA**
foodanddrink.aov <- aov(airline_passenger_satisfaction$`Food and Drink`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(foodanddrink.aov)
**#Food and Drink mean bar graph**
df13 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(food_drink = mean(`Food and Drink`))
df13 <- as.data.frame(df13)
ggplot(df13)+aes(x=Satisfaction, y = food_drink)+
  geom_bar(stat = "identity")
  
**#perform t-test with variables In-Flight Service and Satisfaction**
t.test(airline_passenger_satisfaction$`In-flight Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA**
inflightservice.aov <- aov(airline_passenger_satisfaction$`In-flight Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(inflightservice.aov)
**#In-Flight Service mean bar graph**
df14 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(inflight_service = mean(`In-flight Service`))
df14 <- as.data.frame(df14)
ggplot(df14)+aes(x=Satisfaction, y = inflight_service)+
  geom_bar(stat = "identity")

**#perform t-test with variables In-Flight Wifi Service and Satisfaction**
t.test(airline_passenger_satisfaction$`In-flight Wifi Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA**
inflightiwifiservice.aov <- aov(airline_passenger_satisfaction$`In-flight Wifi Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(inflightiwifiservice.aov)
**#In-Flight Wifi Service mean bar graph**
df15 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(inflight_wifi = mean(`In-flight Wifi Service`))
df15 <- as.data.frame(df15)
ggplot(df15)+aes(x=Satisfaction, y = inflight_wifi)+
  geom_bar(stat = "identity")
            
**#perform t-test with variables in-flight entertainment and Satisfaction**
t.test(airline_passenger_satisfaction$`In-flight Entertainment`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA**
inflightentertainment.aov <- aov(airline_passenger_satisfaction$`In-flight Entertainment`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(inflightentertainment.aov)
# In-flight Entertainment mean bar graph
df16 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(inflight_entertainment = mean(`In-flight Entertainment`))
df16 <- as.data.frame(df16)
ggplot(df16)+aes(x=Satisfaction, y = inflight_entertainment)+
  geom_bar(stat = "identity")

#perform t-test with variables baggage handling and satisfaction
t.test(airline_passenger_satisfaction$`Baggage Handling`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
baggagehandling.aov <- aov(airline_passenger_satisfaction$`Baggage Handling`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(baggagehandling.aov)
# Baggage Handling mean bar graph
df17 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
summarise(baggage_handling = mean(`Baggage Handling`))  
ggplot(df17)+aes(x=Satisfaction, y = baggage_handling)+
  geom_bar(stat = "identity")  

**#create a contigency table with variables Gender and Satisfaction**

gender_satisfaction.data = table(airline_passenger_satisfaction$Gender,airline_passenger_satisfaction$Satisfaction)
print(gender_satisfaction.data)
**#perform chi square test with variables gender and satisfaction**
chisq.test(gender_satisfaction.data)
**#gender_satisfaction bargraph**
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
ggplot(airline_passenger_satisfaction, aes(Satisfaction), ..count..)+
  geom_bar(aes(fill=Gender), position="dodge")
       
**#create a contingency table with variables Customer Type and Satisfaction**
customertype_satisfaction.data = table(airline_passenger_satisfaction$`Customer Type`,airline_passenger_satisfaction$Satisfaction)
print(customertype_satisfaction.data)
**#perform chi square test with variables Customer Type and satisfaction**
chisq.test(customertype_satisfaction.data)
**#customertype_satisfaction.data bar graph**
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
airline_passenger_satisfaction$customer_type<- airline_passenger_satisfaction$'Customer Type'
ggplot(airline_passenger_satisfaction, aes(Satisfaction), ..count..)+
  geom_bar(aes(fill=customer_type), position="dodge")
       
**#create a contingency table with variable Type of Travel and Satisfaction**
typeoftravel_satisfaction.data = table(airline_passenger_satisfaction$`Type of Travel`,airline_passenger_satisfaction$Satisfaction)
print(typeoftravel_satisfaction.data)
**#perform chi square test with variables Type of Travel and Satisfaction**
chisq.test(typeoftravel_satisfaction.data)
**#typeoftravel_satisfaction bar graph**
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
airline_passenger_satisfaction$type_of_travel<- airline_passenger_satisfaction$`Type of Travel`
ggplot(airline_passenger_satisfaction, aes(Satisfaction), ..count..)+
  geom_bar(aes(fill=type_of_travel), position="dodge")

**#create a contingency table with variable Class and Satisfaction**
class_satisfaction.data = table(airline_passenger_satisfaction$Class,airline_passenger_satisfaction$Satisfaction)
print(class_satisfaction.data)
**#perform chi square test with variables Class and Satisfaction**
chisq.test(class_satisfaction.data)
**#class_satisfaction bar graph**
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
ggplot(airline_passenger_satisfaction, aes(Satisfaction), ..count..)+
  geom_bar(aes(fill=Class), position="dodge")
 

## Categorical to Numerical

**#perform t-test for effect of categorical feature having 2 classes on numeric label (C2N)
#perform t-test with variables Age and Satisfaction**
t.test(airline_passenger_satisfaction$Age ~ airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA **
age.aov <- aov(airline_passenger_satisfaction$Age~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(age.aov)
**#create bar graph**
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
airline_passenger_satisfaction$Age <- airline_passenger_satisfaction$Age
airline_passenger_satisfaction$Satisfaction<-airline_passenger_satisfaction$Satisfaction
library(carData)
library(dplyr)


df <-airline_passenger_satisfaction %>% 
  group_by(Satisfaction) %>% 
  summarise(Age_mean = mean(Age))
df <- as.data.frame(df)
ggplot(df)+aes(x=Satisfaction,y = Age_mean)+
  geom_bar(stat = "identity")

**#perform t-test with variables Departure Delay and Satisfaction**
t.test(airline_passenger_satisfaction$`Departure Delay` ~ airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA**
departuredelay.aov <- aov(airline_passenger_satisfaction$`Departure Delay`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(departuredelay.aov)

library(dplyr)
df2 <- airline_passenger_satisfaction %>%
    group_by(Satisfaction) %>%
    summarise(departure_delay = mean(`Departure Delay`))
df2
df2 <- as.data.frame(df2)
ggplot(df2)+aes(x=Satisfaction, y = departure_delay)+
  geom_bar(stat = "identity")

**#perform t-test with variables Arrival Delay and Satisfaction**
t.test(airline_passenger_satisfaction$`Arrival Delay` ~ airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA**
arrivaldelay.aov <- aov(airline_passenger_satisfaction$`Arrival Delay`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(arrivaldelay.aov)
# Arrival Delay mean bar graph
library(dplyr)
airline_passenger_satisfaction <- drop_na(airline_passenger_satisfaction)

df3 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(arrival_delay = mean(`Arrival Delay`))

ggplot(df3, aes(x=Satisfaction, y = arrival_delay)) +
  geom_bar(stat = "identity") 
 
**#perform t-test with variables Departure and Arrival Time Convenience and Satisfaction**
t.test(airline_passenger_satisfaction$`Departure and Arrival Time Convenience`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA**
departureandarrivaltimeconvenience.aov <- aov(airline_passenger_satisfaction$`Departure and Arrival Time Convenience`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(departureandarrivaltimeconvenience.aov)
# Depature and Arrival Time Convinience mean bar graph
df4 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(departure_arrival = mean(`Departure and Arrival Time Convenience`))
df4 <- as.data.frame(df4)
ggplot(df4)+aes(x=Satisfaction, y = departure_arrival)+
  geom_bar(stat = "identity")


**#perform t-test with variables Ease of Online Booking and Satisfaction**
t.test(airline_passenger_satisfaction$`Ease of Online Booking`~airline_passenger_satisfaction$Satisfaction, data = airline_passenger_satisfaction)
**#perform ANOVA**
easeofonlinebooking.aov <- aov(airline_passenger_satisfaction$`Ease of Online Booking`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(easeofonlinebooking.aov)
# Ease of Online Booking mean bar graph
df5 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(ease_online = mean (`Ease of Online Booking`))
df5 <- as.data.frame(df5)
ggplot(df5)+aes(x=Satisfaction, y = ease_online)+
  geom_bar(stat = "identity")

**#perform t-test with variables Check-in Service and Satisfaction**
t.test(airline_passenger_satisfaction$`Check-in Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA**
checkinservice.aov <- aov(airline_passenger_satisfaction$`Check-in Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(checkinservice.aov)
# Check-in Service mean bar graph
df6 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(checkin_service = mean(`Check-in Service`))
df6 <- as.data.frame(df6)
ggplot(df6)+aes(x=Satisfaction, y = checkin_service)+
  geom_bar(stat = "identity")

**#perform t-test with variables Online Boarding and Satisfaction**
t.test(airline_passenger_satisfaction$`Online Boarding`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA**
onlineboarding.aov <- aov(airline_passenger_satisfaction$`Online Boarding`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(onlineboarding.aov)
**#Online Boarding mean bar graph**
df7 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(online_boarding = mean(`Online Boarding`))
df7 <- as.data.frame(df7)
ggplot(df7)+aes(x=Satisfaction, y = online_boarding)+
  geom_bar(stat = "identity")


**#perform t-test with variables Gate Location and Satisfaction**
t.test(airline_passenger_satisfaction$`Gate Location`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA **
gatelocation.aov <- aov(airline_passenger_satisfaction$`Gate Location`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(gatelocation.aov)
**#Gate Location mean bar graph**
df8 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(gate_location = mean(`Gate Location`))
df8 <- as.data.frame(df8)
ggplot(df8)+aes(x=Satisfaction, y = gate_location)+
  geom_bar(stat = "identity")

**#perform t-test with variables On-Board Service and Satisfaction**
t.test(airline_passenger_satisfaction$`On-board Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA**
onboardservice.aov <- aov(airline_passenger_satisfaction$`On-board Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(onboardservice.aov)
# On-Board Service mean bar graph
df9 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(onboard_service = mean(`On-board Service`))
df9 <- as.data.frame(df9)
ggplot(df9)+aes(x=Satisfaction, y = onboard_service)+
  geom_bar(stat = "identity")

**#perform t-test with variables Seat Comfort and Satisfaction**
t.test(airline_passenger_satisfaction$`Seat Comfort`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**#perform ANOVA**
seatcomfort.aov <- aov(airline_passenger_satisfaction$`Seat Comfort`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(seatcomfort.aov)    
**#Seat Comfort mean bar graph**
df10 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(seat_comfort = mean(`Seat Comfort`))
df10 <- as.data.frame(df10)
ggplot(df10)+aes(x=Satisfaction, y = seat_comfort)+
  geom_bar(stat = "identity")

**#perform t-test with variables Leg Room Service and Satisfaction**
t.test(airline_passenger_satisfaction$`Leg Room Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
legroomservice.aov <- aov(airline_passenger_satisfaction$`Leg Room Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(legroomservice.aov)
**#Leg Room Service mean bar graph**
df11 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(legroom_service = mean(`Leg Room Service`))
df11 <- as.data.frame(df11)
ggplot(df11)+aes(x=Satisfaction, y = legroom_service)+
  geom_bar(stat = "identity")

library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
view(airline_passenger_satisfaction)

##perform chi-square for effect of categorical features on categorical label (C2C)

#create a contigency table with variables Gender and Satisfaction
gender_satisfaction.data = table(airline_passenger_satisfaction$Gender,airline_passenger_satisfaction$Satisfaction)
print(gender_satisfaction.data)
#perform chi square test with variables gender and satisfaction
chisq.test(gender_satisfaction.data)
#gender_satisfaction bargraph
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
ggplot(airline_passenger_satisfaction, aes(Satisfaction), ..count..)+
  geom_bar(aes(fill=Gender), position="dodge")
       
#create a contingency table with variables Customer Type and Satisfaction
customertype_satisfaction.data = table(airline_passenger_satisfaction$`Customer Type`,airline_passenger_satisfaction$Satisfaction)
print(customertype_satisfaction.data)
#perform chi square test with variables Customer Type and satisfaction
chisq.test(customertype_satisfaction.data)
#customertype_satisfaction.data bar graph
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
airline_passenger_satisfaction$customer_type<- airline_passenger_satisfaction$'Customer Type'
ggplot(airline_passenger_satisfaction, aes(Satisfaction), ..count..)+
  geom_bar(aes(fill=customer_type), position="dodge")
       
#create a contingency table with variable Type of Travel and Satisfaction
typeoftravel_satisfaction.data = table(airline_passenger_satisfaction$`Type of Travel`,airline_passenger_satisfaction$Satisfaction)
print(typeoftravel_satisfaction.data)
#perform chi square test with variables Type of Travel and Satisfaction
chisq.test(typeoftravel_satisfaction.data)
#typeoftravel_satisfaction bar graph
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
airline_passenger_satisfaction$type_of_travel<- airline_passenger_satisfaction$`Type of Travel`
ggplot(airline_passenger_satisfaction, aes(Satisfaction), ..count..)+
  geom_bar(aes(fill=type_of_travel), position="dodge")

#create a contingency table with variable Class and Satisfaction
class_satisfaction.data = table(airline_passenger_satisfaction$Class,airline_passenger_satisfaction$Satisfaction)
print(class_satisfaction.data)
#perform chi square test with variables Class and Satisfaction
chisq.test(class_satisfaction.data)
#class_satisfaction bar graph
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
ggplot(airline_passenger_satisfaction, aes(Satisfaction), ..count..)+
  geom_bar(aes(fill=Class), position="dodge")
 

## Categorical to Numerical

#perform t-test for effect of categorical feature having 2 classes on numeric label (C2N)
#perform t-test with variables Age and Satisfaction
t.test(airline_passenger_satisfaction$Age ~ airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA 
age.aov <- aov(airline_passenger_satisfaction$Age~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(age.aov)
#create bar graph
library(readxl)
airline_passenger_satisfaction <- read_excel("~/Documents/uni/Spring 2022/TBANLT 485/Final Project/airline_passenger_satisfaction.xlsx")
airline_passenger_satisfaction$Age <- airline_passenger_satisfaction$Age
airline_passenger_satisfaction$Satisfaction<-airline_passenger_satisfaction$Satisfaction
library(carData)
library(dplyr)


df <-airline_passenger_satisfaction %>% 
  group_by(Satisfaction) %>% 
  summarise(Age_mean = mean(Age))
df <- as.data.frame(df)
ggplot(df)+aes(x=Satisfaction,y = Age_mean)+
  geom_bar(stat = "identity")

#perform t-test with variables Departure Delay and Satisfaction
t.test(airline_passenger_satisfaction$`Departure Delay` ~ airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
departuredelay.aov <- aov(airline_passenger_satisfaction$`Departure Delay`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(departuredelay.aov)

library(dplyr)
df2 <- airline_passenger_satisfaction %>%
    group_by(Satisfaction) %>%
    summarise(departure_delay = mean(`Departure Delay`))
df2
df2 <- as.data.frame(df2)
ggplot(df2)+aes(x=Satisfaction, y = departure_delay)+
  geom_bar(stat = "identity")

#perform t-test with variables Arrival Delay and Satisfaction
t.test(airline_passenger_satisfaction$`Arrival Delay` ~ airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
arrivaldelay.aov <- aov(airline_passenger_satisfaction$`Arrival Delay`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(arrivaldelay.aov)
# Arrival Delay mean bar graph
library(dplyr)
airline_passenger_satisfaction <- drop_na(airline_passenger_satisfaction)

df3 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(arrival_delay = mean(`Arrival Delay`))

ggplot(df3, aes(x=Satisfaction, y = arrival_delay)) +
  geom_bar(stat = "identity") 
 
#perform t-test with variables Departure and Arrival Time Convenience and Satisfaction
t.test(airline_passenger_satisfaction$`Departure and Arrival Time Convenience`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
departureandarrivaltimeconvenience.aov <- aov(airline_passenger_satisfaction$`Departure and Arrival Time Convenience`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(departureandarrivaltimeconvenience.aov)
# Depature and Arrival Time Convinience mean bar graph
df4 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(departure_arrival = mean(`Departure and Arrival Time Convenience`))
df4 <- as.data.frame(df4)
ggplot(df4)+aes(x=Satisfaction, y = departure_arrival)+
  geom_bar(stat = "identity")


#perform t-test with variables Ease of Online Booking and Satisfaction
t.test(airline_passenger_satisfaction$`Ease of Online Booking`~airline_passenger_satisfaction$Satisfaction, data = airline_passenger_satisfaction)
#perform ANOVA
easeofonlinebooking.aov <- aov(airline_passenger_satisfaction$`Ease of Online Booking`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(easeofonlinebooking.aov)
# Ease of Online Booking mean bar graph
df5 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(ease_online = mean (`Ease of Online Booking`))
df5 <- as.data.frame(df5)
ggplot(df5)+aes(x=Satisfaction, y = ease_online)+
  geom_bar(stat = "identity")

#perform t-test with variables Check-in Service and Satisfaction
t.test(airline_passenger_satisfaction$`Check-in Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
checkinservice.aov <- aov(airline_passenger_satisfaction$`Check-in Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(checkinservice.aov)
# Check-in Service mean bar graph
df6 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(checkin_service = mean(`Check-in Service`))
df6 <- as.data.frame(df6)
ggplot(df6)+aes(x=Satisfaction, y = checkin_service)+
  geom_bar(stat = "identity")

#perform t-test with variables Online Boarding and Satisfaction
t.test(airline_passenger_satisfaction$`Online Boarding`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
onlineboarding.aov <- aov(airline_passenger_satisfaction$`Online Boarding`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(onlineboarding.aov)
#Online Boarding mean bar graph
df7 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(online_boarding = mean(`Online Boarding`))
df7 <- as.data.frame(df7)
ggplot(df7)+aes(x=Satisfaction, y = online_boarding)+
  geom_bar(stat = "identity")


#perform t-test with variables Gate Location and Satisfaction
t.test(airline_passenger_satisfaction$`Gate Location`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA 
gatelocation.aov <- aov(airline_passenger_satisfaction$`Gate Location`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(gatelocation.aov)
#Gate Location mean bar graph
df8 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(gate_location = mean(`Gate Location`))
df8 <- as.data.frame(df8)
ggplot(df8)+aes(x=Satisfaction, y = gate_location)+
  geom_bar(stat = "identity")

#perform t-test with variables On-Board Service and Satisfaction
t.test(airline_passenger_satisfaction$`On-board Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
onboardservice.aov <- aov(airline_passenger_satisfaction$`On-board Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(onboardservice.aov)
# On-Board Service mean bar graph
df9 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(onboard_service = mean(`On-board Service`))
df9 <- as.data.frame(df9)
ggplot(df9)+aes(x=Satisfaction, y = onboard_service)+
  geom_bar(stat = "identity")

#perform t-test with variables Seat Comfort and Satisfaction
t.test(airline_passenger_satisfaction$`Seat Comfort`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
seatcomfort.aov <- aov(airline_passenger_satisfaction$`Seat Comfort`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(seatcomfort.aov)    
#Seat Comfort mean bar graph
df10 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(seat_comfort = mean(`Seat Comfort`))
df10 <- as.data.frame(df10)
ggplot(df10)+aes(x=Satisfaction, y = seat_comfort)+
  geom_bar(stat = "identity")

#perform t-test with variables Leg Room Service and Satisfaction
t.test(airline_passenger_satisfaction$`Leg Room Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
legroomservice.aov <- aov(airline_passenger_satisfaction$`Leg Room Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(legroomservice.aov)
#Leg Room Service mean bar graph
df11 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(legroom_service = mean(`Leg Room Service`))
df11 <- as.data.frame(df11)
ggplot(df11)+aes(x=Satisfaction, y = legroom_service)+
  geom_bar(stat = "identity")

#perform t-test with variables Cleanliness and Satisfaction
t.test(airline_passenger_satisfaction$Cleanliness~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
cleanliness.aov <- aov(airline_passenger_satisfaction$Cleanliness~airline_passenger_satisfaction$Satisfaction)
summary(cleanliness.aov)
# Cleanliness mean bar graoh
df12 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(cleanliness_mean = mean(Cleanliness))
df12 <- as.data.frame(df12)
ggplot(df12)+aes(x=Satisfaction, y = cleanliness_mean)+  
  geom_bar(stat = "identity")
  
#perform t-test with variables Food and Drink and Satisfaction
t.test(airline_passenger_satisfaction$`Food and Drink`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
foodanddrink.aov <- aov(airline_passenger_satisfaction$`Food and Drink`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(foodanddrink.aov)
#Food and Drink mean bar graph
df13 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(food_drink = mean(`Food and Drink`))
df13 <- as.data.frame(df13)
ggplot(df13)+aes(x=Satisfaction, y = food_drink)+
  geom_bar(stat = "identity")
  
#perform t-test with variables In-Flight Service and Satisfaction
t.test(airline_passenger_satisfaction$`In-flight Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
inflightservice.aov <- aov(airline_passenger_satisfaction$`In-flight Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(inflightservice.aov)
#In-Flight Service mean bar graph
df14 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(inflight_service = mean(`In-flight Service`))
df14 <- as.data.frame(df14)
ggplot(df14)+aes(x=Satisfaction, y = inflight_service)+
  geom_bar(stat = "identity")

#perform t-test with variables In-Flight Wifi Service and Satisfaction
t.test(airline_passenger_satisfaction$`In-flight Wifi Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
inflightiwifiservice.aov <- aov(airline_passenger_satisfaction$`In-flight Wifi Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(inflightiwifiservice.aov)
#In-Flight Wifi Service mean bar graph
df15 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(inflight_wifi = mean(`In-flight Wifi Service`))
df15 <- as.data.frame(df15)
ggplot(df15)+aes(x=Satisfaction, y = inflight_wifi)+
  geom_bar(stat = "identity")
            
#perform t-test with variables in-flight entertainment and Satisfaction
t.test(airline_passenger_satisfaction$`In-flight Entertainment`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
inflightentertainment.aov <- aov(airline_passenger_satisfaction$`In-flight Entertainment`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(inflightentertainment.aov)
# In-flight Entertainment mean bar graph
df16 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(inflight_entertainment = mean(`In-flight Entertainment`))
df16 <- as.data.frame(df16)
ggplot(df16)+aes(x=Satisfaction, y = inflight_entertainment)+
  geom_bar(stat = "identity")

#perform t-test with variables baggage handling and satisfaction
t.test(airline_passenger_satisfaction$`Baggage Handling`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
baggagehandling.aov <- aov(airline_passenger_satisfaction$`Baggage Handling`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(baggagehandling.aov)
# Baggage Handling mean bar graph
df17 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
summarise(baggage_handling = mean(`Baggage Handling`))  
ggplot(df17)+aes(x=Satisfaction, y = baggage_handling)+
  geom_bar(stat = "identity")  

t.test(airline_passenger_satisfaction$Cleanliness~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
cleanliness.aov <- aov(airline_passenger_satisfaction$Cleanliness~airline_passenger_satisfaction$Satisfaction)
summary(cleanliness.aov)
# Cleanliness mean bar graoh
df12 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(cleanliness_mean = mean(Cleanliness))
df12 <- as.data.frame(df12)
ggplot(df12)+aes(x=Satisfaction, y = cleanliness_mean)+  
  geom_bar(stat = "identity")
  
#perform t-test with variables Food and Drink and Satisfaction
t.test(airline_passenger_satisfaction$`Food and Drink`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
foodanddrink.aov <- aov(airline_passenger_satisfaction$`Food and Drink`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(foodanddrink.aov)
#Food and Drink mean bar graph
df13 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(food_drink = mean(`Food and Drink`))
df13 <- as.data.frame(df13)
ggplot(df13)+aes(x=Satisfaction, y = food_drink)+
  geom_bar(stat = "identity")
  
**#perform t-test with variables In-Flight Service and Satisfaction**
t.test(airline_passenger_satisfaction$`In-flight Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
inflightservice.aov <- aov(airline_passenger_satisfaction$`In-flight Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(inflightservice.aov)
#In-Flight Service mean bar graph
df14 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(inflight_service = mean(`In-flight Service`))
df14 <- as.data.frame(df14)
ggplot(df14)+aes(x=Satisfaction, y = inflight_service)+
  geom_bar(stat = "identity")

**#perform t-test with variables In-Flight Wifi Service and Satisfaction**
t.test(airline_passenger_satisfaction$`In-flight Wifi Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
inflightiwifiservice.aov <- aov(airline_passenger_satisfaction$`In-flight Wifi Service`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(inflightiwifiservice.aov)

**#In-Flight Wifi Service mean bar graph**
df15 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(inflight_wifi = mean(`In-flight Wifi Service`))
df15 <- as.data.frame(df15)
ggplot(df15)+aes(x=Satisfaction, y = inflight_wifi)+
  geom_bar(stat = "identity")
            
**# perform t-test with variables in-flight entertainment and Satisfaction**
t.test(airline_passenger_satisfaction$`In-flight Entertainment`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
**# perform ANOVA**
inflightentertainment.aov <- aov(airline_passenger_satisfaction$`In-flight Entertainment`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(inflightentertainment.aov)

# In-flight Entertainment mean bar graph
df16 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
  summarise(inflight_entertainment = mean(`In-flight Entertainment`))
df16 <- as.data.frame(df16)
ggplot(df16)+aes(x=Satisfaction, y = inflight_entertainment)+
  geom_bar(stat = "identity")

**#perform t-test with variables baggage handling and satisfaction**
t.test(airline_passenger_satisfaction$`Baggage Handling`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
#perform ANOVA
baggagehandling.aov <- aov(airline_passenger_satisfaction$`Baggage Handling`~airline_passenger_satisfaction$Satisfaction, data=airline_passenger_satisfaction)
summary(baggagehandling.aov)

# Baggage Handling mean bar graph
df17 <- airline_passenger_satisfaction %>%
  group_by(Satisfaction) %>%
summarise(baggage_handling = mean(`Baggage Handling`))  
ggplot(df17)+aes(x=Satisfaction, y = baggage_handling)+
  geom_bar(stat = "identity")  
