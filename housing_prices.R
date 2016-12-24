rm(list = ls()) # Clear out my environment workspace
setwd("C:/Users/sparta/Desktop/R/housing prices")

#Read the housing data
# as.is conforms no character column converted to Factor
house_data=read.csv("data.csv")
dim(house_data)
str(house_data)
View(house_data)

# Data Cleaning
library(dplyr)
memory.limit()
library(caret)
str(house_data)
names(house_data)
# Create numerical variable for different factor variables
house_data= mutate(house_data, Date_id=match(Date, unique(Date)),
                   Postcode_id=match(Postcode, unique(Postcode)),
                   Property_Type_id=match(Property_Type, unique(Property_Type)),
                   Old_New_id=match(Old_New, unique(Old_New)),
                   Duration_id=match(Duration, unique(Duration)),
                   Street_id=match(Street, unique(Street)),
                   Locality_id=match(Locality, unique(Locality)),
                   Town_id=match(Town, unique(Town)),
                   District_id=match(District, unique(District)),
                   County_id=match(County, unique(County)),
                   PPD_Category_Type_id=match(PPD_Category_Type, unique(PPD_Category_Type)))

# Create training and test sets
attach(house_data)
house_training= filter(house_data, Price!="")
house_test=house_data[which(house_data$ID > 98817),]

### Modelling ###
# Generate correlations
cor(house_data[,c(2,14:24)])
plot(pairs(house_data[,c(2,14:24)]))

names(house_data)

## 1. Random Forest
library(randomForest)
model=randomForest(Price ~ Date_id + Postcode_id + Property_Type_id + Old_New_id + Duration_id 
                   + Street_id + Locality_id + Town_id + District_id + County_id + 
                     PPD_Category_Type_id, data = house_training, na.action=na.omit)
model

## Prediction
house_test$Price=predict(model, house_test[14:24])

row1= data.frame(filter(house_test, ID==99563))
price1=predict(model, row1[14:24])
price1


## 2. Regression

# Backward elimination
step(glm(Price ~ Date_id + Postcode_id + Property_Type_id + Old_New_id + Duration_id 
         + Street_id + Locality_id + Town_id + District_id + County_id + 
           PPD_Category_Type_id, data = house_training), direction = "backward")

model=glm(Price ~ Date_id + Postcode_id + Property_Type_id, data = house_training, na.action=na.omit)
model
## Prediction
house_test$Price=predict(model, house_test[14:24])

row1= data.frame(filter(house_test, ID==99563))
price1=predict(model, row1[14:24])
price1

# Forward selection
step(glm(Price~1, data = house_training), direction = "forward",
     scope =~ Date_id + Postcode_id + Property_Type_id + Old_New_id + Duration_id 
     + Street_id + Locality_id + Town_id + District_id + County_id + PPD_Category_Type_id)

model=glm(Price ~ Date_id + County_id + Property_Type_id, data = house_training, na.action=na.omit)
model
## Prediction
house_test$Price=predict(model, house_test[14:24])

row1= data.frame(filter(house_test, ID==99563))
price1=predict(model, row1[14:24])
price1

# Stepwise selection
step(glm(Price ~ Date_id + Postcode_id + Property_Type_id + Old_New_id + Duration_id 
         + Street_id + Locality_id + Town_id + District_id + County_id + 
           PPD_Category_Type_id, data = house_training), direction = "both")

model=glm(Price ~ Date_id + Postcode_id + Property_Type_id, data = house_training, na.action=na.omit)
model
## Prediction
house_test$Price=predict(model, house_test[14:24])

row1= data.frame(filter(house_test, ID==99563))
price1=predict(model, row1[14:24])
price1