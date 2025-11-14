library(ggplot2)

house.data <- read.csv("NY-House-Dataset.csv", header = TRUE)
View(house.data)

price <- house.data$PRICE
PropertySqFt <- house.data$PROPERTYSQFT
NumBaths <- house.data$BATH
NumBeds <- house.data$BEDS

#Beds only variable
lin.mod0 <- lm(price ~ NumBeds, data = house.data)
  (lin.mod0)

ggplot(house.data, aes(x = BEDS, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
summary(lin.mod0)

#Beds and bath used 
lin.mod1 <- lm(price ~ NumBeds + NumBaths, data = house.data)
summary(lin.mod1)

house.data$PredictedPrice <- predict(lin.mod1)

ggplot(house.data, aes(x = PredictedPrice, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(x = "Predicted Price", y = "Actual Price",
       title = "Predicted vs Actual House Prices")

#Beds, Baths, and PropertySqFt used
lin.mod2 <- lm(price ~ NumBeds + NumBaths + PropertySqFt, data = house.data)
summary(lin.mod2)

house.data$PredictedPrice2 <- predict(lin.mod2)

ggplot(house.data, aes(x = PredictedPrice2, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "green") +
  labs(x = "Predicted Price", y = "Actual Price",
       title = "Predicted vs Actual House Prices 2")


