#=========================================================================================


trend.14.test <- insight.data %>% select(journey_name, Day, Visits, day.x) %>% group_by(journey_name) %>% 
  filter(Day >= start_14_sun_start & Day <= end_14_sun_end)
model14 <- lm(Visits ~ day.x, trend.14.test)
plot(trend.14.test$day.x, trend.14.test$Visits, type = "l")
lines(trend.14.test$day.x,
      predict(model14),
      col = 2,
      lwd = 2)
my_coef <- coef(model14)            # Extract coefficients of model
my_coef  
my_equation <- paste("y =",        # Extract equation of model
                     coef(model14)[[1]],
                     "+",
                     coef(model14)[[2]],
                     "* x")
my_equation 
summary(model14)$r.squared
#------------------------------------------------------------------------------------

trend.7.test <- insight.data %>% select(journey_name, Day, Visits, day.x) %>% group_by(journey_name) %>% 
  filter(Day >= start_7_sun_start & Day <= end_7_sun_end)
model7 <- lm(Visits ~ day.x, trend.7.test)
plot(trend.7.test$Day, trend.7.test$Visits, type = "l")
lines(trend.7.test$Day,
      predict(model7),
      col = 2,
      lwd = 2)
my_coef <- coef(model7)            # Extract coefficients of model
my_coef  
my_equation <- paste("y =",        # Extract equation of model
                     coef(model7)[[1]],
                     "+",
                     coef(model7)[[2]],
                     "* x")
my_equation 
summary(model7)$r.squared
#------------------------------------------------------------------------------------

  