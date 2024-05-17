library(plotly)
library(tidyverse)

View(cars)
?cars

cars.lm <- lm(dist ~ speed, data=cars)
summary(cars.lm)

cars2 <- cbind(rbind(cars, 
                     cbind(speed=rep(0,50), dist=cars$dist),
                     cbind(speed=cars$speed, dist=cars.lm$fitted.values)),
               frame = rep(c(2,1,3), each=50))
plot_ly(cars2,
        x = ~speed,
        y = ~dist,
        frame = ~frame,
        type = 'scatter',
        mode = 'markers',
        showlegend = F,
        marker=list(color="firebrick")) %>%
  layout(title="Stopping Distance of 1920's Vehicles\n (cars data set)",
         xaxis=list(title="Vehicles Speed in mph (speed)"),
         yaxis=list(title="Stopping Distance in Feet (dist)")) %>%
  add_segments(x = 0, xend = 25, y = mean(cars$dist), yend = mean(cars$dist), line=list(color="gray", dash="dash", width=1), inherit=FALSE, name=TeX("\\bar{Y}"), showlegend=T) %>%
  add_segments(x = 0, xend = 25, y = sum(coef(cars.lm)*c(1,0)), yend = sum(coef(cars.lm)*c(1,25)), line=list(color="darkgray", width=1), inherit=FALSE, name=TeX("\\hat{Y}"), showlegend=T) %>%
  config(mathjax = 'cdn') %>%
  animation_opts(frame=2000, transition=1000, redraw=T)

mean(cars$dist)

sum(1:6)
sum(1:100)
sum(c(5, 15, 2, 29, 35, 24, 25, 39)^2)

sum((1:6)^2)

var(c(5, 15, 2, 29, 35, 24, 25, 39))

some_data = c(5, 15, 2, 29, 35, 24, 25, 39)
datamean = mean(some_data)

sum((some_data-datamean)^2 / length(some_data)/1)


plot(cars$speed,cars$dist,pch=16,color="red",size=1)

ggplot(cars, aes(x=speed,y=dist)) +
  geom_point() +
  geom_smooth(method="lm", se=F,formula=y~x, aes(linetype = "Regression Line")) +
  geom_hline(aes(yintercept = mean(dist), linetype = "Y-bar")) + 
  labs(title = "main title", x="speed of cars", y="distance in feet", linetype = "Linetype") +
  geom_segment(aes(x=speed, xend=speed, y=dist, yend=cars.lm$fit)) + 
  geom_rect(aes(xmin=speed,ymax=speed-cars.lm$residuals*.15,ymin=dist, alpha = 0.1)) # *.5 to make them squares because of the distance ratio

ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = "Regression Line")) +
  geom_hline(yintercept = mean(cars$dist), linetype = "Y-bar") + 
  labs(title = "Main Title", x = "Speed of Cars", y = "Distance in Feet", linetype = "Linetype") +
  geom_segment(aes(x = speed, xend = speed, y = dist, yend = cars.lm$fit)) + 
  geom_rect(aes(xmin = speed, xmax = speed, ymin = dist, ymax = cars.lm$fit, fill = "Residuals"), alpha = 0.1) +
  scale_fill_manual(values = "red")

# Variance - unexplained behavior of dots around their mean

sum( (cars$dist - cars.lm$fit)^2 )
sum( (cars.lm$fit - mean(cars$dist))^2 )
sum( (cars$dist - mean(cars$dist))^2 )


# ssr / ssto -> r^2
r2 <- sum( (cars.lm$fit - mean(cars$dist))^2 ) / sum( (cars$dist - mean(cars$dist))^2 )
sqrt(r2)

# 1 is thea total 'explainability' or 'variability' from the data to the mean - ybar - y is it varying? 
# correlation is a measure of the data, not the model