library(tidyverse)
library(ggfortify)
library(patchwork)

cars2010 <- read.csv("data/cars2010.csv")
cars2010 <- read.csv("data/cars2011.csv")


# We can load plots from the 2010 and 2011 datasets
plot_2010 <- ggplot(cars2010) +
  geom_point(aes(y=FE, x=EngDispl))+
  labs(title = "2010 Model Year",
       x="Engine Displacement",
       y="Fuel Efficency (MPG)")

plot_2011 <- ggplot(cars2011) +
  geom_point(aes(y=FE, x=EngDispl))+
  labs(title = "2011 Model Year",
       x="Engine Displacement",
       y="Fuel Efficency (MPG)")

# And use patchwork to put them side by side
plot_2010 + plot_2011


linModel = lm(FE ~ EngDispl, data = cars2010)
summary(linModel)


# We can use geom_smooth to automatically model the LM's slope
# Note that the automatic formula is y~x, it does not need to be declared here
train_predicts <- ggplot(data=cars2010,
       aes(
         x = EngDispl,
         y = FE
       )) +
       geom_point(color='blue') +
       geom_smooth(method="lm",formula=y~x, se = FALSE,color='orange') +
      labs(
      title = '2010 Model Year',
      x = 'Engine Displacement',
      y = 'Fuel Efficency (MPG)'
      )
      

fePredicts  = select(cars2010,FE)   %>%
  mutate(fe_pred=predict(linModel,newdata=cars2010))

actual_v_pred <- ggplot(data=fePredicts,
       aes(x=FE,y=fe_pred)) +
       geom_point(color='blue') +
       geom_smooth(method='lm',se = FALSE, color='orange')

train_predicts + actual_v_pred
