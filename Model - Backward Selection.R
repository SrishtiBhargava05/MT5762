#Backward Selection 
#FullLinearModel

FullModel <- lm(wt ~ ., data = babiesNoNA)
summary(FullModel)

Anova(FullModel)

#Removing Variable with highest p value 
Model<- update(FullModel, .~ . - age)
summary(Model)
Anova(Model)
#adjusted R squared increases 

model1<- update(Model, .~. -race)
summary(model1)
Anova(model1)
#adjusted R squared increases 

model2<- update(model1,.~. -dage)
summary(model2)
Anova(model2)
#adjusted R squared increases 

model3 <- update(model2,.~. -marital)
summary(model3)
Anova(model3)
#adjusted R squared increases 

model4 <- update(model3,.~. -ded)
summary(model4)
Anova(model4)
#adjusted R squared increases 

model5<- update(model4,.~. -date)
summary(model5)
Anova(model5)
#adjusted R squared increases 

model6<-update(model5,.~. -ed)
summary(model6)
Anova(model6)
#adjusted R squared increases 

model7<- update(model6,.~. -smoke)
summary(model7)
Anova(model7)

model8 <- update(model7, .~. -inc)
summary(model8)
Anova(model8)

model9 <- update(model8,.~. -dht)
summary(model9)
Anova(model9)

##Removing any other variables reduces the R-squared 

finalmodel <- model9

##testing the model
hist(finalmodel$residuals)
shapiro.test(resid(finalmodel))
plot(finalmodel)


