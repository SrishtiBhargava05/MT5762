#LinearModel
BabiesDataNAOM<- na.omit(BabiesData)
FullModel <- lm(wt ~ ., data = BabiesDataNAOM)
summary(FullModel)
SteppedModel <- step(FullModel)

summary(SteppedModel)