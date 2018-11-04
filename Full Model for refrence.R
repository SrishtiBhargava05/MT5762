#LinearModel

FullModel <- lm(wt ~ ., data = babiesNoNA)
summary(FullModel)
SteppedModel <- step(FullModel)

summary(SteppedModel)
