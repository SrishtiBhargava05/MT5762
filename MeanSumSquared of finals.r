rel <- step(lm(wt ~ mwt+dwt+ht+gestation+smoker+drace), direction = 'both', trace = 0)
intrel <- step(lm(wt ~ (mwt+ dwt + ht + gestation + smoker + drace)^2), direction = 'both', trace = 0)
noges <- step(lm(wt ~ mwt+dwt+ht+dht+smoker+parity+age), direction = 'both', trace = 0)
babiesNoNANoSmoke <- select(babiesNoNA, -smoker)
allnosmoke <- step(lm(wt~.,babiesNoNANoSmoke), direction = 'both', trace = 0)
r2sr <- lm(wt~gestation + parity + ht + mwt + drace + dwt + time + number, data = babiesNoNA)

# 
# Smoking made into boolean
# AIC method of all values   : wt ~ mwt+ dwt + ht + gestation + smoker + drace
# AIC method of all but with interaction: wt ~ mwt + dwt + ht + gestation + smoker + drace + mwt:gestation + dwt:gestation + dwt:smoker + dwt:drace + ht:smoker + gestation:smoker
# AIC method no gestation    : wt ~ mwt + dwt + ht + smoker
# 
# Smoking not made into boolean
# AIC method of all values   :  wt ~ gestation + parity + ht + mwt + drace + dwt + time + number
# Type II Anova by R2        :  Srishti's


GetMeanSquaredError(rel, validation.data.set)
GetMeanSquaredError(intrel, validation.data.set)
GetMeanSquaredError(noges, validation.data.set)
GetMeanSquaredError(allnosmoke, validation.data.set)
GetMeanSquaredError(r2sr, validation.data.set)

babiesRaw$smoker <- babiesRaw$smoke == 1
babiesFull <- na.omit(select(babiesRaw, wt, wt.1, dwt, ht, gestation, smoker, drace))
relFull <- step(lm(wt~mwt+dwt+ht+gestation+smoker+drace), direction = 'both', trace = 0)
