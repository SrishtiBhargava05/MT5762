BabiesData<- read.table("~/Downloads/babies23.data", header = TRUE)

#RemovingColumns
BabiesData<- BabiesData%>%select(-id,-pluralty,-sex,-outcome)


#RecodingData
BabiesData$date <- as.Date(BabiesData$date, origin = '1961-01-01')
BabiesData$gestation[BabiesData$gestation == 999] <- NA
BabiesData$race <- recode(BabiesData$race, '0' = 'White', '1' = 'White', '2' = 'White', 
                          '3' = 'White', '4' = 'White', '5' = 'White', '6' = 'Mexican',
                          '7' = 'Black','8' = 'Asian', '9' = 'Mixed')
BabiesData$race <- as.factor(BabiesData$race)
BabiesData$parity[BabiesData$parity == 99] <- NA
BabiesData$age[BabiesData$age == 99] <- NA
BabiesData$ed <- recode(BabiesData$ed, '0' = 'less than 8th grade', 
                        '1' = '8th -12th grade - did not graduate', 
                        '2' = 'HS graduate--no other schooling', '3' = 'HS+trade',
                        '4' = 'HS+some college', '5' = 'College graduate', '6' = 'Trade school HS unclear',
                        '7' = 'Trade school HS unclear')
BabiesData$ed <- as.factor(BabiesData$ed)
BabiesData$ht[BabiesData$ht == 99] <- NA
BabiesData$wt.1[BabiesData$wt.1 == 999] <- NA
BabiesData$drace <- recode(BabiesData$drace, '0' = 'White', '1' = 'White', '2' = 'White', 
                           '3' = 'White', '4' = 'White', '5' = 'White', '6' = 'Mexican',
                           '7' = 'Black','8' = 'Asian', '9' = 'Mixed', .default = '')
BabiesData$drace <- as.factor(BabiesData$drace)
BabiesData$dage[BabiesData$dage == 99] <- NA
BabiesData$ded <- recode(BabiesData$ded, '0' = 'less than 8th grade', 
                         '1' = '8th -12th grade - did not graduate', 
                         '2' = 'HS graduate--no other schooling', '3' = 'HS+trade',
                         '4' = 'HS+some college', '5' = 'College graduate', '6' = 'Trade school HS unclear',
                         '7' = 'Trade school HS unclear')
BabiesData$ded <- as.factor(BabiesData$ded)
BabiesData$dht[BabiesData$dht == 99] <- NA
BabiesData$dwt[BabiesData$dwt == 99] <- NA
BabiesData$marital <- recode(BabiesData$marital, '1' = 'Married', '2' = 'Legally Separated', '3' = 'Divorced',
                             '4' = 'Widowed', '5' = 'Never Married')
BabiesData$marital <- as.factor(BabiesData$marital)
BabiesData$inc[BabiesData$inc == 98] <- NA
BabiesData$inc<- as.factor(BabiesData$inc)
BabiesData$smoke <- recode(BabiesData$smoke, '0' = 'Never', '1' = 'Smokes Now', 
                           '2' = 'Until Current Pregnancy', '3' = 'Once Did, not now')
BabiesData$smoke<- as.factor(BabiesData$smoke)
BabiesData$time <- recode(BabiesData$time, '0' = 'Never smoked', '1' = 'Still Smokes',
                          '2' = 'During current preg', '3' = 'Within 1 yr', '4' = '1 to 2 years ago',
                          '5' = '2 to 3 yr ago', '6' = '3 to 4 yrs ago', '7'= '5 to 9yrs ago', 
                          '8' = '10+yrs ago', '9' = 'quit and dont know')
BabiesData$time<-as.factor(BabiesData$time)
BabiesData$number <- recode(BabiesData$number, '0' = 'Never', '1' = '1-4',
                            '2' = '5-9', '3' = '10-14', '4' = '15-19',
                            '5' = '20-29', '6' = '30-39', '7'= '40-60', 
                            '8' = '60 +', '9'= 'smoke but dont know')
BabiesData$number <- as.factor(BabiesData$number)
BabiesData <- BabiesData %>% rename('mwt' = 'wt.1', 'mht' = 'ht', 'mrace' = 'race', 'mage' = 'age', 'med' = 'ed')