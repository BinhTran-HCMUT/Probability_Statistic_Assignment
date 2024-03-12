#import owid_covid_data.csv & change the "date" column from 'character' format (mm/dd/yy) to 'date' format (yyyy-mm-dd) 
library(readr)
library(dbplyr)
library(ggplot2)
library(qqplotr)

#1/
# Read file
gia_nha <- read_csv("gia_nha.csv")

#2/
#Clean Data
new_df<-subset(gia_nha, select = c(price, floors, condition, view, sqft_above, sqft_living, sqft_basement))
colSums(is.na(new_df))
apply(is.na(new_df), 2, which)
apply(new_df, 2, function(x)sum(is.na(x))/length(x))
new_df<-na.omit(new_df)

#3/
#Data visualization
## Logarit category price, sqft_above, sqft_living for easier analytic.
new_df$sqft_basement<- new_df$sqft_basement + 1
new_df$price<-log(new_df$price)
new_df$sqft_above<-log(new_df$sqft_above)
new_df$sqft_living<-log(new_df$sqft_living)
new_df$sqft_basement<-log(new_df$sqft_basement)

## Discrete variable: floors, condition, view.
table_floors<-as.data.frame(table(new_df$floors))
table_condition<-as.data.frame(table(new_df$condition))
table_view<-as.data.frame(table(new_df$view))

## Continuous variable: price, sqft_above, sqft_living, sqft_basement
stat_table<-apply(new_df[,c("price", "sqft_above", "sqft_living", "sqft_basement")], 2, function(x){c(mean(x), median(x), sd(x), min(x), max(x))})
rownames(stat_table)<-c("mean", "median", "sd", "min", "max")

# Hist plot for price
hist(new_df$price, main="Do thi phan phoi cua price", col = "blue", border = "white", xlab = "Price", breaks = 20)

# Boxplot price for each floor.
boxplot(price~floors, new_df, main="Phan phoi price cho moi floor", col = "blue")

# Boxplot price for each condition 
boxplot(price~condition, new_df, main="Phan phoi price cho moi condition", col = "blue")

# Boxplot price for each view
boxplot(price~view, new_df, main="Phan phoi price cho moi view", col = "blue")

#Pairs plot price for each sqft_above, sqft_living, sqft_basement
contTab = subset(new_df, select = c(price, sqft_above, sqft_living, sqft_basement))
pairs(contTab)
#pairs ( price~sqft_above, new_df )
#pairs ( price~sqft_living, new_df)
#pairs(price~sqft_basement, new_df)

#4/
## Choosing the model: the price is the dependent variable and all other categories are independent variables.
linearModule <-lm(price~sqft_living+sqft_above+floors+condition+sqft_basement+view, data=new_df)
summary(linearModule)

## p-value of all independent variable is 2e-16 < 0,05 = alpha, so the all independent variable are meaningful.

## Coefficients of linear model
linearModule$coefficients

##SSE, SSR, SST
SSE <- (linearModule$residuals ^ 2) %>% sum()
SSR <- ((linearModule$fitted.values - mean(new_df$price)) ^ 2) %>% sum()
SST <- ((new_df$price - mean(new_df$price)) ^ 2) %>% sum()
SSE
SSR
SST

##Standard deviation of error
error.sd <- sqrt(SSE / linearModule$df.residual)
error.sd

##Normalize Probability Plot
ggplot(mapping = aes(sample = linearModule$residuals)) +
      stat_qq_point(size = 1) + stat_qq_line(color = "red") + 
labs(title = "Normal probability plot")

#Example predict 1
x1<-data.frame(sqft_living = mean(new_df$sqft_living), sqft_above = mean(new_df$sqft_above), floors = max(new_df$floors), condition = max(new_df$condition), 
               sqft_basement = mean(new_df$sqft_basement), view = min(new_df$view))

pricePredict<- as.data.frame(predict(linearModule, newdata = x1, interval = "confidence"))
pricePredict