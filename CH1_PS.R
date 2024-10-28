install.packages("readxl")
library(readxl)

data = read_excel("~/Documents/data analysis/Earnings_and_Height.xlsx")
#1. 67
summary(data)

#1.(a). 44488.4359
earning_lt_height <- subset(data, height <= 67) 
earning_lt <- mean(earning_lt_height$earnings) 

#1.(b). 49987.875 
earning_lat_height <- subset(data, height > 67) 
earning_lat <- mean(earning_lat_height$earnings) 

# 1.(c). yes, 5499.4399, 
  # 95 percent confidence interval: -6292.643 -4706.237
diff <- earning_lat - earning_lt
t_test_result <- t.test(earning_lat_height$earnings, earning_lt_height$earnings)
t_test_result

#2
ols1 <- lm(earnings~height, data = data)
summary(ols1)
confint(ols1, level = 0.95)

predicted_value <- predict(ols1, newdata =  data.frame(height =  c(67, 70, 65)) )
print(predicted_value)

#3 female only
female_data <- subset(data, sex == 0)
ols2 <- lm(earnings~height, data = female_data)
summary(ols2)
confint(ols2, level = 0.95)

mean_height = mean(female_data$height)
y <- 511.2*x +12650.9
x<- mean_height
x<- (mean_height+1)
print(y)
#or
predicted_value <- predict(ols2, newdata = data.frame(height = mean_height))
print(predicted_value)

coef(ols2)

#4 male only
male_data <- subset(data, sex == 1)
ols3 <- lm(earnings~height, data = male_data)
summary(ols3)
confint(ols3, level = 0.95)

mean_height = mean(male_data$height)
y <- 1306.9*x - 43130.3
x<- mean_height
x<- (mean_height+1)
print(y)
#or
predicted_value <- predict(ols3, newdata = data.frame(height = mean_height))
print(predicted_value)
print(coef(ols3))



#5
#male sex == 1, female sex == 0
ols4<-lm(earnings~sex*height,data=data)
summary(ols4)

install.packages("car")  # 패키지 설치
library(car) 
lht(ols4,c("sex:height=0"))

#6 -> 코드 없음

#7.
less_strength <- subset(data, occupation %in% c(1:8))
ols5<-lm(earnings~height,data=less_strength)
summary(ols5)

#8.
#(i)
data$LS_HS = as.numeric(data$educ < 12)
data$HS = as.numeric(data$educ == 12)
data$Some_col = as.numeric(data$educ > 12 & data$educ < 16)
data$College = as.numeric(data$educ >= 16)
#(ii)
  #(A)
data$female = as.numeric(data$sex == 0)
data$male = as.numeric(data$sex == 1)

ols = lm(earnings~female+height,data = data)
summary(ols)
ols6 = lm(earnings~female+height+LS_HS+HS+Some_col, data = data)
summary(ols_cg)

  #(B) -> 코드 없음 
  #(C)
lht(ols6, c("LS_HS=0","HS=0","Some_col=0"))
  #(D) -> 코드 없음 

#(iii)
  #(A)
ols_male = lm(earnings~male+height,data = data)
summary(ols_male)
ols7 = lm(earnings~male+height+LS_HS+HS+Some_col, data = data)
summary(ols7)

  #(B) -> 코드 없음 
  #(C)
lht(ols7, c("LS_HS=0","HS=0","Some_col=0"))
  #(D) -> 코드 없음





