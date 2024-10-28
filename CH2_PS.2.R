library(readxl)
library(plm) 
library(zoo)
library(lmtest)
library(stargazer)
setwd("/Users/munseung-eun/Documents/data analysis")

id <- read_excel("income_democracy.xlsx")
Guns <- read_excel("Guns.xlsx")

#Q1

#1
#(1)
#(a)
re <- plm(log(vio)~shall, data = Guns,index = c("stateid", "year"), model = "random")
summary(re)
coeftest(re,vcovHC(re,type="HC0",cluster="group")) 

#(2)
#(b)
re2 <- plm(log(vio)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029, data = Guns, index = c("stateid", "year"), model="random")
summary(re2)
coeftest(re2,vcovHC(re2,type="HC0",cluster="group")) 

#(c): violent를 대하는 문화적 특성 등

#2
fe <- plm(log(vio)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029, data = Guns, index = c("stateid", "year"), model="within")
summary(fe)

phtest(fe,re2)

#3 add time dummy
fe2 <- plm(log(vio)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029+year, data = Guns, index = c("stateid", "year"), model="within")
summary(fe2)

#4 - rob
re_rob <- plm(log(rob)~shall, data = Guns,index = c("stateid", "year"), model = "random")
summary(re_rob)
coeftest(re_rob,vcovHC(re_rob,type="HC0",cluster="group")) 

re2_rob <- plm(log(rob)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029, data = Guns, index = c("stateid", "year"), model="random")
summary(re2_rob)
coeftest(re2_rob,vcovHC(re2_rob,type="HC0",cluster="group")) 

fe_rob <- plm(log(rob)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029, data = Guns, index = c("stateid", "year"), model="within")
summary(fe_rob)

phtest(fe_rob,re2_rob)

fe2_rob <- plm(log(rob)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029+year, data = Guns, index = c("stateid", "year"), model="within")
summary(fe2_rob)

#4 - mur
re_mur <- plm(log(mur)~shall, data = Guns,index = c("stateid", "year"), model = "random")
summary(re_mur)
coeftest(re_mur,vcovHC(re_mur,type="HC0",cluster="group")) 

re2_mur <- plm(log(mur)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029, data = Guns, index = c("stateid", "year"), model="random")
summary(re2_mur)
coeftest(re2_mur,vcovHC(re2_mur,type="HC0",cluster="group")) 

fe_mur <- plm(log(mur)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029, data = Guns, index = c("stateid", "year"), model="within")
summary(fe_mur)

phtest(fe_mur,re2_mur)

fe2_mur <- plm(log(mur)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029+year, data = Guns, index = c("stateid", "year"), model="within")
summary(fe2_mur)


stargazer(re,re2,fe,fe2, type = "text", digits = 3, header = FALSE, title= "Panel Results",
          model.names = FALSE,
          model.numbers = FALSE,
          out = "CH2_PS.1.text",
          column.labels   = c("RE","RE","FE","FE"))

stargazer(re_rob,re2_rob,fe_rob,fe2_rob, type = "text", digits = 3, header = FALSE, title= "Panel Results",
          model.names = FALSE,
          model.numbers = FALSE,
          out = "CH2_PS.2.text",
          column.labels   = c("RE","RE","FE","FE"))

stargazer(re_mur,re2_mur,fe_mur,fe2_mur, type = "text", digits = 3, header = FALSE, title= "Panel Results",
          model.names = FALSE,
          model.numbers = FALSE,
          out = "CH2_PS.3.text",
          column.labels   = c("RE","RE","FE","FE"))

#5,6 -> pdf




#Q2
#1.
is.pbalanced(id)
#false

id <- make.pbalanced(id, index = c("country", "year"))
is.pbalanced(id)


#2.
#(a)
summary(id$dem_ind)
quantile( id$dem_ind ,probs=seq(0,1,length=21), na.rm = TRUE, type=4)

#(b)
id_agg <- aggregate(id$dem_ind, list(id$country, id$year), FUN=mean)
us <- subset(id_agg, Group.1 == "United States")
mean(us$x)
libya <- subset(id_agg, Group.1 == "Libya")
mean(libya$x)
#(c) ########print 5개 나라 하기
dem <- aggregate(id$dem_ind, list(id$country), FUN=mean)

dem95 <- subset(dem, x>0.95)
sort_dem95 <- dem95[order(dem95$x),]
#Sweden, Luxembourg, Japan ,Costa Rica, France

dem10<- subset(dem, x < 0.1)
sort_dem10 <- dem10[order(dem10$x),]
# Korea, Dem. Rep, Iraq, Cuba, China, Afghanistan

dem37<- subset(dem, x>0.3 & x<0.7)
sort_dem37 <- dem37[order(dem37$x),]
# Paraguay, Jordan, Poland, Bulgaria, Hungary


#3.
#(a) coef 구하기
re_q2 <- plm(dem_ind~log_gdppc, model = "random", index = c("country", "year"), data = id)
summary(re_q2)
coef_result <- coeftest(re_q2,vcovHC(re_q2,type="HC0",cluster="group")) 


#(b) confidence interval is..
beta_hat <- coef_result["log_gdppc", "Estimate"]
se_beta_hat <- coef_result["log_gdppc", "Std. Error"]
CI95l <- beta_hat +  qnorm(0.025, 0, 1) * se_beta_hat
CI95u <- beta_hat - qnorm(0.025, 0, 1) * se_beta_hat
confidence_interval <- c(CI95l, CI95u)
confidence_interval

#4.
#(a) (not code)
#(b) allowing for country fixed effects
fe_q2 <- plm(dem_ind~log_gdppc, model = "within", index = c("country", "year"), data = id)
summary(fe_q2)

CI95l <- fe_q2$coefficients["log_gdppc"] +  qnorm(0.025, 0, 1) *  0.020805 
CI95u <- fe_q2$coefficients["log_gdppc"] - qnorm(0.025, 0, 1) *  0.020805 
confidence_interval <- c(CI95l, CI95u)
confidence_interval

#(c) exclude Azerbaijan
azer_data <- subset(id, id$country != "Azerbaijan")
azer_fe_q2 <- plm(dem_ind~log_gdppc, model = "within", index = c("country", "year"), data = azer_data)
summary(azer_fe_q2)

#(d) +time fixed effects (not code)

#(e) country effect + time fixed effect
re_q2 <- plm(dem_ind~log_gdppc, model = "random", index = c("country", "year"), data = id)
coeftest(re_q2,vcovHC(re_q2,type="HC0",cluster="group")) 

phtest(fe_q2,re_q2)

fe2_q2 <- plm(dem_ind~log_gdppc+factor(year), model = "within", index = c("country", "year"), data = id)
summary(fe2_q2)

#(f)
add_fe <- plm(dem_ind~log_gdppc+educ+age_median, model = "within", index = c("country", "year"), data = id)
summary(add_fe)
add_re <- plm(dem_ind~log_gdppc+educ+age_median, model = "random", index = c("country", "year"), data = id)
summary(add_re)

phtest(add_fe, add_re)

add_fe2 <- plm(dem_ind~log_gdppc+educ+age_median+factor(year), model = "within", index = c("country", "year"), data = id)
summary(add_fe2)






