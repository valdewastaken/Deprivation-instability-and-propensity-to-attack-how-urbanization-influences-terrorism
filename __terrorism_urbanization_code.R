####DISCLAMIER####

##Variables in the regressions and descriptive statistics tables were renamed manually in the LaTeX
##in the paper. Also, regression summaries produced by esttex() command from the fixest package were
##modified manually for better visual appearance.

####install and load  required libraries####

install.packages("tidyr")
inctall.packages("dplyr")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("plm")
install.packages("fixest")
install.packages("gplots")
install.packages("dmm")

library(tidyr)
library(dplyr)
library(fixest)
library(plm)
library(gplots)
library(stargazer)
library(ggplot2)
library(zoo)
library(lmtest)
library(dmm)

####load data####

data <- read.csv("ter_urb_data.csv")

####vigintiles plot -- figure 1 in the paper####

plotmeans(data$n_terror_attack_normalized~data$urb_pop_vigintile,
          xlab = "Urbanization vigintile", ylab = "Number of terror attacks",
          barcol = "gray60")

####create new variables####

data$occupied<-factor(ifelse(data$p_polity == (-66)|data$p_polity == (-77), yes= 1, no = 0))

data <- dplyr::filter(data,data$c_names != "Holy See (Vatican City State)")

data <- pdata.frame(data, index=c("c_names", "year"))

data$movavg_urb_10 <- ave(data$share_urban_population_UN, data$c_names, 
                          FUN = function(x) rollmean(x, k=10, align="right", na.pad=T))

data$movavg_urb_5 <- ave(data$share_urban_population_UN, data$c_names, 
                         FUN = function(x) rollmean(x, k=5, align="right", na.pad=T))

data$share_urb_pop_lag <- plm::lag(data$share_urban_population_UN, 1)

data$delta_share_urb_pop <- data$share_urban_population_UN - data$share_urb_pop_lag


data$movavg_delta_urb_10 <- ave(data$delta_share_urb_pop, data$c_names, 
                                FUN = function(x) rollmean(x, k=10, align="right", na.pad=T))

data$movavg_delta_urb_5 <- ave(data$delta_share_urb_pop, data$c_names, 
                               FUN = function(x) rollmean(x, k=5, align="right", na.pad=T))

####descriptive statistics -- table 2 in the paper####

stargazer(data[,-c(4, 17, 18, 21)])

####model on subsamples -- table 3 in the paper####

modbasefs <- fenegbin(n_terror_attack~share_urban_population_UN+
                        log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                        occupied+epr_discriminated+as.factor(war)|c_names+factor(year), 
                      data=data)

modbase19 <- fenegbin(n_terror_attack~share_urban_population_UN+
                        log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                        occupied+epr_discriminated+as.factor(war)|c_names+factor(year), 
                      data=dplyr::filter(data, urb_pop_vigintile < 10))

modbase1014 <- fenegbin(n_terror_attack~share_urban_population_UN+
                          log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                          occupied+epr_discriminated+as.factor(war)|c_names+factor(year), 
                      data=dplyr::filter(data, urb_pop_vigintile < 15 & urb_pop_vigintile > 9))

modbase1520 <- fenegbin(n_terror_attack~share_urban_population_UN+
                          log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                          occupied+epr_discriminated+as.factor(war)|c_names+factor(year), 
                      data=dplyr::filter(data, urb_pop_vigintile > 14))


vcovlist <- list(vcov(modbasefs, se="hetero"),
                 vcov(modbasefs, se="hetero"),
                 vcov(modbase1014, se="hetero"),
                 vcov(modbase1520, se="hetero"))


esttex(modbasefs, modbase19, modbase1014, modbase1520, se="hetero", vcov=vcovlist)

####h1.1 -- table 4 in the paper####

mod0s <- fenegbin(n_terror_attack~share_urban_population_UN+
                    log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                    occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                  data=data)
summary(mod0s)

mod0s5 <- fenegbin(n_terror_attack~movavg_urb_5+
                     log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                     occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                   data=data)
summary(mod0s5)

mod0s10 <- fenegbin(n_terror_attack~movavg_urb_10+
                      log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                      occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                    data=data)
summary(mod0s10)

mod0sk <- fenegbin(nkill~share_urban_population_UN+
                     log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                     occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                   data=data)
summary(mod0sk)

mod0s5k <- fenegbin(nkill~movavg_urb_5+
                      log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                      occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                    data=data)
summary(mod0s5k)

mod0s10k <- fenegbin(nkill~movavg_urb_10+
                       log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                       occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                     data=data)
summary(mod0s10k)


vcovlist <- list(vcov(mod0s, se="hetero"),
                 vcov(mod0s5, se="hetero"),
                 vcov(mod0s10, se="hetero"),
                 vcov(mod0sk, se="hetero"),
                 vcov(mod0s5k, se="hetero"),
                 vcov(mod0s10k, se="hetero")
)


esttex(mod0s, mod0s5, mod0s10, mod0sk, mod0s5k, mod0s10k, se="hetero", vcov=vcovlist)



####h1.2 -- table 5 in the paper####

mod0s <- fenegbin(n_terror_attack~share_urban_population_UN+
                    share_urban_population_UN^2+
                    log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                    occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                  data=data)
summary(mod0s)


mod0s5 <- fenegbin(n_terror_attack~movavg_urb_5+
                     movavg_urb_5^2+
                     log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                     occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                   data=data)
summary(mod0s5)

mod0s10 <- fenegbin(n_terror_attack~movavg_urb_10+
                      movavg_urb_10^2+
                      log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                      occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                    data=data)
summary(mod0s10)


mod0sk <- fenegbin(nkill~share_urban_population_UN+
                     share_urban_population_UN^2+
                     log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                     occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                   data=data)
summary(mod0sk)

mod0s5k <- fenegbin(nkill~movavg_urb_5+movavg_urb_5^2+
                      log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                      occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                    data=data)
summary(mod0s5k)

mod0s10k <- fenegbin(nkill~movavg_urb_10+movavg_urb_10^2+
                       log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                       occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                     data=data)
summary(mod0s10k)

vcovlist <- list(vcov(mod0s, se="hetero"),
                 vcov(mod0s5, se="hetero"),
                 vcov(mod0s10, se="hetero"),
                 vcov(mod0sk, se="hetero"),
                 vcov(mod0s5k, se="hetero"),
                 vcov(mod0s10k, se="hetero")
)


esttex(mod0s, mod0s5, mod0s10, mod0sk, mod0s5k, mod0s10k, se="hetero", vcov=vcovlist)

####h2.1 table 6####

moddsn <- fenegbin(n_terror_attack~delta_share_urb_pop+
                     share_urban_population_UN+
                     log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                     occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                   data=data)
summary(moddsn)

modds5n <- fenegbin(n_terror_attack~movavg_delta_urb_5+movavg_urb_5+
                      log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                      occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                    data=data)
summary(modds5n)

modds10n <- fenegbin(n_terror_attack~movavg_delta_urb_10+movavg_urb_10+
                       log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                       occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                     data=data)
summary(modds10n)

moddskn <- fenegbin(nkill~delta_share_urb_pop+share_urban_population_UN+
                      log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                      occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                    data=data)
summary(moddskn)

modds5kn <- fenegbin(nkill~movavg_delta_urb_5+movavg_urb_5+
                       log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                       occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                     data=data)
summary(modds5kn)

modds10kn <- fenegbin(nkill~movavg_delta_urb_10+movavg_urb_10+
                        log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                        occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                      data=data)
summary(modds10kn)

vcovlist <- list(vcov(moddsn, se="hetero"),
                 vcov(modds5n, se="hetero"),
                 vcov(modds10n, se="hetero"),
                 vcov(moddskn, se="hetero"),
                 vcov(modds5kn, se="hetero"),
                 vcov(modds10kn, se="hetero")
)


esttex(moddsn, modds5n, modds10n, moddskn, modds5kn, modds10kn, se="hetero", vcov=vcovlist)


####h2.2 -- table 7####

moddsni <- fenegbin(n_terror_attack~delta_share_urb_pop+
                      share_urban_population_UN+
                      delta_share_urb_pop:share_urban_population_UN+
                      log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                      occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                    data=data)
summary(moddsni)

modds5ni <- fenegbin(n_terror_attack~movavg_delta_urb_5+movavg_urb_5+
                       movavg_delta_urb_5:movavg_urb_5+
                       log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                       occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                     data=data)
summary(modds5ni)

modds10ni <- fenegbin(n_terror_attack~movavg_delta_urb_10+movavg_urb_10+
                        movavg_delta_urb_10:movavg_urb_10+
                        log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                        occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                      data=data)
summary(modds10ni)

moddskni <- fenegbin(nkill~delta_share_urb_pop+share_urban_population_UN+
                       delta_share_urb_pop:share_urban_population_UN+
                       log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                       occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                     data=data)
summary(moddskni)

modds5kni <- fenegbin(nkill~movavg_delta_urb_5+movavg_urb_5+
                        movavg_delta_urb_5:movavg_urb_5+
                        log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                        occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                      data=data)
summary(modds5kni)

modds10kni <- fenegbin(nkill~movavg_delta_urb_10+movavg_urb_10+
                         movavg_delta_urb_10:movavg_urb_10+
                         log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                         occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                       data=data)
summary(modds10kni)

vcovlist <- list(vcov(moddsni, se="hetero"),
                 vcov(modds5ni, se="hetero"),
                 vcov(modds10ni, se="hetero"),
                 vcov(moddskni, se="hetero"),
                 vcov(modds5kni, se="hetero"),
                 vcov(modds10kni, se="hetero"))


esttex(moddsni, modds5ni, modds10ni, moddskni, modds5kni,modds10kni, se="hetero", vcov=vcovlist)

####plots####

datas <- dplyr::select(data, c_names, year, n_terror_attack, share_urban_population_UN,
                       delta_share_urb_pop,
                       population_UN, gdp_per_capita_PPP_WB, p_polity_2_2,
                       occupied, epr_discriminated, war)

datas <- na.omit(datas)

####plot 2 in the paper####

mod00s <- fenegbin(n_terror_attack~share_urban_population_UN+
                     share_urban_population_UN^2+
                     log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                     occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                   data=datas)
summary(mod00s)

mod00s$obsRemoved
mod00s$fixef_removed

datas <- dplyr::filter(datas, c_names != "Mongolia" & c_names != "Oman" &
                         year != 1993)

datas$predicted <- mod00s$fitted.values

datass <- data.frame(datas)

datass$predicted_norm <- datass$predicted/datass$population_UN
ggplot(data = datass, aes(x=share_urban_population_UN, y=predicted_norm))+
  geom_point(color="gray60")+
  geom_smooth(color="red", fill="lightblue", method="loess")+
  ylim(c(0,0.001))+
  ylab("Fitted values of terror attacks normalized on total population")+
  xlab("Proportion of urban population")+
  theme_bw()

####plot 3 in the paper####

moddsni0 <- fenegbin(n_terror_attack~delta_share_urb_pop+
                       share_urban_population_UN+
                       delta_share_urb_pop:share_urban_population_UN+
                       log(population_UN)+log(gdp_per_capita_PPP_WB)+ p_polity_2_2+
                       occupied+epr_discriminated+as.factor(war)|c_names+factor(year),
                     data=datas)
summary(moddsni0)

moddsni0$fixef_removed

datas$pred <- moddsni0$fitted.values

datass <- data.frame(datas)

datass$pred_norm <- datass$pred/datass$population_UN

trend_change <- -moddsni0$coeftable[1,1]/moddsni0$coeftable[9,1]

datass$trend <- (datass$share_urban_population_UN > trend_change)

datass$trend <- recode_factor(as.factor(datass$trend),
                              'TRUE' = "Share urban population > 63.8",
                              'FALSE' = "Share urban population <= 63.8")

ggplot2::ggplot(data=datass, aes(x=delta_share_urb_pop,y=pred_norm))+
  geom_point(col="gray60")+
  geom_smooth(method = "loess",color="red", fill="lightblue")+
  ylim(c(0,0.0025))+
  xlim(c(-0.25,1.5))+
  facet_wrap(vars(factor(trend, levels=c("Share urban population <= 63.8",
                                         "Share urban population > 63.8"))))+
  ylab("Fitted values of terror attacks normalized on total population")+
  xlab("Growth of proportion of urban population")+
  theme_bw()

####robusntess####

#########


#Proper robust check with delta urb

panel1 <- data
panel1<-drop_na(panel1,n_terror_attack,nkill,gdp_per_capita_PPP_WB,fh_status_sum,population_UN,occupied,war,share_urban_population_UN,movavg_urb_5,movavg_urb_10)

#####
#alternative estimation of delta effect with quassi poisson models with delta


model1<-glm(n_terror_attack~share_urban_population_UN+I(share_urban_population_UN^2) +log(gdp_per_capita_PPP_WB)+fh_status_sum+log(population_UN)+occupied+war+epr_discriminated+factor(c_names)+factor(year),data=panel1,family = "quasipoisson")
cov1<-vcovHC(model1,type="HC0")
robust.se1<-sqrt(diag(cov1))
model2<-glm(n_terror_attack~movavg_urb_5+I(movavg_urb_5^2) +log(gdp_per_capita_PPP_WB)+fh_status_sum+log(population_UN)+occupied+war+epr_discriminated+factor(c_names)+factor(year),data=panel1,family = "quasipoisson")
summary(model2)
cov2<-vcovHC(model2,type="HC0")
robust.se2<-sqrt(diag(cov2))
model3<-glm(n_terror_attack~movavg_urb_10+I(movavg_urb_10^2) +log(gdp_per_capita_PPP_WB)+fh_status_sum+log(population_UN)+occupied+war+epr_discriminated+factor(c_names)+factor(year),data=panel1,family = "quasipoisson")

cov3<-vcovHC(model3,type="HC0")
robust.se3<-sqrt(diag(cov3))

model4<-glm(nkill~share_urban_population_UN+I(share_urban_population_UN^2) +log(gdp_per_capita_PPP_WB)+fh_status_sum+log(population_UN)+occupied+war+epr_discriminated+factor(c_names)+factor(year),data=panel1,family = "quasipoisson")
cov4<-vcovHC(model4,type="HC0")
robust.se4<-sqrt(diag(cov4))
model5<-glm(nkill~movavg_urb_5+I(movavg_urb_5^2) +log(gdp_per_capita_PPP_WB)+fh_status_sum+log(population_UN)+occupied+war+epr_discriminated+factor(c_names)+factor(year),data=panel1,family = "quasipoisson")
cov5<-vcovHC(model5,type="HC0")
robust.se5<-sqrt(diag(cov5))
model6<-glm(nkill~movavg_urb_10+I(movavg_urb_10^2) +log(gdp_per_capita_PPP_WB)+fh_status_sum+log(population_UN)+occupied+war+epr_discriminated+factor(c_names)+factor(year),data=panel1,family = "quasipoisson")
cov6<-vcovHC(model6,type="HC0")
robust.se6<-sqrt(diag(cov6))




stargazer(model1,model2,model3,model4,model5,model6, se=list(robust.se1,robust.se2,robust.se3, robust.se4, robust.se5, robust.se6), single.row = F,
          keep = c(1,2,3,4,5,6,7),t.auto = T,  model.numbers = T,
          title = "Alternative estimation with quassi-poisson distribution (models with Proportion of Urban population",
          covariate.labels = c("Propotion of Urban population" , "Propotion of Urban population^2", "Moving Average of urban population proportion (5-year)", "Moving Average of urban population proportion (5-year)^2","Moving Average of urban population proportion (10-year)","Moving Average of urban population proportion (10-year)^2"),
          dep.var.labels = c("Terrorist attacks", " Terrorist Killings"),font.size = "small",column.sep.width = "-10pt", notes = "Robust standard errors in parentness")

model1_g<-glm(n_terror_attack~ share_urban_population_UN*delta_share_urb_pop+log(gdp_per_capita_PPP_WB)+fh_status_sum+log(population_UN)+occupied+war+epr_discriminated+factor(c_names)+factor(year),data=panel1,family = "quasipoisson")
cov1_g<-vcovHC(model1_g,type="HC0")
robust.se1_g<-sqrt(diag(cov1_g))
summary(model1_g)
model2_g<-glm(n_terror_attack~movavg_urb_5*movavg_delta_urb_5+log(population_UN)+log(gdp_per_capita_PPP_WB)+fh_status_sum+occupied+war+epr_discriminated+factor(c_names)+factor(year),data=panel1,family = "quasipoisson")

cov2_g<-vcovHC(model2_g,type="HC0")
robust.se2_g<-sqrt(diag(cov2_g))

model3_g<-glm(n_terror_attack~movavg_urb_10*movavg_delta_urb_10+log(population_UN)+log(gdp_per_capita_PPP_WB)+fh_status_sum+occupied+war+epr_discriminated+factor(c_names)+factor(year),data=panel1,family = "quasipoisson")
cov3_g<-vcovHC(model3_g,type="HC0")
robust.se3_g<-sqrt(diag(cov3_g))

model4_g<-glm(nkill~share_urban_population_UN*delta_share_urb_pop+log(population_UN)+log(gdp_per_capita_PPP_WB)+fh_status_sum+occupied+war+epr_discriminated+factor(c_names)+factor(year),data=panel1,family = "quasipoisson")

cov4_g<-vcovHC(model4_g,type="HC0")
robust.se4_g<-sqrt(diag(cov4_g))
model5_g<-glm(nkill~movavg_urb_5*movavg_delta_urb_5+log(population_UN)+log(gdp_per_capita_PPP_WB)+fh_status_sum+occupied+war+epr_discriminated+factor(c_names)+factor(year),data=panel1,family = "quasipoisson")
cov5_g<-vcovHC(model5_g,type="HC0")

robust.se5_g<-sqrt(diag(cov5_g))

model6_g<-glm(nkill~movavg_urb_10*movavg_delta_urb_10+log(population_UN)+log(gdp_per_capita_PPP_WB)+fh_status_sum+occupied+war+epr_discriminated+factor(c_names)+factor(year),data=panel1,family = "quasipoisson")
cov6_g<-vcovHC(model6_g,type="HC0")
robust.se6_g<-sqrt(diag(cov6_g))

##there is an issue with stargazer here, as it does not show the coefficient before the
##interaction term in the model1_g, it was included manually in the LaTeX

stargazer(model1_g,model2_g,model3_g,model4_g,model5_g,model6_g, se=list(robust.se1_g,robust.se2_g,robust.se3_g,robust.se4_g,robust.se5_g,robust.se6_g), single.row = F,
          keep = c(1,2,3,4,5,6, 7,8, 194,195,196,197),t.auto = T,  
          model.numbers = T,
          title = "Alternative estimation with quassi-poisson distribution (models with Interactions)",
          dep.var.labels = c("Terrorist attacks", " Terrorist Killings"),font.size = "small",column.sep.width = "-10pt", notes = "Robust standard errors in parentness")

summary(model1_g)
stargazer(model1_g,se=list(robust.se1_g))
####
#negbin models with additional controls
library(fixest)
panel1$loggdp<-log(panel1$gdp_per_capita_PPP_WB)
panel1$logref<-log(panel1$SM.POP.REFG)
panel1$shares_squared<-(panel1$share_urban_population_UN)^2
panel1$logpop<-log(panel1$population_UN)
mod1<- fenegbin(n_terror_attack~share_urban_population_UN+shares_squared+logpop+loggdp+fh_status_sum+occupied+war+epr_discriminated+state_capacity+logref+femaleLF|c_names+year,data=panel1)
sev1<-se(mod1,se="white")[1:11]

mod2<- fenegbin(nkill~share_urban_population_UN+shares_squared+logpop+loggdp+fh_status_sum+occupied+war+epr_discriminated+state_capacity+logref+femaleLF|c_names+year,data=panel1)
sev2<-se(mod2,se="white")[1:11]

esttex(mod1,mod2,se="hetero",
       title="Negative binomial regression with additional controls (Models with Proportion of Urban population)",
       dict =c(share_urban_population_UN="Proportion of urban population",shares_squared = "Propotion of Urban population^2",occupied = "Occupied country",
               war = "War",epr_discriminated = "Proportion of discriminated population",
               loggdp="Log of GDP per Capita (PPP)",logpop="Log of population",
               femaleLF="Female labor force participation",state_capacity="State capacity",fh_status_sum="Freedom house index of democracy",
               logref="Log of number of refugees (by territory of asylum)",n_terror_attack="Number of terrorist attacks", nkill = "Terrorist kills",c_names="Countries",year="Time"),
       coefstat = "se")

mod3<- fenegbin(n_terror_attack~share_urban_population_UN*delta_share_urb_pop+logpop+loggdp+fh_status_sum+occupied+war+epr_discriminated+state_capacity+logref+femaleLF|c_names+year,data=panel1)

sev3<-se(mod3,se="white")[1:11]
mod4<- fenegbin(nkill~share_urban_population_UN*delta_share_urb_pop+logpop+loggdp+fh_status_sum+occupied+war+epr_discriminated+state_capacity+logref+femaleLF|c_names+year,data=panel1)
sev4<-se(mod4,se="white")[1:11]
summary(mod4)
esttex(mod3,mod4,se="hetero",
       title="Negative binomial regression with additional controls (Models with Interactions)",
       dict =c(share_urban_population_UN="Proportion of urban population",delta_share_urb_pop="Growth of Urban population (proportion)",occupied = "Occupied country",
               war = "War",epr_discriminated = "Proportion of discriminated population",
               loggdp="Log of GDP per Capita (PPP)",logpop="Log of population",
               femaleLF="Female labor force participation",state_capacity="State capacity",fh_status_sum="Freedom house index of democracy",
               logref="Log of number of refugees (by territory of asylum)",n_terror_attack="Number of terrorist attacks", nkill = "Terrorist kills",c_names="Countries",year="Time"),
       coefstat = "se")

