setwd("/Users/wongalex/Documents/hkbu/sem7/ISEM3035/project/")
dat <- read.csv("real_estate_db.csv", header = T)
attach(dat)

data_mortgage <- dat [,-c(1:3,4,5,7,11,12,24,25,34,35,39,40,44,45,49,50,54,55,69,70,74,75)]
colnames(data_mortgage)
data_mortgage2 <- data_mortgage[,-c(5,13,15:24,27,29,30,32,33,35,40:42,44:46,48,49,51,52,54)]
colnames(data_mortgage2)
row.has.na <- apply(data_mortgage2, 1, function(x){any(is.na(x))})
sum(row.has.na)
data_mortgage3 <- data_mortgage2[!row.has.na,]
colnames(data_mortgage3)

Georgia <-subset(data_mortgage3, state=="Georgia")
lm_Georgia<-lm(hc_mortgage_median~hc_median+hi_median+married+separated+male_age_median+female_age_median+hs_degree+married+separated+divorced ,data=Georgia)
summary(lm_Georgia)


statelist = c("Washington", "Colorado", "Nevada", "Arizona", "Utah")

z = {}
r2list = {}
for (i in statelist) {
  subsetdata = subset(data_mortgage3, state == i)
  lm_model<-lm(hc_mortgage_median~hc_median+hi_median+married+male_age_median+hs_degree+divorced ,data=subsetdata)
  x = summary(lm_model)$coef
  x1 = x[1,]
  z = rbind(z,x)
  r2 = summary(lm_model)$adj.r.squared
  r2list = rbind(r2list,r2)
}


Nevada <-subset(data_mortgage3, state=="Nevada")
library(leaps)
attach(Nevada)
neveda_summary<-regsubsets(hc_mortgage_median~., data=Nevada, nvmax=7)
summary(neveda_summary)


lm_Nevada<-lm(hc_mortgage_median~hc_median+hi_median+married+separated+male_age_median+female_age_median+hs_degree+married+separated+divorced ,data=Nevada)
summary(lm_Nevada)
Washington <-subset(data_mortgage3, state=="Washington")
lm_Washington<-lm(hc_mortgage_median~hc_median+hi_median+married+separated+male_age_median+female_age_median+hs_degree+married+separated+divorced ,data=Washington)
summary(lm_Washington)
Colorado <-subset(data_mortgage3, state=="Colorado")
lm_Colorado<-lm(hc_mortgage_median~hc_median+hi_median+married+separated+male_age_median+female_age_median+hs_degree+married+separated+divorced ,data=Colorado)
summary(lm_Colorado)
Arizona <-subset(data_mortgage3, state=="Arizona")
lm_Arizona<-lm(hc_mortgage_median~hc_median+hi_median+married+separated+male_age_median+female_age_median+hs_degree+married+separated+divorced ,data=Arizona)
summary(lm_Arizona)
Utah <-subset(data_mortgage3, state=="Utah")
lm_Utah<-lm(hc_mortgage_median~hc_median+hi_median+married+separated+male_age_median+female_age_median+hs_degree+married+separated+divorced ,data=Utah)
summary(lm_Utah)
par( mfrow = c( 2, 2 ) )

par( mfrow = c( 2, 2 ) )
plot(lm_Washington)
plot(lm_Colorado)
plot(lm_Nevada)
plot(lm_Arizona)
plot(lm_Utah)

par( mfrow = c( 2, 3 ) )
attach(Utah)
plot(hc_mortgage_median~married)
plot(hc_mortgage_median~divorced)
plot(hc_mortgage_median~hs_degree)
plot(hc_mortgage_median~male_age_median)
plot(hc_mortgage_median~hc_median)
plot(hc_mortgage_median~hi_median)
