setwd("C:\\Users\\14223570\\Documents\\ISEM_R")
dat <- read.csv("real_estate_db.csv", header = T)
attach(dat)

dat1 <- dat[,-2]

row.has.na <- apply(dat1, 1, function(x){any(is.na(x))})
sum(row.has.na)
final.filtered <- dat1[!row.has.na,]

summary(final.filtered)

cor(final.filtered[,-c(1,5,6,7,8,9,10,11,12)])
final.filtered2 <- final.filtered[,-c1]

plot(final.filtered2)

c1 <- seq(19)
c(1,5,6,7,8,9,10,11,12)