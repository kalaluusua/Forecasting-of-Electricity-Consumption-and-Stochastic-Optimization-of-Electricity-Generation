
# install.packages("TSA")
library(TSA)
library(cowplot)
library(forecast)
library(gridGraphics)

par(xpd = NA, # switch off clipping, necessary to always see axis labels
    bg = "transparent", # switch off background to avoid obscuring adjacent plots
    oma = c(2, 2, 0, 0), # move plot to the right and up
    mgp = c(2, 1, 0) # move axis labels closer to axis
) 

# (0,1,1)S_w(0,1,1)S(0,1,1) X temp
# toteutus 1: (0,1,1)S_w(0,1,0)S(0,1,1) X temp
# toteutus 2: (0,1,0)S_w(0,1,1)S(0,1,1) X temp
# differenssi: (0,1,0)S_w(0,1,0)S(0,1,0) X temp
# auto: (0,1,0)S_w(1,1,0)S(2,1,0) X temp
# (0,1,1)S_w(0,1,1) X [temp, kulutus vuorokausi sitten]

# Luetaan sähkönkulutus- ja lämpötiladata, hypätään headerrivin yli
eletemp = read.table(file = "sahko.csv",
                     sep = ";",
                     dec = ",",
                     skip = 1,
                     col.names = c('kWh','Celcius'))

a = 24*7# ex post tunteina (oltava n*24)

lv = 7*24 # acf lag.max

# Määrää lämpötilan mahdollinen viive L
L = 12

# Sähkönkulutus aikasarjaksi
ele = ts(eletemp$kWh[1:(816-a)], start = 1, frequency = 24)
ele24 = ts(eletemp$kWh[(817-a):(840-a)], start = c(35-a/24,1), frequency = 24)

ele = ele[-(1:L)]

# Lämpötila kahdeksi aikasarjaksi: 816 ensimmäistä havaintoa käytetään mallin estimointiin
# ja 24 viimeistä havaintoa ennustamiseen.
temp = ts(eletemp$Celcius, start = 1, frequency = 24)

# Stationarisoidaan aikasarjat. Määrittele parametrit d,S,D
# Huomaa, että sähkönkulutuksen ja lämpötilan aikasarjojen differointien asteiden ei välttämättä tarvitse olla samoja.
S = 24 # Kausidifferoinnin jakso S
S_w = 7*24 # Kausidifferoinnin jakso S_w
de = 1 # Differoinnin kertaluku de
D = 1 # Kausidifferensoinnin kertaluku D
D_w = 1 # Kausidifferensoinnin kertaluku D_w
dt = 1 # Differoinnin kertaluku d

par(mfrow=c(2,2))

# toteutus 1: (0,1,1)S_w(0,1,0)S(0,1,1) X temp
p = 0
q = 1
P = 0
Q = 1

dele1 = ele
dele1 = diff(ele, lag = S, differences = D)
rmv1 = length(ele)-length(dele1)
xreg = temp[(1+rmv1):(816-L-a)]

malli = arima(dele1,
              order = c(p,de,q),
              seasonal = list(order = c(P, D_w, Q),period = S_w),
              xreg = xreg,
              method = "CSS")

ahead = 24
enne = predict(malli, n.ahead = ahead, newxreg = temp[(816-L+1-a):(816-L-a+24)])

#Integroidaan for-loopilla ennuste ja luottamusvälit kausivaihtelun pituudella L1
ennuste = c(1:ahead)
clyla = c(1:ahead)
clala = c(1:ahead)
for (x in c(1:ahead)) {
  ennuste[x] = ele[816-a-L-ahead+x] + enne$pred[x]
  clyla[x] = ennuste[x] + 1.96*enne$se[x]
  clala[x] = ennuste[x] - 1.96*enne$se[x]
}

par(mfrow=c(1,1))
#Tehdään ennusteesta aikasarja ja plotataan
ennuste = ts(ennuste, start = c(35-a/24,1), frequency = 24)
clyla = ts(clyla, start = c(35-a/24,1), frequency = 24)
clala = ts(clala, start = c(35-a/24,1), frequency = 24)
# ts.plot(ennuste, col = c("red", "black", "red","blue"))

par(mfrow=c(1,1))

deledf = data.frame(kWh=as.matrix(dele1), date=time(dele1))
mallidf = data.frame(kWh=as.matrix(dele1-malli$residuals), date=time(dele1))

ennustedf = data.frame(kWh=as.matrix(ennuste), date=time(ele24))
ele24df = data.frame(kWh=as.matrix(ele24), date=time(ele24))
clyladf = data.frame(kWh=as.matrix(clyla), date=time(clyla))
claladf = data.frame(kWh=as.matrix(clala), date=time(clala))

pm1e <- ggplot(mapping = aes(x=date, y=kWh)) +
  geom_line(data = ennustedf, aes(x = date, y = kWh), color = "blue") +
  geom_line(data = clyladf, aes(x = date, y = kWh), linetype = 2, color = "blue") +
  geom_line(data = claladf, aes(x = date, y = kWh), linetype = 2, color = "blue") +
  geom_line(data = ele24df, aes(x = date, y = kWh)) +
  labs(y="d. kulutus/kWh", x="aika/h", title="Malli 1")


r2e_1 = cor(ennuste,ele24)^2
r2_1 = cor(fitted(malli),dele1)^2

par(mfrow=c(1,1))

# Poistetaan edestä nollat
x = malli$res
res = x[ min( which ( x != 0 )) : max( which( x != 0 )) ]
res1 = res
rese1 = ennuste-ele24
# plot(res)
acf_res1 <- autoplot(acf(res, lag.max = lv, ci.type="ma", plot = FALSE)) + geom_hline(aes(yintercept = 0)) + labs (title="Malli 1")
pacf_res1 <- autoplot(acf(res, lag.max= lv, type = "partial", plot = FALSE), ylab = "PACF") + geom_hline(aes(yintercept = 0))

plot_grid(acf_res1, pacf_res1, nrow = 2)

par(mfrow=c(1,1))
library(MASS)
sres = res

h <- hist(sres,
     xlab="Residuaali",ylab="Frekvenssi", main = "")
xfit <- seq(min(sres), max(sres), length = 40) 
yfit <- dnorm(xfit, mean = mean(sres), sd = sd(sres))
yfit <- yfit * diff(h$mids[1:2]) * length(sres) 

lines(xfit, yfit, col = "blue", lty = 2, lwd = 1)

hist1 <- recordPlot()

qqnorm(res, pch = ".", cex = 2, main = "", xlab = "Teoreettiset kvantiilit", ylab = "Otoskvantiilit")

qq1 <- recordPlot()

malli1_bl <- rep(NA,49)
k <- p + q + P + Q

for (i in 1:49)
{
  
  malli1_bl[i]=Box.test(res,lag=(i+k),fitdf=k,
                       type="Ljung-Box")$p.value
}
# malli1_bl
round(malli1_bl,3)

#aic

# # Kirjoitetaan ennuste ja luottamusvälit .csv-tiedostoon, jonka voi avata Excelillä.
# output = cbind(ennuste,
#                clyla,
#                clala)
# write.csv2(output, file = 'ennuste.csv')



# toteutus 2: (0,1,0)S_w(0,1,1)S(0,1,1) X temp

p = 0
q = 1
P = 0
Q = 1

dele1 = diff(ele, lag = S_w, differences = D_w)
rmv1 = length(ele)-length(dele1)
xreg = temp[(1+rmv1):(816-L-a)]

malli = arima(dele1,
              order = c(p,de,q),
              seasonal = list(order = c(P, D, Q),period = S),
              xreg = xreg,
              method = "CSS")

ahead = 24
enne = predict(malli, n.ahead = ahead, newxreg = temp[(816-L+1-a):(816-L-a+24)])

#Integroidaan for-loopilla ennuste ja luottamusvälit kausivaihtelun pituudella L1
ennuste = c(1:ahead)
clyla = c(1:ahead)
clala = c(1:ahead)
for (x in c(1:ahead)) {
  ennuste[x] = ele[816-a-L-ahead+x] + enne$pred[x]
  clyla[x] = ennuste[x] + 1.96*enne$se[x]
  clala[x] = ennuste[x] - 1.96*enne$se[x]
}

par(mfrow=c(1,1))
#Tehdään ennusteesta aikasarja ja plotataan
ennuste = ts(ennuste, start = c(35-a/24,1), frequency = 24)
clyla = ts(clyla, start = c(35-a/24,1), frequency = 24)
clala = ts(clala, start = c(35-a/24,1), frequency = 24)
# ts.plot(ennuste, col = c("red", "black", "red","blue"))

par(mfrow=c(1,1))

deledf = data.frame(kWh=as.matrix(dele1), date=time(dele1))
mallidf = data.frame(kWh=as.matrix(dele1-malli$residuals), date=time(dele1))
pm2 <- ggplot(mapping = aes(x=date, y=kWh)) +
  geom_line(data = deledf, aes(x = date, y = kWh), color = "red") +
  geom_line(data = mallidf, aes(x = date, y = kWh)) +
  labs(y="d. kulutus/kWh", x="aika/h", title="Malli 2")
pm2

ts.plot(ele24, ennuste, clyla, clala, xlab = "aika/vrk", col = c("red", "blue", "blue", "blue"))

r2e_2 = cor(ennuste,ele24)^2
r2_2 = cor(fitted(malli),dele1)^2

par(mfrow=c(1,2))

# Poistetaan edestä nollat
x = malli$res
res = x[ min( which ( x != 0 )) : max( which( x != 0 )) ]
res2 = res
rese2 = ennuste-ele24
# plot(res)
acf_res2 <- autoplot(acf(res, lag.max = lv, ci.type="ma", plot = FALSE)) + geom_hline(aes(yintercept = 0)) + labs (title="Malli 2")
pacf_res2 <- autoplot(acf(res, lag.max= lv, type = "partial", plot = FALSE), ylab = "PACF") + geom_hline(aes(yintercept = 0))

plot_grid(acf_res2, pacf_res2, nrow = 2)

par(mfrow=c(1,1))
library(MASS)
sres = res
h <- hist(sres,
          xlab="Residuaali",ylab="Frekvenssi", main = "")
xfit <- seq(min(sres), max(sres), length = 40) 
yfit <- dnorm(xfit, mean = mean(sres), sd = sd(sres))
yfit <- yfit * diff(h$mids[1:2]) * length(sres)  

lines(xfit, yfit, col = "blue", lty = 2, lwd = 1)


hist2<- recordPlot()

qqnorm(res, main = "", pch = ".", cex = 2, xlab = "Teoreettiset kvantiilit", ylab = "Otoskvantiilit")

qq2 <- recordPlot()

malli2_bl <- rep(NA,49)
k <- p + q + P + Q

for (i in 1:49)
{
  
  malli2_bl[i]=Box.test(res,lag=(i+k),fitdf=k,
                       type="Ljung-Box")$p.value
}
# malli2_bl
round(malli2_bl,3)

#aic




# auto: (0,1,0)S_w(1,1,0)S(2,1,0) X temp

p = 2
q = 0
P = 1
Q = 0

dele1 = diff(ele, lag = S, differences = D)
rmv1 = length(ele)-length(dele1)
xreg = temp[(1+rmv1):(816-L-a)]

malli = arima(dele1,
              order = c(p,de,q),
              seasonal = list(order = c(P, D_w, Q),period = S_w),
              xreg = xreg,
              method = "CSS")

ahead = 24
enne = predict(malli, n.ahead = ahead, newxreg = temp[(816-L+1-a):(816-L-a+24)])

#Integroidaan for-loopilla ennuste ja luottamusvälit kausivaihtelun pituudella L1
ennuste = c(1:ahead)
clyla = c(1:ahead)
clala = c(1:ahead)
for (x in c(1:ahead)) {
  ennuste[x] = ele[816-a-L-ahead+x] + enne$pred[x]
  clyla[x] = ennuste[x] + 1.96*enne$se[x]
  clala[x] = ennuste[x] - 1.96*enne$se[x]
}

par(mfrow=c(1,1))
#Tehdään ennusteesta aikasarja ja plotataan
ennuste = ts(ennuste, start = c(35-a/24,1), frequency = 24)
clyla = ts(clyla, start = c(35-a/24,1), frequency = 24)
clala = ts(clala, start = c(35-a/24,1), frequency = 24)
# ts.plot(ennuste, col = c("red", "black", "red","blue"))

par(mfrow=c(1,1))

deledf = data.frame(kWh=as.matrix(dele1), date=time(dele1))
mallidf = data.frame(kWh=as.matrix(dele1-malli$residuals), date=time(dele1))

pm3 <- ggplot(mapping = aes(x=date, y=kWh)) +
  geom_line(data = deledf, aes(x = date, y = kWh), color = "red") +
  geom_line(data = mallidf, aes(x = date, y = kWh)) +
  labs(y="d. kulutus/kWh", x="aika/h", title="Malli 3")

ts.plot(ele24, ennuste, clyla, clala, xlab = "aika/vrk", col = c("red", "blue", "blue", "blue"))

r2e_3 = cor(ennuste,ele24)^2
r2_3 = cor(fitted(malli),dele1)^2

par(mfrow=c(1,2))

# Poistetaan edestä nollat
x = malli$res
res = x[ min( which ( x != 0 )) : max( which( x != 0 )) ]
rese3 = ennuste-ele24
# plot(res)
acf_res3 <- autoplot(acf(res, lag.max = lv, ci.type="ma", plot = FALSE)) + geom_hline(aes(yintercept = 0)) + labs (title="Malli 3")
pacf_res3 <- autoplot(acf(res, lag.max= lv, type = "partial", plot = FALSE), ylab = "PACF") + geom_hline(aes(yintercept = 0))

plot_grid(acf_res3, pacf_res3, nrow = 2)

par(mfrow=c(1,1))
library(MASS)
sres = res
h <- hist(sres,
          xlab="Residuaali",ylab="Frekvenssi", main = "")
xfit <- seq(min(sres), max(sres), length = 40) 
yfit <- dnorm(xfit, mean = mean(sres), sd = sd(sres))
yfit <- yfit * diff(h$mids[1:2]) * length(sres) 

lines(xfit, yfit, col = "blue", lty = 2, lwd = 1)


hist3 <- recordPlot()

qqnorm(res, pch = ".", cex = 2, main = "", xlab = "Teoreettiset kvantiilit", ylab = "Otoskvantiilit")

qq3 <- recordPlot()

malli3_bl <- rep(NA,49)
k <- p + q + P + Q

for (i in 1:49)
{
  
  malli3_bl[i]=Box.test(res,lag=(i+k),fitdf=k,
                       type="Ljung-Box")$p.value
}
# malli3_bl
round(malli3_bl,3)

#aic




# (0,1,1)S_w(0,1,1) X [temp, kulutus vuorokausi sitten]

p = 0
q = 1
P = 0
Q = 1

rmv = S

# Estimoidaan malli lämpötilan kanssa.
eleestimointi = ele[-(1:rmv)]

dele24 = ts(ele, start = 1, frequency = 24)

# testi = cbind(dele24[1:(816-a-L-rmv)], tempestimointi)

# Selitetään aikasarjaa vuorokaudenajan (xreg_h) ja lämpötilan avulla
xreg = cbind(temp[(1):(840-a-L)][-(1:rmv)], dele24)
xestimointi = xreg[1:(816-a-L-rmv),]
xennuste = xreg[(816-a-L-rmv+1):(816-a-L-rmv+24),]

library(forecast)
# eleestimointi = ts(tempestimointi, start = 2, frequency = 24)
# dele24 = ts(eletemp$kWh[(1+L):(816-a)], start = 2, frequency = 24)
malli = arima(eleestimointi,
               order = c(p,de,q),
               seasonal = list(order = c(P, D_w, Q), period = S_w),
               xreg = xestimointi,
               include.mean=FALSE,
               method = "CSS")
enne = predict(malli,
                n.ahead = 24,
                newxreg = xennuste)

ennepr = ts(enne$pred, start = c(35-a/24,1), frequency = 24)
clyla = ts(enne$pred + 1.96*enne$se, start = c(35-a/24,1), frequency = 24)
clala = ts(enne$pred - 1.96*enne$se, start = c(35-a/24,1), frequency = 24)

# Kirjoitetaan ennuste ja luottamusvälit .csv-tiedostoon, jonka voi avata Excelillä.
# output = cbind(round(ennuste,0),
               # round(clyla,0),
               # round(clala,0))
# write.csv2(output, file = 'ennuste.csv')

zeros = length(ele)-length(malli$residuals)
fit = ele-append(rep(0,zeros),malli$residuals)
fit = ts(fit, start = 1, frequency = 24)

ele = ts(ele, start = 1, frequency = 24)
deledf = data.frame(kWh=as.matrix(ele), date=time(ele))
mallidf = data.frame(kWh=as.matrix(fit), date=time(fit))

pm4 <- ggplot(mapping = aes(x=date, y=kWh)) +
  geom_line(data = deledf, aes(x = date, y = kWh), color = "red") +
  geom_line(data = mallidf, aes(x = date, y = kWh)) +
  labs(y="kulutus/kWh", x="aika/vrk", title="Malli 4")
pm4

ele = ts(eletemp$kWh[1:(816-a)], start = 1, frequency = 24)
eledf = data.frame(kWh=as.matrix(ele), date=time(ele))
ennedf = data.frame(kWh=as.matrix(ennepr), date=time(ennepr))
clyladf = data.frame(kWh=as.matrix(clyla), date=time(clyla))
claladf = data.frame(kWh=as.matrix(clala), date=time(clala))

pe4 <- ggplot(mapping = aes(x=date, y=kWh)) +
  geom_line(data = eledf, aes(x = date, y = kWh), color = "black") +
  # geom_line(data = mallidf, aes(x = date, y = kWh)) +
  geom_line(data = ennedf, aes(x = date, y = kWh), color = "blue") +
  # geom_line(data = clyladf, aes(x = date, y = kWh), color = "blue") +
  # geom_line(data = claladf, aes(x = date, y = kWh), color = "blue") +
  labs(y="kulutus/kWh", x="aika/vrk")
pe4

pm4e <- ggplot(mapping = aes(x=date, y=kWh)) +
  # geom_line(data = eledf, aes(x = date, y = kWh), color = "black") +
  # geom_line(data = mallidf, aes(x = date, y = kWh)) +
  geom_line(data = ennedf, aes(x = date, y = kWh), color = "blue") +
  geom_line(data = clyladf, aes(x = date, y = kWh), linetype = 2, color = "blue") +
  geom_line(data = claladf, aes(x = date, y = kWh), linetype = 2, color = "blue") +
  geom_line(data = ele24df, aes(x = date, y = kWh)) +
  labs(y="kulutus/kWh", x="aika/vrk", title = "Malli 4")
pm4e

plot_grid(pe4, pe4ci, nrow = 2)

# Plotataan kuva pelkästä ennusteesta.
ts.plot(ennepr, ele24,
        clyla,
        clala,
        col = c("blue", "red", "blue", "blue"),
        main = "Malli 4", xlab = "aika/vrk")

r2e_4 = cor(ennuste,ele24)^2
r2_4 = cor(fitted(malli),dele1)^2

par(mfrow=c(1,2))

# Poistetaan edestä nollat
x = malli$res
res = x[ min( which ( x != 0 )) : max( which( x != 0 )) ]
res4 = res
rese4 = ennepr-ele24
# plot(res)
acf_res4 <- autoplot(acf(res, lag.max = lv, ci.type="ma", plot = FALSE)) + geom_hline(aes(yintercept = 0)) + labs(title="Malli 4")
pacf_res4 <- autoplot(acf(res, lag.max= lv, type = "partial", plot = FALSE), ylab = "PACF") + geom_hline(aes(yintercept = 0))

plot_grid(acf_res4, pacf_res4, nrow = 2)

par(mfrow=c(1,1))


library(MASS)
sres = res
h <- hist(sres,
          xlab="Residuaali",ylab="Frekvenssi", main = "")
xfit <- seq(min(sres), max(sres), length = 40) 
yfit <- dnorm(xfit, mean = mean(sres), sd = sd(sres))
yfit <- yfit * diff(h$mids[1:2]) * length(sres) 

lines(xfit, yfit, col = "blue", lty = 2, lwd = 1)

hist4 <- recordPlot()

qqnorm(res, pch = ".", cex = 2, main = "", xlab = "Teoreettiset kvantiilit", ylab = "Otoskvantiilit")

qq4 <- recordPlot()

malli4_bl <- rep(NA,49)
k <- p + q + P + Q

for (i in 1:49)
{
  
  malli4_bl[i]=Box.test(res,lag=(i+k),fitdf=k,
                       type="Ljung-Box")$p.value
}
# malli4_bl
round(malli4_bl,3)

#aic

r2 <- c(r2_1, r2_2, r2_3, r2_4)
# c(r2e_1, r2e_2, r2e_3, r2e_4, r2e_5)
# (c(mean(res1), mean(res2), mean(res3), mean(res4), mean(res5)))
absmeanres_pred <- round(c(sqrt(sum(rese1^2)), sqrt(sum(rese2^2)), sqrt(sum(rese3^2)), sqrt(sum(rese4^2))),0)

# write.csv2(r2, file = 'r2.txt')
write.csv2(absmeanres_pred, file = paste('distL2_pred',toString(a),'.txt', sep = ""))

bl1 <- matrix(round(malli1_bl,3), byrow = TRUE, nrow = 7,ncol = 7)
bl2 <- matrix(round(malli2_bl,3), byrow = TRUE, nrow = 7,ncol = 7)
bl3 <- matrix(round(malli3_bl,3), byrow = TRUE, nrow = 7,ncol = 7)
bl4 <- matrix(round(malli4_bl,3), byrow = TRUE, nrow = 7,ncol = 7)

plot_grid(pm1, pm2, pm3, pm4, nrow = 4)

# plot_grid(acf_res1, acf_res2,
          # acf_res3, acf_res4,
          # nrow = 2, ncol = 2)

# plot_grid(hist1, hist2, hist3, hist4, labels = c("Malli 1","Malli 2","Malli 3","Malli 4"), scale = 0.8, nrow = 2)

# plot_grid(qq1, qq2, qq3, qq4, labels =  c("Malli 1","Malli 2","Malli 3","Malli 4"), scale = 0.7, nrow = 2)

plot_grid(pm1e, pm4e, nrow = 2)

# write.csv2(bl1, file = 'bl1.txt')
# write.csv2(bl2, file = 'bl2.txt')
# write.csv2(bl3, file = 'bl3.txt')
# write.csv2(bl4, file = 'bl4.txt')