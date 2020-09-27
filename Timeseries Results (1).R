library("ggplot2")
library("rlang")
library("tsdf")
library("forecast")
library("corrgram")
library("data.table")
library("tidyverse") 
library("fUnitRoots")
library("lmtest")
library("timeDate")
library("timeSeries")
library("fBasics")
library("ggfortify")
library("magrittr")
library("ggpmisc")
library("tseries")
library("tsbox")
Seris = function(mean,n,start="2017-05-01"){
  Month_D = mat.or.vec(n,1)
  Month_obs = rnorm(n,mean,10)
  for(i in 1:n){
    Month_D[i]=as.Date(start,origin = "1970-01-01")+(i-1)
  }
  Month_D=as.Date(Month_D,origin = "1970-01-01")
  Month = data.frame(Month_D,Month_obs)
  return(Month)
}
#################NORTH CENTRAL###########

####For Niger State
May_17_Beans=Seris(310.61,31,"2017-05-01")
Jun_17_Beans=Seris(321.82,30,"2017-06-01")
July_17_Beans=Seris(340.07,31,"2017-07-01")
Aug_17_Beans=Seris(337.52,31,"2017-08-01")
Sept_17_Beans=Seris(344.7,30,"2017-09-01")
Oct_17_Beans=Seris(324.81,31,"2017-10-01")
Nov_17_Beans=Seris(318.63,30,"2017-11-01")
Dec_17_Beans=Seris(318.69,31,"2017-12-01")
Jan_18_Beans=Seris(377.22,31,"2018-01-01")
Feb_18_Beans=Seris(328.14,28,"2018-02-01")
Mar_18_Beans=Seris(357.71,31,"2018-03-01")
Apr_18_Beans=Seris(309.96,30,"2018-04-01")
May_18_Beans=Seris(344.7,31,"2018-05-01")
Jun_18_Beans=Seris(371.72,30,"2018-06-01")

NIG_Beans_Seris = rbind(May_17_Beans,Jun_17_Beans,July_17_Beans,Aug_17_Beans,
                        Sept_17_Beans,Oct_17_Beans,Nov_17_Beans,Dec_17_Beans,
                        Jan_18_Beans,Feb_18_Beans, Mar_18_Beans, Apr_18_Beans,
                        May_18_Beans,Jun_18_Beans)

NIG_Beans_Seris

head(NIG_Beans_Seris)


plot(NIG_Beans_Seris$Month_D,NIG_Beans_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices", main="GRAPH OF BEANS PRICES  IN NIGER STATE",col="blue")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()
#Onion
May_17_Onion=Seris(188.29,31,"2017-05-01")
Jun_17_Onion=Seris(219.43,30,"2017-06-01")
July_17_Onion=Seris(212.3,31,"2017-07-01")
Aug_17_Onion=Seris(368.44,31,"2017-08-01")
Sept_17_Onion=Seris(323.62,30,"2017-09-01")
Oct_17_Onion=Seris(236.69,31,"2017-10-01")
Nov_17_Onion=Seris(278.18,30,"2017-11-01")
Dec_17_Onion=Seris(273.04,31,"2017-12-01")
Jan_18_Onion=Seris(216.11,31,"2018-01-01")
Feb_18_Onion=Seris(200,28,"2018-02-01")
Mar_18_Onion=Seris(239.59,31,"2018-03-01")
Apr_18_Onion=Seris(237.75,30,"2018-04-01")
May_18_Onion=Seris(249.35,31,"2018-05-01")
Jun_18_Onion=Seris(265.31,30,"2018-06-01")

NIG_Onion_Seris = rbind(May_17_Onion,Jun_17_Onion,July_17_Onion,Aug_17_Onion,
                        Sept_17_Onion,Oct_17_Onion,Nov_17_Onion,Dec_17_Onion,
                        Jan_18_Onion,Feb_18_Onion, Mar_18_Onion, Apr_18_Onion,
                        May_18_Onion,Jun_18_Onion)

NIG_Onion_Seris

head(NIG_Onion_Seris)

plot(NIG_Onion_Seris$Month_D,NIG_Onion_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF ONION PRICES  IN NIGER STATE",col="red")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Tomato
May_17_Tomato=Seris(304.86,31,"2017-05-01")
Jun_17_Tomato=Seris(305.2,30,"2017-06-01")
July_17_Tomato=Seris(327.85,31,"2017-07-01")
Aug_17_Tomato=Seris(338.2,31,"2017-08-01")
Sept_17_Tomato=Seris(293.94,30,"2017-09-01")
Oct_17_Tomato=Seris(259.12,31,"2017-10-01")
Nov_17_Tomato=Seris(265.86,30,"2017-11-01")
Dec_17_Tomato=Seris(264.25,31,"2017-12-01")
Jan_18_Tomato=Seris(226.54,31,"2018-01-01")
Feb_18_Tomato=Seris(222,28,"2018-02-01")
Mar_18_Tomato=Seris(269.76,31,"2018-03-01")
Apr_18_Tomato=Seris(288.85,30,"2018-04-01")
May_18_Tomato=Seris(293.94,31,"2018-05-01")
Jun_18_Tomato=Seris(286.2,30,"2018-06-01")

NIG_Tomato_Seris = rbind(May_17_Tomato,Jun_17_Tomato,July_17_Tomato,Aug_17_Tomato,
                         Sept_17_Tomato,Oct_17_Tomato,Nov_17_Tomato,Dec_17_Tomato,
                         Jan_18_Tomato,Feb_18_Tomato, Mar_18_Tomato, Apr_18_Tomato,
                         May_18_Tomato,Jun_18_Tomato)

NIG_Tomato_Seris

head(NIG_Tomato_Seris)

plot(NIG_Tomato_Seris$Month_D,NIG_Tomato_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF TOMATO PRICES  IN NIGER STATE",col="green")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()



#Yam
May_17_Yam=Seris(277.81,31,"2017-05-01")
Aug_17_Yam=Seris(266.97,31,"2017-08-01")
Jun_17_Yam=Seris(285.83,30,"2017-06-01")
July_17_Yam=Seris(288.81,31,"2017-07-01")
Sept_17_Yam=Seris(250.5,30,"2017-09-01")
Oct_17_Yam=Seris(226.3,31,"2017-10-01")
Nov_17_Yam=Seris(220.52,30,"2017-11-01")
Dec_17_Yam=Seris(220.25,31,"2017-12-01")
Jan_18_Yam=Seris(219.14,31,"2018-01-01")
Feb_18_Yam=Seris(250,28,"2018-02-01")
Mar_18_Yam=Seris(297.76,31,"2018-03-01")
Apr_18_Yam=Seris(300.24,30,"2018-04-01")
May_18_Yam=Seris(317.47,31,"2018-05-01")
Jun_18_Yam=Seris(348.03,30,"2018-06-01")

NG_Yam_Seris = rbind(May_17_Yam,Jun_17_Yam,July_17_Yam,Aug_17_Yam,
                     Sept_17_Yam,Oct_17_Yam,Nov_17_Yam,Dec_17_Yam,
                     Jan_18_Yam,Feb_18_Yam, Mar_18_Yam, Apr_18_Yam,
                     May_18_Yam,Jun_18_Yam)

NG_Yam_Seris

head(NG_Yam_Seris)

plot(NG_Yam_Seris$Month_D,NG_Yam_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF YAM PRICES  IN NIGER STATE",col="red")
months= seq(min(NG_Yam_Seris$Month_D), max(NG_Yam_Seris$Month_obs), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()






####For BENUE State
May_17_Beans=Seris(350,31,"2017-05-01")
Jun_17_Beans=Seris(350,30,"2017-06-01")
July_17_Beans=Seris(366.39,31,"2017-07-01")
Aug_17_Beans=Seris(417.22,31,"2017-08-01")
Sept_17_Beans=Seris(426.65,30,"2017-09-01")
Oct_17_Beans=Seris(398.78,31,"2017-10-01")
Nov_17_Beans=Seris(396.04,30,"2017-11-01")
Dec_17_Beans=Seris(387.31,31,"2017-12-01")
Jan_18_Beans=Seris(427.44,31,"2018-01-01")
Feb_18_Beans=Seris(413.5,28,"2018-02-01")
Mar_18_Beans=Seris(383.89,31,"2018-03-01")
Apr_18_Beans=Seris(351.37,30,"2018-04-01")
May_18_Beans=Seris(396.65,31,"2018-05-01")
Jun_18_Beans=Seris(411.81,30,"2018-06-01")

BEN_Beans_Seris = rbind(May_17_Beans,Jun_17_Beans,July_17_Beans,Aug_17_Beans,
                        Sept_17_Beans,Oct_17_Beans,Nov_17_Beans,Dec_17_Beans,
                        Jan_18_Beans,Feb_18_Beans, Mar_18_Beans, Apr_18_Beans,
                        May_18_Beans,Jun_18_Beans)

BEN_Beans_Seris

head(BEN_Beans_Seris)

plot(BEN_Beans_Seris$Month_D,BEN_Beans_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices", main="GRAPH OF BEANS PRICES  IN BENUE STATE",col="blue")
months= seq(min(BEN_Beans_Seris$Month_D), max(BEN_Beans_Seris$Month_obs), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()



#Onion
May_17_Onion=Seris(411.81,31,"2017-05-01")
Jun_17_Onion=Seris(204.92,30,"2017-06-01")
July_17_Onion=Seris(235.65,31,"2017-07-01")
Aug_17_Onion=Seris(242.22,31,"2017-08-01")
Sept_17_Onion=Seris(241.38,30,"2017-09-01")
Oct_17_Onion=Seris(201.29,31,"2017-10-01")
Nov_17_Onion=Seris(199.18,30,"2017-11-01")
Dec_17_Onion=Seris(201.01,31,"2017-12-01")
Jan_18_Onion=Seris(224.29,31,"2018-01-01")
Feb_18_Onion=Seris(218.65,28,"2018-02-01")
Mar_18_Onion=Seris(201.31,31,"2018-03-01")
Apr_18_Onion=Seris(225.88,30,"2018-04-01")
May_18_Onion=Seris(218.91,31,"2018-05-01")
Jun_18_Onion=Seris(234.54,30,"2018-06-01")

BEN_Onion_Seris = rbind(May_17_Onion,Jun_17_Onion,July_17_Onion,Aug_17_Onion,
                        Sept_17_Onion,Oct_17_Onion,Nov_17_Onion,Dec_17_Onion,
                        Jan_18_Onion,Feb_18_Onion, Mar_18_Onion, Apr_18_Onion,
                        May_18_Onion,Jun_18_Onion)

BEN_Onion_Seris

head(BEN_Onion_Seris)


plot(BEN_Onion_Seris$Month_D,BEN_Onion_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices", main="GRAPH OF ONION PRICES  IN BENUE STATE",col="RED")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Tomato
May_17_Tomato=Seris(380.43,31,"2017-05-01")
Jun_17_Tomato=Seris(397.62,30,"2017-06-01")
July_17_Tomato=Seris(380.64,31,"2017-07-01")
Aug_17_Tomato=Seris(323.23,31,"2017-08-01")
Sept_17_Tomato=Seris(280.07,30,"2017-09-01")
Oct_17_Tomato=Seris(203.42,31,"2017-10-01")
Nov_17_Tomato=Seris(254.61,30,"2017-11-01")
Dec_17_Tomato=Seris(242.69,31,"2017-12-01")
Jan_18_Tomato=Seris(235.24,31,"2018-01-01")
Feb_18_Tomato=Seris(250.77,28,"2018-02-01")
Mar_18_Tomato=Seris(236.91,31,"2018-03-01")
Apr_18_Tomato=Seris(276.04,30,"2018-04-01")
May_18_Tomato=Seris(262.26,31,"2018-05-01")
Jun_18_Tomato=Seris(251.88,30,"2018-06-01")

BEN_Tomato_Seris = rbind(May_17_Tomato,Jun_17_Tomato,July_17_Tomato,Aug_17_Tomato,
                         Sept_17_Tomato,Oct_17_Tomato,Nov_17_Tomato,Dec_17_Tomato,
                         Jan_18_Tomato,Feb_18_Tomato, Mar_18_Tomato, Apr_18_Tomato,
                         May_18_Tomato,Jun_18_Tomato)

BEN_Tomato_Seris

head(BEN_Tomato_Seris)

plot(BEN_Tomato_Seris$Month_D,BEN_Tomato_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices", main="GRAPH OF TOMATO PRICES  IN BENUE STATE",col="violet")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()



#Yam
May_17_Yam=Seris(253.59,31,"2017-05-01")
Aug_17_Yam=Seris(277.09,31,"2017-08-01")
Jun_17_Yam=Seris(252.12,30,"2017-06-01")
July_17_Yam=Seris(262.8,31,"2017-07-01")
Sept_17_Yam=Seris(224.37,30,"2017-09-01")
Oct_17_Yam=Seris(203.62,31,"2017-10-01")
Nov_17_Yam=Seris(185.88,30,"2017-11-01")
Dec_17_Yam=Seris(179.24,31,"2017-12-01")
Jan_18_Yam=Seris(202.06,31,"2018-01-01")
Feb_18_Yam=Seris(209.2,28,"2018-02-01")
Mar_18_Yam=Seris(278.54,31,"2018-03-01")
Apr_18_Yam=Seris(319.35,30,"2018-04-01")
May_18_Yam=Seris(326.28,31,"2018-05-01")
Jun_18_Yam=Seris(369.89,30,"2018-06-01")

BEN_Yam_Seris = rbind(May_17_Yam,Jun_17_Yam,July_17_Yam,Aug_17_Yam,
                      Sept_17_Yam,Oct_17_Yam,Nov_17_Yam,Dec_17_Yam,
                      Jan_18_Yam,Feb_18_Yam, Mar_18_Yam, Apr_18_Yam,
                      May_18_Yam,Jun_18_Yam)

BEN_Yam_Seris

head(BEN_Yam_Seris)

plot(BEN_Yam_Seris$Month_D,BEN_Yam_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF YAM PRICES IN BENUE STATE",col="PURPLE")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()


#####For KOGI State
May_17_Beans=Seris(480.32,31,"2017-05-01")
Jun_17_Beans=Seris(499.64,30,"2017-06-01")
July_17_Beans=Seris(448.76,31,"2017-07-01")
Aug_17_Beans=Seris(510.24,31,"2017-08-01")
Sept_17_Beans=Seris(530.42,30,"2017-09-01")
Oct_17_Beans=Seris(510,31,"2017-10-01")
Nov_17_Beans=Seris(485.86,30,"2017-11-01")
Dec_17_Beans=Seris(452.74,31,"2017-12-01")
Jan_18_Beans=Seris(434.66,31,"2018-01-01")
Feb_18_Beans=Seris(474.69,28,"2018-02-01")
Mar_18_Beans=Seris(503.19,31,"2018-03-01")
Apr_18_Beans=Seris(491.96,30,"2018-04-01")
May_18_Beans=Seris(503.42,31,"2018-05-01")
Jun_18_Beans=Seris(548,30,"2018-06-01")


KG_Beans_Seris = rbind(May_17_Beans,Jun_17_Beans,July_17_Beans,Aug_17_Beans,
                       Sept_17_Beans,Oct_17_Beans,Nov_17_Beans,Dec_17_Beans,
                       Jan_18_Beans,Feb_18_Beans, Mar_18_Beans, Apr_18_Beans,
                       May_18_Beans,Jun_18_Beans)

KG_Beans_Seris

head(KG_Beans_Seris)


plot(KG_Beans_Seris$Month_D,KG_Beans_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF BEANS PRICES  IN KOGI STATE",col="green")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()



#Onion
May_17_Onion=Seris(302.29,31,"2017-05-01")
Jun_17_Onion=Seris(323.44,30,"2017-06-01")
July_17_Onion=Seris(321.6,31,"2017-07-01")
Aug_17_Onion=Seris(341.67,31,"2017-08-01")
Sept_17_Onion=Seris(345.38,30,"2017-09-01")
Oct_17_Onion=Seris(347.23,31,"2017-10-01")
Nov_17_Onion=Seris(302.81,30,"2017-11-01")
Dec_17_Onion=Seris(300.02,31,"2017-12-01")
Jan_18_Onion=Seris(306.46,31,"2018-01-01")
Feb_18_Onion=Seris(277.97,28,"2018-02-01")
Mar_18_Onion=Seris(277.93,31,"2018-03-01")
Apr_18_Onion=Seris(186.64,30,"2018-04-01")
May_18_Onion=Seris(206.1,31,"2018-05-01")
Jun_18_Onion=Seris(256,30,"2018-06-01")

KG_Onion_Seris = rbind(May_17_Onion,Jun_17_Onion,July_17_Onion,Aug_17_Onion,
                       Sept_17_Onion,Oct_17_Onion,Nov_17_Onion,Dec_17_Onion,
                       Jan_18_Onion,Feb_18_Onion, Mar_18_Onion, Apr_18_Onion,
                       May_18_Onion,Jun_18_Onion)

KG_Onion_Seris

head(KG_Onion_Seris)

plot(KG_Onion_Seris$Month_D,KG_Onion_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF ONION PRICES  IN KOGI STATE",col="green")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()



#Tomato
May_17_Tomato=Seris(373.23,31,"2017-05-01")
Jun_17_Tomato=Seris(421.07,30,"2017-06-01")
July_17_Tomato=Seris(397.15,31,"2017-07-01")
Aug_17_Tomato=Seris(446.34,31,"2017-08-01")
Sept_17_Tomato=Seris(423.58,30,"2017-09-01")
Oct_17_Tomato=Seris(373.86,31,"2017-10-01")
Nov_17_Tomato=Seris(370.96,30,"2017-11-01")
Dec_17_Tomato=Seris(365.83,31,"2017-12-01")
Jan_18_Tomato=Seris(324.92,31,"2018-01-01")
Feb_18_Tomato=Seris(291.07,28,"2018-02-01")
Mar_18_Tomato=Seris(278.78,31,"2018-03-01")
Apr_18_Tomato=Seris(281.38,30,"2018-04-01")
May_18_Tomato=Seris(279.04,31,"2018-05-01")
Jun_18_Tomato=Seris(285.08,30,"2018-06-01")

KG_Tomato_Seris = rbind(May_17_Tomato,Jun_17_Tomato,July_17_Tomato,Aug_17_Tomato,
                        Sept_17_Tomato,Oct_17_Tomato,Nov_17_Tomato,Dec_17_Tomato,
                        Jan_18_Tomato,Feb_18_Tomato, Mar_18_Tomato, Apr_18_Tomato,
                        May_18_Tomato,Jun_18_Tomato)

KG_Tomato_Seris

head(KG_Tomato_Seris)


plot(KG_Tomato_Seris$Month_D,KG_Tomato_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF TOMATO PRICES  IN KOGI STATE",col="black")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()


#Yam
May_17_Yam=Seris(284.82,31,"2017-05-01")
Aug_17_Yam=Seris(309.57,31,"2017-08-01")
Jun_17_Yam=Seris(320.77,30,"2017-06-01")
July_17_Yam=Seris(365.51,31,"2017-07-01")
Sept_17_Yam=Seris(279.59,30,"2017-09-01")
Oct_17_Yam=Seris(261.53,31,"2017-10-01")
Nov_17_Yam=Seris(252.92,30,"2017-11-01")
Dec_17_Yam=Seris(237.21,31,"2017-12-01")
Jan_18_Yam=Seris(248.17,31,"2018-01-01")
Feb_18_Yam=Seris(241.92,28,"2018-02-01")
Mar_18_Yam=Seris(390.66,31,"2018-03-01")
Apr_18_Yam=Seris(404.79,30,"2018-04-01")
May_18_Yam=Seris(381.54,31,"2018-05-01")
Jun_18_Yam=Seris(397.8,30,"2018-06-01")

KG_Yam_Seris = rbind(May_17_Yam,Jun_17_Yam,July_17_Yam,Aug_17_Yam,
                     Sept_17_Yam,Oct_17_Yam,Nov_17_Yam,Dec_17_Yam,
                     Jan_18_Yam,Feb_18_Yam, Mar_18_Yam, Apr_18_Yam,
                     May_18_Yam,Jun_18_Yam)

KG_Yam_Seris

head(KG_Yam_Seris)

plot(KG_Yam_Seris$Month_D,KG_Yam_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF YAM PRICES  IN KOGI STATE",col="orange")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()


#####For KWARA State
May_17_Beans=Seris(333.65,31,"2017-05-01")
Jun_17_Beans=Seris(347.7,30,"2017-06-01")
July_17_Beans=Seris(381.56,31,"2017-07-01")
Aug_17_Beans=Seris(372.4,31,"2017-08-01")
Sept_17_Beans=Seris(381.63,30,"2017-09-01")
Oct_17_Beans=Seris(374.88,31,"2017-10-01")
Nov_17_Beans=Seris(370.22,30,"2017-11-01")
Dec_17_Beans=Seris(369.01,31,"2017-12-01")
Jan_18_Beans=Seris(382.62,31,"2018-01-01")
Feb_18_Beans=Seris(370.79,28,"2018-02-01")
Mar_18_Beans=Seris(401.23,31,"2018-03-01")
Apr_18_Beans=Seris(382.92,30,"2018-04-01")
May_18_Beans=Seris(381.63,31,"2018-05-01")
Jun_18_Beans=Seris(389.05,30,"2018-06-01")


KW_Beans_Seris = rbind(May_17_Beans,Jun_17_Beans,July_17_Beans,Aug_17_Beans,
                       Sept_17_Beans,Oct_17_Beans,Nov_17_Beans,Dec_17_Beans,
                       Jan_18_Beans,Feb_18_Beans, Mar_18_Beans, Apr_18_Beans,
                       May_18_Beans,Jun_18_Beans)

KW_Beans_Seris

head(KW_Beans_Seris)

plot(KW_Beans_Seris$Month_D,KW_Beans_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF BEANS PRICES  IN KWARA STATE",col="GREY")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Onion
May_17_Onion=Seris(190.75,31,"2017-05-01")
Jun_17_Onion=Seris(188.45,30,"2017-06-01")
July_17_Onion=Seris(184.5,31,"2017-07-01")
Aug_17_Onion=Seris(216.2,31,"2017-08-01")
Sept_17_Onion=Seris(210.41,30,"2017-09-01")
Oct_17_Onion=Seris(216.32,31,"2017-10-01")
Nov_17_Onion=Seris(214.28,30,"2017-11-01")
Dec_17_Onion=Seris(207.58,31,"2017-12-01")
Jan_18_Onion=Seris(272.74,31,"2018-01-01")
Feb_18_Onion=Seris(287.35,28,"2018-02-01")
Mar_18_Onion=Seris(215.15,31,"2018-03-01")
Apr_18_Onion=Seris(204.32,30,"2018-04-01")
May_18_Onion=Seris(202.84,31,"2018-05-01")
Jun_18_Onion=Seris(194.58,30,"2018-06-01")

KW_Onion_Seris = rbind(May_17_Onion,Jun_17_Onion,July_17_Onion,Aug_17_Onion,
                       Sept_17_Onion,Oct_17_Onion,Nov_17_Onion,Dec_17_Onion,
                       Jan_18_Onion,Feb_18_Onion, Mar_18_Onion, Apr_18_Onion,
                       May_18_Onion,Jun_18_Onion)

KW_Onion_Seris

head(KW_Onion_Seris)

plot(KW_Onion_Seris$Month_D,KW_Onion_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF ONION PRICES  IN KWARA STATE",col="red")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Tomato
May_17_Tomato=Seris(284.5,31,"2017-05-01")
Jun_17_Tomato=Seris(307.6,30,"2017-06-01")
July_17_Tomato=Seris(310.17,31,"2017-07-01")
Aug_17_Tomato=Seris(375.81,31,"2017-08-01")
Sept_17_Tomato=Seris(310.55,30,"2017-09-01")
Oct_17_Tomato=Seris(251.22,31,"2017-10-01")
Nov_17_Tomato=Seris(249.89,30,"2017-11-01")
Dec_17_Tomato=Seris(240.74,31,"2017-12-01")
Jan_18_Tomato=Seris(248.8,31,"2018-01-01")
Feb_18_Tomato=Seris(185.22,28,"2018-02-01")
Mar_18_Tomato=Seris(202.89,31,"2018-03-01")
Apr_18_Tomato=Seris(271.73,30,"2018-04-01")
May_18_Tomato=Seris(265.56,31,"2018-05-01")
Jun_18_Tomato=Seris(269.65,30,"2018-06-01")

KW_Tomato_Seris = rbind(May_17_Tomato,Jun_17_Tomato,July_17_Tomato,Aug_17_Tomato,
                        Sept_17_Tomato,Oct_17_Tomato,Nov_17_Tomato,Dec_17_Tomato,
                        Jan_18_Tomato,Feb_18_Tomato, Mar_18_Tomato, Apr_18_Tomato,
                        May_18_Tomato,Jun_18_Tomato)

KW_Tomato_Seris

head(KW_Tomato_Seris)

plot(KW_Tomato_Seris$Month_D,KW_Tomato_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF TOMATO PRICES  IN KWARA STATE",col="red")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Yam
May_17_Yam=Seris(299.8,31,"2017-05-01")
Aug_17_Yam=Seris(298.01,31,"2017-08-01")
Jun_17_Yam=Seris(292.2,30,"2017-06-01")
July_17_Yam=Seris(321.52,31,"2017-07-01")
Sept_17_Yam=Seris(242.68,30,"2017-09-01")
Oct_17_Yam=Seris(221.73,31,"2017-10-01")
Nov_17_Yam=Seris(222.79,30,"2017-11-01")
Dec_17_Yam=Seris(221.78,31,"2017-12-01")
Jan_18_Yam=Seris(238.17,31,"2018-01-01")
Feb_18_Yam=Seris(260,28,"2018-02-01")
Mar_18_Yam=Seris(270.09,31,"2018-03-01")
Apr_18_Yam=Seris(258.77,30,"2018-04-01")
May_18_Yam=Seris(295.23,31,"2018-05-01")
Jun_18_Yam=Seris(325.5,30,"2018-06-01")

KW_Yam_Seris = rbind(May_17_Yam,Jun_17_Yam,July_17_Yam,Aug_17_Yam,
                     Sept_17_Yam,Oct_17_Yam,Nov_17_Yam,Dec_17_Yam,
                     Jan_18_Yam,Feb_18_Yam, Mar_18_Yam, Apr_18_Yam,
                     May_18_Yam,Jun_18_Yam)

KW_Yam_Seris

head(KW_Yam_Seris)

plot(KW_Yam_Seris$Month_D,KW_Yam_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF YAM PRICES  IN KWARA STATE",col="blue")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()




#################NORTH WEST###########

####For Kaduna State
May_17_Beans=Seris(327.54,31,"2017-05-01")
Jun_17_Beans=Seris(346.67,30,"2017-06-01")
July_17_Beans=Seris(363.05,31,"2017-07-01")
Aug_17_Beans=Seris(384.92,31,"2017-08-01")
Sept_17_Beans=Seris(376.46,30,"2017-09-01")
Oct_17_Beans=Seris(353.25,31,"2017-10-01")
Nov_17_Beans=Seris(359.62,30,"2017-11-01")
Dec_17_Beans=Seris(280.02,31,"2017-12-01")
Jan_18_Beans=Seris(318.81,31,"2018-01-01")
Feb_18_Beans=Seris(319.47,28,"2018-02-01")
Mar_18_Beans=Seris(342.95,31,"2018-03-01")
Apr_18_Beans=Seris(349.25,30,"2018-04-01")
May_18_Beans=Seris(365.29,31,"2018-05-01")
Jun_18_Beans=Seris(389.07,30,"2018-06-01")

KAD_Beans_Seris = rbind(May_17_Beans,Jun_17_Beans,July_17_Beans,Aug_17_Beans,
                        Sept_17_Beans,Oct_17_Beans,Nov_17_Beans,Dec_17_Beans,
                        Jan_18_Beans,Feb_18_Beans, Mar_18_Beans, Apr_18_Beans,
                        May_18_Beans,Jun_18_Beans)

KAD_Beans_Seris

head(KAD_Beans_Seris)


plot(KAD_Beans_Seris$Month_D,KAD_Beans_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF BEANS PRICES IN KADUNA STATE",col="green")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Onion
May_17_Onion=Seris(142.21,31,"2017-05-01")
Jun_17_Onion=Seris(157.63,30,"2017-06-01")
July_17_Onion=Seris(149.92,31,"2017-07-01")
Aug_17_Onion=Seris(135.52,31,"2017-08-01")
Sept_17_Onion=Seris(135.67,30,"2017-09-01")
Oct_17_Onion=Seris(134.76,31,"2017-10-01")
Nov_17_Onion=Seris(165.17,30,"2017-11-01")
Dec_17_Onion=Seris(166.04,31,"2017-12-01")
Jan_18_Onion=Seris(165.21,31,"2018-01-01")
Feb_18_Onion=Seris(172.31,28,"2018-02-01")
Mar_18_Onion=Seris(174.28,31,"2018-03-01")
Apr_18_Onion=Seris(172.25,30,"2018-04-01")
May_18_Onion=Seris(178.22,31,"2018-05-01")
Jun_18_Onion=Seris(192.81,30,"2018-06-01")

KAD_Onion_Seris = rbind(May_17_Onion,Jun_17_Onion,July_17_Onion,Aug_17_Onion,
                        Sept_17_Onion,Oct_17_Onion,Nov_17_Onion,Dec_17_Onion,
                        Jan_18_Onion,Feb_18_Onion, Mar_18_Onion, Apr_18_Onion,
                        May_18_Onion,Jun_18_Onion)

KAD_Onion_Seris

head(KAD_Onion_Seris)

plot(KAD_Onion_Seris$Month_D,KAD_Onion_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF ONION PRICES IN KADUNA STATE",col="purple")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Tomato
May_17_Tomato=Seris(238.59,31,"2017-05-01")
Jun_17_Tomato=Seris(234.67,30,"2017-06-01")
July_17_Tomato=Seris(236.63,31,"2017-07-01")
Aug_17_Tomato=Seris(242.81,31,"2017-08-01")
Sept_17_Tomato=Seris(232.76,30,"2017-09-01")
Oct_17_Tomato=Seris(228.71,31,"2017-10-01")
Nov_17_Tomato=Seris(237.55,30,"2017-11-01")
Dec_17_Tomato=Seris(308.23,31,"2017-12-01")
Jan_18_Tomato=Seris(211.14,31,"2018-01-01")
Feb_18_Tomato=Seris(191.29,28,"2018-02-01")
Mar_18_Tomato=Seris(294.19,31,"2018-03-01")
Apr_18_Tomato=Seris(275.35,30,"2018-04-01")
May_18_Tomato=Seris(301.68,31,"2018-05-01")
Jun_18_Tomato=Seris(337.8,30,"2018-06-01")

KAD_Tomato_Seris = rbind(May_17_Tomato,Jun_17_Tomato,July_17_Tomato,Aug_17_Tomato,
                         Sept_17_Tomato,Oct_17_Tomato,Nov_17_Tomato,Dec_17_Tomato,
                         Jan_18_Tomato,Feb_18_Tomato, Mar_18_Tomato, Apr_18_Tomato,
                         May_18_Tomato,Jun_18_Tomato)

KAD_Tomato_Seris

head(KAD_Tomato_Seris)

plot(KAD_Tomato_Seris$Month_D,KAD_Tomato_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF TOMATO PRICES IN KADUNA STATE",col="RED")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Yam
May_17_Yam=Seris(277.25,31,"2017-05-01")
Aug_17_Yam=Seris(284.14,31,"2017-08-01")
Jun_17_Yam=Seris(280.69,30,"2017-06-01")
July_17_Yam=Seris(314.85,31,"2017-07-01")
Sept_17_Yam=Seris(298.14,30,"2017-09-01")
Oct_17_Yam=Seris(252.67,31,"2017-10-01")
Nov_17_Yam=Seris(235.09,30,"2017-11-01")
Dec_17_Yam=Seris(251.43,31,"2017-12-01")
Jan_18_Yam=Seris(206.31,31,"2018-01-01")
Feb_18_Yam=Seris(234.79,31,28,"2018-02-01")
Mar_18_Yam=Seris(229.26,31,"2018-03-01")
Apr_18_Yam=Seris(284.83,30,"2018-04-01")
May_18_Yam=Seris(297.62,31,"2018-05-01")
Jun_18_Yam=Seris(324.98,30,"2018-06-01")

KAD_Yam_Seris = rbind(May_17_Yam,Jun_17_Yam,July_17_Yam,Aug_17_Yam,
                      Sept_17_Yam,Oct_17_Yam,Nov_17_Yam,Dec_17_Yam,
                      Jan_18_Yam,Feb_18_Yam, Mar_18_Yam, Apr_18_Yam,
                      May_18_Yam,Jun_18_Yam)

KAD_Yam_Seris

head(KAD_Yam_Seris)

plot(KAD_Yam_Seris$Month_D,KAD_Yam_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF YAM PRICES IN KADUNA STATE",col="purple")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

####For KATSINA State
May_17_Beans=Seris(291.59,31,"2017-05-01")
Jun_17_Beans=Seris(275.31,30,"2017-06-01")
July_17_Beans=Seris(296.64,31,"2017-07-01")
Aug_17_Beans=Seris(290.37,31,"2017-08-01")
Sept_17_Beans=Seris(297.39,30,"2017-09-01")
Oct_17_Beans=Seris(285,31,"2017-10-01")
Nov_17_Beans=Seris(284.36,30,"2017-11-01")
Dec_17_Beans=Seris(224.14,31,"2017-12-01")
Jan_18_Beans=Seris(250.15,31,"2018-01-01")
Feb_18_Beans=Seris(269.97,28,"2018-02-01")
Mar_18_Beans=Seris(284.34,31,"2018-03-01")
Apr_18_Beans=Seris(306.8,30,"2018-04-01")
May_18_Beans=Seris(324.3,31,"2018-05-01")
Jun_18_Beans=Seris(327.45,30,"2018-06-01")

KAT_Beans_Seris = rbind(May_17_Beans,Jun_17_Beans,July_17_Beans,Aug_17_Beans,
                        Sept_17_Beans,Oct_17_Beans,Nov_17_Beans,Dec_17_Beans,
                        Jan_18_Beans,Feb_18_Beans, Mar_18_Beans, Apr_18_Beans,
                        May_18_Beans,Jun_18_Beans)

KAT_Beans_Seris

head(KAT_Beans_Seris)


plot(KAT_Beans_Seris$Month_D,KAT_Beans_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF BEANS PRICES IN KATSINA STATE",col="GREEN")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()
#Onion
May_17_Onion=Seris(153.86,31,"2017-05-01")
Jun_17_Onion=Seris(167.44,30,"2017-06-01")
July_17_Onion=Seris(160.65,31,"2017-07-01")
Aug_17_Onion=Seris(172.18,31,"2017-08-01")
Sept_17_Onion=Seris(172.79,30,"2017-09-01")
Oct_17_Onion=Seris(170.18,31,"2017-10-01")
Nov_17_Onion=Seris(183.61,30,"2017-11-01")
Dec_17_Onion=Seris(183.94,31,"2017-12-01")
Jan_18_Onion=Seris(209.86,31,"2018-01-01")
Feb_18_Onion=Seris(222.43,28,"2018-02-01")
Mar_18_Onion=Seris(227.04,31,"2018-03-01")
Apr_18_Onion=Seris(233.45,30,"2018-04-01")
May_18_Onion=Seris(251.07,31,"2018-05-01")
Jun_18_Onion=Seris(263.36,30,"2018-06-01")

KAT_Onion_Seris = rbind(May_17_Onion,Jun_17_Onion,July_17_Onion,Aug_17_Onion,
                        Sept_17_Onion,Oct_17_Onion,Nov_17_Onion,Dec_17_Onion,
                        Jan_18_Onion,Feb_18_Onion, Mar_18_Onion, Apr_18_Onion,
                        May_18_Onion,Jun_18_Onion)

KAT_Onion_Seris

head(KAT_Onion_Seris)
plot(KAT_Onion_Seris$Month_D,KAT_Onion_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF ONION PRICES IN KATSINA STATE",col="Red")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Tomato
May_17_Tomato=Seris(303.47,31,"2017-05-01")
Jun_17_Tomato=Seris(318.56,30,"2017-06-01")
July_17_Tomato=Seris(370,31,"2017-07-01")
Aug_17_Tomato=Seris(446.02,31,"2017-08-01")
Sept_17_Tomato=Seris(351.98,30,"2017-09-01")
Oct_17_Tomato=Seris(346.02,31,"2017-10-01")
Nov_17_Tomato=Seris(284.29,30,"2017-11-01")
Dec_17_Tomato=Seris(320.75,31,"2017-12-01")
Jan_18_Tomato=Seris(274.07,31,"2018-01-01")
Feb_18_Tomato=Seris(260.61,28,"2018-02-01")
Mar_18_Tomato=Seris(207.95,31,"2018-03-01")
Apr_18_Tomato=Seris(221.84,30,"2018-04-01")
May_18_Tomato=Seris(261.28,31,"2018-05-01")
Jun_18_Tomato=Seris(303.32,30,"2018-06-01")

KAT_Tomato_Seris = rbind(May_17_Tomato,Jun_17_Tomato,July_17_Tomato,Aug_17_Tomato,
                         Sept_17_Tomato,Oct_17_Tomato,Nov_17_Tomato,Dec_17_Tomato,
                         Jan_18_Tomato,Feb_18_Tomato, Mar_18_Tomato, Apr_18_Tomato,
                         May_18_Tomato,Jun_18_Tomato)

KAT_Tomato_Seris

head(KAT_Tomato_Seris)

plot(KAT_Tomato_Seris$Month_D,KAT_Tomato_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF TOMATO PRICES IN KATSINA STATE",col="blue")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Yam
May_17_Yam=Seris(328.77,31,"2017-05-01")
Aug_17_Yam=Seris(323.34,31,"2017-08-01")
Jun_17_Yam=Seris(350.45,30,"2017-06-01")
July_17_Yam=Seris(359.47,31,"2017-07-01")
Sept_17_Yam=Seris(274.52,30,"2017-09-01")
Oct_17_Yam=Seris(239.47,31,"2017-10-01")
Nov_17_Yam=Seris(223.48,30,"2017-11-01")
Dec_17_Yam=Seris(213.15,31,"2017-12-01")
Jan_18_Yam=Seris(296.02,31,"2018-01-01")
Feb_18_Yam=Seris(250.68,28,"2018-02-01")
Mar_18_Yam=Seris(253.74,31,"2018-03-01")
Apr_18_Yam=Seris(277.05,30,"2018-04-01")
May_18_Yam=Seris(302.26,31,"2018-05-01")
Jun_18_Yam=Seris(387.47,30,"2018-06-01")

KAT_Yam_Seris = rbind(May_17_Yam,Jun_17_Yam,July_17_Yam,Aug_17_Yam,
                      Sept_17_Yam,Oct_17_Yam,Nov_17_Yam,Dec_17_Yam,
                      Jan_18_Yam,Feb_18_Yam, Mar_18_Yam, Apr_18_Yam,
                      May_18_Yam,Jun_18_Yam)

KAT_Yam_Seris

head(KAT_Yam_Seris)

plot(KAT_Yam_Seris$Month_D,KAT_Yam_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF YAM PRICES IN KATSINA STATE",col="grey")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#####For KEBBI State
May_17_Beans=Seris(320.62,31,"2017-05-01")
Jun_17_Beans=Seris(328.05,30,"2017-06-01")
July_17_Beans=Seris(323.5,31,"2017-07-01")
Aug_17_Beans=Seris(381.05,31,"2017-08-01")
Sept_17_Beans=Seris(359.72,30,"2017-09-01")
Oct_17_Beans=Seris(281.05,31,"2017-10-01")
Nov_17_Beans=Seris(263.28,30,"2017-11-01")
Dec_17_Beans=Seris(320.03,31,"2017-12-01")
Jan_18_Beans=Seris(354.09,31,"2018-01-01")
Feb_18_Beans=Seris(328.89,28,"2018-02-01")
Mar_18_Beans=Seris(430.51,31,"2018-03-01")
Apr_18_Beans=Seris(392.08,30,"2018-04-01")
May_18_Beans=Seris(397.37,31,"2018-05-01")
Jun_18_Beans=Seris(427.84,30,"2018-06-01")


KEB_Beans_Seris = rbind(May_17_Beans,Jun_17_Beans,July_17_Beans,Aug_17_Beans,
                        Sept_17_Beans,Oct_17_Beans,Nov_17_Beans,Dec_17_Beans,
                        Jan_18_Beans,Feb_18_Beans, Mar_18_Beans, Apr_18_Beans,
                        May_18_Beans,Jun_18_Beans)

KEB_Beans_Seris

head(KEB_Beans_Seris)

plot(KEB_Beans_Seris$Month_D,KEB_Beans_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF BEANS PRICES IN KEBBI STATE",col="green")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Onion
May_17_Onion=Seris(187.81,31,"2017-05-01")
Jun_17_Onion=Seris(205.3,30,"2017-06-01")
July_17_Onion=Seris(225.54,31,"2017-07-01")
Aug_17_Onion=Seris(243.78,31,"2017-08-01")
Sept_17_Onion=Seris(219.01,30,"2017-09-01")
Oct_17_Onion=Seris(160.15,31,"2017-10-01")
Nov_17_Onion=Seris(184.76,30,"2017-11-01")
Dec_17_Onion=Seris(218.02,31,"2017-12-01")
Jan_18_Onion=Seris(209.48,31,"2018-01-01")
Feb_18_Onion=Seris(227.44,28,"2018-02-01")
Mar_18_Onion=Seris(227.47,31,"2018-03-01")
Apr_18_Onion=Seris(243.9,30,"2018-04-01")
May_18_Onion=Seris(254.15,31,"2018-05-01")
Jun_18_Onion=Seris(302.06,30,"2018-06-01")

KEB_Onion_Seris = rbind(May_17_Onion,Jun_17_Onion,July_17_Onion,Aug_17_Onion,
                        Sept_17_Onion,Oct_17_Onion,Nov_17_Onion,Dec_17_Onion,
                        Jan_18_Onion,Feb_18_Onion, Mar_18_Onion, Apr_18_Onion,
                        May_18_Onion,Jun_18_Onion)

KEB_Onion_Seris

head(KEB_Onion_Seris)

plot(KEB_Onion_Seris$Month_D,KEB_Onion_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF ONION PRICES IN KEBBI STATE",col="red")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Tomato
May_17_Tomato=Seris(300.69,31,"2017-05-01")
Jun_17_Tomato=Seris(316.63,30,"2017-06-01")
July_17_Tomato=Seris(312.41,31,"2017-07-01")
Aug_17_Tomato=Seris(338.52,31,"2017-08-01")
Sept_17_Tomato=Seris(283.25,30,"2017-09-01")
Oct_17_Tomato=Seris(240.91,31,"2017-10-01")
Nov_17_Tomato=Seris(235.71,30,"2017-11-01")
Dec_17_Tomato=Seris(234.43,31,"2017-12-01")
Jan_18_Tomato=Seris(265.36,31,"2018-01-01")
Feb_18_Tomato=Seris(260.13,28,"2018-02-01")
Mar_18_Tomato=Seris(259.72,31,"2018-03-01")
Apr_18_Tomato=Seris(230.13,30,"2018-04-01")
May_18_Tomato=Seris(241.79,31,"2018-05-01")
Jun_18_Tomato=Seris(256.82,30,"2018-06-01")

KEB_Tomato_Seris = rbind(May_17_Tomato,Jun_17_Tomato,July_17_Tomato,Aug_17_Tomato,
                         Sept_17_Tomato,Oct_17_Tomato,Nov_17_Tomato,Dec_17_Tomato,
                         Jan_18_Tomato,Feb_18_Tomato, Mar_18_Tomato, Apr_18_Tomato,
                         May_18_Tomato,Jun_18_Tomato)

KEB_Tomato_Seris

head(KEB_Tomato_Seris)

plot(KEB_Tomato_Seris$Month_D,KEB_Tomato_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF TOMATO PRICES IN KEBBI STATE",col="violet")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Yam
May_17_Yam=Seris(252.72,31,"2017-05-01")
Aug_17_Yam=Seris(263.04,31,"2017-08-01")
Jun_17_Yam=Seris(258.82,30,"2017-06-01")
July_17_Yam=Seris(316.97,31,"2017-07-01")
Sept_17_Yam=Seris(246.32,30,"2017-09-01")
Oct_17_Yam=Seris(215.49,31,"2017-10-01")
Nov_17_Yam=Seris(210.68,30,"2017-11-01")
Dec_17_Yam=Seris(285.33,31,"2017-12-01")
Jan_18_Yam=Seris(255.06,31,"2018-01-01")
Feb_18_Yam=Seris(245.62,28,"2018-02-01")
Mar_18_Yam=Seris(237.8,31,"2018-03-01")
Apr_18_Yam=Seris(263.35,30,"2018-04-01")
May_18_Yam=Seris(311.66,31,"2018-05-01")
Jun_18_Yam=Seris(328.08,30,"2018-06-01")

KEB_Yam_Seris = rbind(May_17_Yam,Jun_17_Yam,July_17_Yam,Aug_17_Yam,
                      Sept_17_Yam,Oct_17_Yam,Nov_17_Yam,Dec_17_Yam,
                      Jan_18_Yam,Feb_18_Yam, Mar_18_Yam, Apr_18_Yam,
                      May_18_Yam,Jun_18_Yam)

KEB_Yam_Seris

head(KEB_Yam_Seris)


plot(KEB_Yam_Seris$Month_D,KEB_Yam_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF YAM PRICES IN KEBBI STATE",col="red")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()



#####For SOKOTO State
May_17_Beans=Seris(341.23,31,"2017-05-01")
Jun_17_Beans=Seris(351.28,30,"2017-06-01")
July_17_Beans=Seris(357.54,31,"2017-07-01")
Aug_17_Beans=Seris(352,31,"2017-08-01")
Sept_17_Beans=Seris(347.38,30,"2017-09-01")
Oct_17_Beans=Seris(332,31,"2017-10-01")
Nov_17_Beans=Seris(324.06,30,"2017-11-01")
Dec_17_Beans=Seris(459.67,31,"2017-12-01")
Jan_18_Beans=Seris(442.12,31,"2018-01-01")
Feb_18_Beans=Seris(430.55,28,"2018-02-01")
Mar_18_Beans=Seris(402.5,31,"2018-03-01")
Apr_18_Beans=Seris(425.82,30,"2018-04-01")
May_18_Beans=Seris(427.67,31,"2018-05-01")
Jun_18_Beans=Seris(498.37,30,"2018-06-01")


SOK_Beans_Seris = rbind(May_17_Beans,Jun_17_Beans,July_17_Beans,Aug_17_Beans,
                        Sept_17_Beans,Oct_17_Beans,Nov_17_Beans,Dec_17_Beans,
                        Jan_18_Beans,Feb_18_Beans, Mar_18_Beans, Apr_18_Beans,
                        May_18_Beans,Jun_18_Beans)

SOK_Beans_Seris

head(SOK_Beans_Seris)

plot(SOK_Beans_Seris$Month_D,SOK_Beans_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF BEANS PRICES IN SOKOTO STATE",col="PURPLE")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Onion
May_17_Onion=Seris(144.14,31,"2017-05-01")
Jun_17_Onion=Seris(140.18,30,"2017-06-01")
July_17_Onion=Seris(153.61,31,"2017-07-01")
Aug_17_Onion=Seris(175,31,"2017-08-01")
Sept_17_Onion=Seris(168.1,30,"2017-09-01")
Oct_17_Onion=Seris(158,31,"2017-10-01")
Nov_17_Onion=Seris(168.48,30,"2017-11-01")
Dec_17_Onion=Seris(264.32,31,"2017-12-01")
Jan_18_Onion=Seris(239.29,31,"2018-01-01")
Feb_18_Onion=Seris(215,28,"2018-02-01")
Mar_18_Onion=Seris(232.74,31,"2018-03-01")
Apr_18_Onion=Seris(247.96,30,"2018-04-01")
May_18_Onion=Seris(259.58,31,"2018-05-01")
Jun_18_Onion=Seris(270,30,"2018-06-01")

SOK_Onion_Seris = rbind(May_17_Onion,Jun_17_Onion,July_17_Onion,Aug_17_Onion,
                        Sept_17_Onion,Oct_17_Onion,Nov_17_Onion,Dec_17_Onion,
                        Jan_18_Onion,Feb_18_Onion, Mar_18_Onion, Apr_18_Onion,
                        May_18_Onion,Jun_18_Onion)

SOK_Onion_Seris

head(SOK_Onion_Seris)
plot(SOK_Onion_Seris$Month_D,SOK_Onion_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF ONION PRICES IN SOKOTO STATE",col="black")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Tomato
May_17_Tomato=Seris(292.05,31,"2017-05-01")
Jun_17_Tomato=Seris(292.76,30,"2017-06-01")
July_17_Tomato=Seris(320.55,31,"2017-07-01")
Aug_17_Tomato=Seris(305.56,31,"2017-08-01")
Sept_17_Tomato=Seris(278.71,30,"2017-09-01")
Oct_17_Tomato=Seris(285.12,31,"2017-10-01")
Nov_17_Tomato=Seris(276.67,30,"2017-11-01")
Dec_17_Tomato=Seris(277.54,31,"2017-12-01")
Jan_18_Tomato=Seris(287.8,31,"2018-01-01")
Feb_18_Tomato=Seris(297.14,28,"2018-02-01")
Mar_18_Tomato=Seris(299.04,31,"2018-03-01")
Apr_18_Tomato=Seris(285.71,30,"2018-04-01")
May_18_Tomato=Seris(324.61,31,"2018-05-01")
Jun_18_Tomato=Seris(372.93,30,"2018-06-01")

SOK_Tomato_Seris = rbind(May_17_Tomato,Jun_17_Tomato,July_17_Tomato,Aug_17_Tomato,
                         Sept_17_Tomato,Oct_17_Tomato,Nov_17_Tomato,Dec_17_Tomato,
                         Jan_18_Tomato,Feb_18_Tomato, Mar_18_Tomato, Apr_18_Tomato,
                         May_18_Tomato,Jun_18_Tomato)

SOK_Tomato_Seris

head(SOK_Tomato_Seris)
plot(SOK_Tomato_Seris$Month_D,SOK_Tomato_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF TOMATO PRICES IN SOKOTO STATE",col="green")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

#Yam
May_17_Yam=Seris(323.23,31,"2017-05-01")
Aug_17_Yam=Seris(332.08,31,"2017-08-01")
Jun_17_Yam=Seris(357.29,30,"2017-06-01")
July_17_Yam=Seris(327.79,31,"2017-07-01")
Sept_17_Yam=Seris(280.92,30,"2017-09-01")
Oct_17_Yam=Seris(227.79,31,"2017-10-01")
Nov_17_Yam=Seris(214.72,30,"2017-11-01")
Dec_17_Yam=Seris(257.59,31,"2017-12-01")
Jan_18_Yam=Seris(242.37,31,"2018-01-01")
Feb_18_Yam=Seris(254.03,28,"2018-02-01")
Mar_18_Yam=Seris(399.26,31,"2018-03-01")
Apr_18_Yam=Seris(405.07,30,"2018-04-01")
May_18_Yam=Seris(466.62,31,"2018-05-01")
Jun_18_Yam=Seris(488.11,30,"2018-06-01")

SOK_Yam_Seris = rbind(May_17_Yam,Jun_17_Yam,July_17_Yam,Aug_17_Yam,
                      Sept_17_Yam,Oct_17_Yam,Nov_17_Yam,Dec_17_Yam,
                      Jan_18_Yam,Feb_18_Yam, Mar_18_Yam, Apr_18_Yam,
                      May_18_Yam,Jun_18_Yam)

SOK_Yam_Seris

head(SOK_Yam_Seris)

plot(SOK_Yam_Seris$Month_D,SOK_Yam_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="GRAPH OF YAM PRICES IN SOKOTO STATE",col="red")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid()

######CONVERTING TO TIME SERIES######
NIG_Beans_Seris1=ts_ts(ts_long(NIG_Beans_Seris))
NIG_Onion_Seris1=ts_ts(ts_long(NIG_Onion_Seris))
NIG_Tomato_Seris1=ts_ts(ts_long(NIG_Tomato_Seris))
NIG_Yam_Seris1=ts_ts(ts_long(NG_Yam_Seris))
BEN_Beans_Seris1=ts_ts(ts_long(BEN_Beans_Seris))
BEN_Onion_Seris1=ts_ts(ts_long(BEN_Onion_Seris))
BEN_Tomato_Seris1=ts_ts(ts_long(BEN_Tomato_Seris))
BEN_Yam_Seris1=ts_ts(ts_long(BEN_Yam_Seris))
KG_Beans_Seris1=ts_ts(ts_long(KG_Beans_Seris))
KG_Onion_Seris1=ts_ts(ts_long(KG_Onion_Seris))
KG_Tomato_Seris1=ts_ts(ts_long(KG_Tomato_Seris))
KG_Yam_Seris1=ts_ts(ts_long(KG_Yam_Seris))
KW_Beans_Seris1=ts_ts(ts_long(KW_Beans_Seris))
KW_Onion_Seris1=ts_ts(ts_long(KW_Onion_Seris))
KW_Tomato_Seris1=ts_ts(ts_long(KW_Tomato_Seris))
KW_Yam_Seris1=ts_ts(ts_long(KW_Yam_Seris))
KAD_Beans_Seris1=ts_ts(ts_long(KAD_Beans_Seris))
KAD_Onion_Seris1=ts_ts(ts_long(KAD_Onion_Seris))
KAD_Tomato_Seris1=ts_ts(ts_long(KAD_Tomato_Seris))
KAD_Yam_Seris1=ts_ts(ts_long(KAD_Yam_Seris))
KAT_Beans_Seris1=ts_ts(ts_long(KAT_Beans_Seris))
KAT_Onion_Seris1=ts_ts(ts_long(KAT_Onion_Seris))
KAT_Tomato_Seris1=ts_ts(ts_long(KAT_Tomato_Seris))
KAT_Yam_Seris1=ts_ts(ts_long(KAT_Yam_Seris))
KEB_Beans_Seris1=ts_ts(ts_long(KEB_Beans_Seris))
KEB_Onion_Seris1=ts_ts(ts_long(KEB_Onion_Seris))
KEB_Tomato_Seris1=ts_ts(ts_long(KEB_Tomato_Seris))
KEB_Yam_Seris1=ts_ts(ts_long(KEB_Yam_Seris))
SOK_Beans_Seris1=ts_ts(ts_long(SOK_Beans_Seris))
SOK_Onion_Seris1=ts_ts(ts_long(SOK_Onion_Seris))
SOK_Tomato_Seris1=ts_ts(ts_long(SOK_Tomato_Seris))
SOK_Yam_Seris1=ts_ts(ts_long(SOK_Yam_Seris))




###Checking Stationarity using Unit root Test for the newly obtained data(If p<0.05 means the ts is stationary)
##FOR NIGER
k <- kpss.test(NIG_Beans_Seris1, null="Trend")
k
k$statistic
k$p.value # Stationary
k1 <- kpss.test(NIG_Onion_Seris1, null="Trend")
k1
k1$statistic
k1$p.value #Stationary
k2 <- kpss.test(NIG_Tomato_Seris1, null="Trend")
k2
k2$statistic
k2$p.value  # Stationary

k3 <- kpss.test(NIG_Yam_Seris1, null="Trend")
k3
k3$statistic
k3$p.value   #Stationary
###FOR BENUE
k4<- kpss.test(BEN_Beans_Seris1, null="Trend")
k4
k4$statistic
k4$p.value  #Stationary
k5 <- kpss.test(BEN_Onion_Seris1, null="Trend")
k5
k5$statistic
k5$p.value #Stationary
k6 <- kpss.test(BEN_Tomato_Seris1, null="Trend")
k6
k6$statistic
k6$p.value # Stationary
k7 <- kpss.test(BEN_Yam_Seris1, null="Trend")
k7
k7$statistic
k7$p.value  #Stationary
###FOR KOGI
k8<- kpss.test(KG_Beans_Seris1, null="Trend")
k8
k8$statistic
k8$p.value  #Stationary
k9 <- kpss.test(KG_Onion_Seris1, null="Trend")
k9
k9$statistic
k9$p.value #stationary
k10 <- kpss.test(KG_Tomato_Seris1, null="Trend")
k10
k10$statistic
k10$p.value #Stationary

k11 <- kpss.test(KG_Yam_Seris1, null="Trend")
k11
k11$statistic
k11$p.value #stationary
##FOR KWARA
k12 <- kpss.test(KW_Beans_Seris1, null="Trend")
k12
k12$statistic
k12$p.value #stationary
k13 <- kpss.test(KW_Onion_Seris1, null="Trend")
k13
k13$statistic
k13$p.value #Stationary
k14 <- kpss.test(KW_Tomato_Seris1, null="Trend")
k14
k14$statistic
k14$p.value  #Stationary

k15 <- kpss.test(KW_Yam_Seris1, null="Trend")
k15
k15$statistic
k15$p.value  # Stationary
##FOR KADUNA
k16<- kpss.test(KAD_Beans_Seris1, null="Trend")
k16
k16$statistic
k16$p.value #Stationary
k17 <- kpss.test(KAD_Onion_Seris1, null="Trend")
k17
k17$statistic
k17$p.value # Stationary
k19 <- kpss.test(KAD_Tomato_Seris1, null="Trend")
k19
k19$statistic
k19$p.value # Stationary

k20 <- kpss.test(KAD_Yam_Seris1, null="Trend")
k20
k20$statistic
k20$p.value   #Stationary
###FOR KATSINA
k21<- kpss.test(KAT_Beans_Seris1, null="Trend")
k21
k21$statistic
k21$p.value  # Stationary
k22 <- kpss.test(KAT_Onion_Seris1, null="Trend")
k22
k22$statistic
k22$p.value #Stationary
k23 <- kpss.test(KAT_Tomato_Seris1, null="Trend")
k23
k23$statistic
k23$p.value # Stationary
k24 <- kpss.test(KAT_Yam_Seris1, null="Trend")
k24
k24$statistic
k24$p.value  #Stationary
###FOR KEBBI
k25<- kpss.test(KEB_Beans_Seris1, null="Trend")
k25
k25$statistic
k25$p.value      #Stationary
k26 <- kpss.test(KEB_Onion_Seris1, null="Trend")
k26
k26$statistic
k26$p.value #stationary
k27 <- kpss.test(KEB_Tomato_Seris1, null="Trend")
k27
k27$statistic
k27$p.value #Stationary

k28 <- kpss.test(KEB_Yam_Seris1, null="Trend")
k28
k28$statistic
k28$p.value #stationary
##FOR SOKOTO
k29 <- kpss.test(SOK_Beans_Seris1, null="Trend")
k29
k29$statistic
k29$p.value #stationary
k30 <- kpss.test(SOK_Onion_Seris1, null="Trend")
k30
k30$statistic
k30$p.value #Stationary
k31 <- kpss.test(SOK_Tomato_Seris1, null="Trend")
k31
k31$statistic
k31$p.value  #Stationary

k32 <- kpss.test(SOK_Yam_Seris1, null="Trend")
k32
k32$statistic
k32$p.value  # Not Stationary





####Model####
#For Niger state Beans
Beans_Niger_model=auto.arima(NIG_Beans_Seris1,ic="aic",trace=TRUE)
Beans_Niger_model ### therefore because we differenced twice, the model is ARIMA(5,2,0)
#For Niger State Onion
Onion_Niger_model=auto.arima(NIG_Onion_Seris1,ic="aic",trace=TRUE)
Onion_Niger_model
#For Niger State Tomato
Tomato_Niger_model=auto.arima(NIG_Tomato_Seris1,ic="aic",trace=TRUE)
Tomato_Niger_model
#For Niger State Yam
Yam_Niger_model=auto.arima(NIG_Yam_Seris1,ic="aic",trace=TRUE)
Yam_Niger_model
#For Benue state Beans
Beans_benue_model=auto.arima(BEN_Beans_Seris1,ic="aic",trace=TRUE)
Beans_benue_model 
#For Benue Onion
onion_benue_model=auto.arima(BEN_Onion_Seris1,ic="aic",trace=TRUE)
onion_benue_model
#For Benue State Tomato
Tomato_Benue_model=auto.arima(BEN_Tomato_Seris1,ic="aic",trace=TRUE)
Tomato_Benue_model
#For Benue State Yam
Yam_Benue_model=auto.arima(BEN_Yam_Seris1,ic="aic",trace=TRUE)
Yam_Benue_model
#For Kogi state Beans
Beans_Kogi_model=auto.arima(KG_Beans_Seris1,ic="aic",trace=TRUE)
Beans_Kogi_model 
#For Kogi Onion
onion_Kogi_model=auto.arima(KG_Onion_Seris1,ic="aic",trace=TRUE)
onion_Kogi_model
#For Kogi State Tomato
Tomato_Kogi_model=auto.arima(KG_Tomato_Seris1,ic="aic",trace=TRUE)
Tomato_Kogi_model
#For Kogi State Yam
Yam_Kogi_model=auto.arima(KG_Yam_Seris1,ic="aic",trace=TRUE)
Yam_Kogi_model
#For Kwara state Beans
Beans_Kwara_model=auto.arima(KW_Beans_Seris1,ic="aic",trace=TRUE)
Beans_Kwara_model 
#For Kwara state Onion
Onion_kwara_model=auto.arima(KW_Onion_Seris1,ic="aic",trace=TRUE)
Onion_kwara_model
#For Kwara State Tomato
Tomato_Kwara_model=auto.arima(KW_Tomato_Seris1,ic="aic",trace=TRUE)
Tomato_Kwara_model
#For Kwara Yam
yam_kwara_model=auto.arima(KW_Yam_Seris1,ic="aic",trace=TRUE)
yam_kwara_model
########NORTH WESTERN STATES###
#For Kaduna state Beans
Beans_Kad_model=auto.arima(KAD_Beans_Seris1,ic="aic",trace=TRUE)
Beans_Kad_model 
#For Kaduna State Onion
Onion_Kad_model=auto.arima(KAD_Onion_Seris1,ic="aic",trace=TRUE)
Onion_Kad_model
#For Kaduna State Tomato
Tomato_Kad_model=auto.arima(KAD_Tomato_Seris1,ic="aic",trace=TRUE)
Tomato_Kad_model
#For Kaduna State Yam
Yam_Kad_model=auto.arima(KAD_Yam_Seris1,ic="aic",trace=TRUE)
Yam_Kad_model
#For Katsina state Beans
Beans_Kat_model=auto.arima(KAT_Beans_Seris1,ic="aic",trace=TRUE)
Beans_Kat_model 
#For Katsina Onion
onion_Kat_model=auto.arima(KAT_Onion_Seris1,ic="aic",trace=TRUE)
onion_Kat_model
#For Katsina State Tomato
Tomato_Kat_model=auto.arima(KAT_Tomato_Seris1,ic="aic",trace=TRUE)
Tomato_Kat_model
#For Katsina State Yam
Yam_Kat_model=auto.arima(KAT_Yam_Seris1,ic="aic",trace=TRUE)
Yam_Kat_model
#For Kebbi state Beans
Beans_Keb_model=auto.arima(KEB_Beans_Seris1,ic="aic",trace=TRUE)
Beans_Keb_model 
#For Kebbi Onion
onion_Keb_model=auto.arima(KEB_Onion_Seris1,ic="aic",trace=TRUE)
onion_Keb_model
#For Kebbi State Tomato
Tomato_Keb_model=auto.arima(KEB_Tomato_Seris1,ic="aic",trace=TRUE)
Tomato_Keb_model
#For Kebbi State Yam
Yam_Keb_model=auto.arima(KEB_Yam_Seris1,ic="aic",trace=TRUE)
Yam_Keb_model
#For SOKOTO state Beans
Beans_Sok_model=auto.arima(SOK_Beans_Seris1,ic="aic",trace=TRUE)
Beans_Sok_model 
#For Sokoto state Onion
Onion_Sok_model=auto.arima(SOK_Onion_Seris1,ic="aic",trace=TRUE)
Onion_Sok_model
#For Sokoto State Tomato
Tomato_Sok_model=auto.arima(SOK_Tomato_Seris1,ic="aic",trace=TRUE)
Tomato_Sok_model
#For Sokoto Yam
yam_Sok_model=auto.arima(SOK_Yam_Seris1,ic="aic",trace=TRUE)
yam_Sok_model


##VALIDATION##
Box.test(Beans_Niger_model$residuals,lag=4,type="Ljung-Box")
Box.test(Beans_Niger_model$residuals,lag=5,type="Ljung-Box")
plot(Beans_Niger_model$residuals)
Box.test(Onion_Niger_model$residuals,lag=4,type="Ljung-Box")
Box.test(Onion_Niger_model$residuals,lag=5,type="Ljung-Box")
plot(Onion_Niger_model$residuals)
Box.test(Tomato_Niger_model$residuals,lag=4,type="Ljung-Box")
Box.test(Tomato_Niger_model$residuals,lag=5,type="Ljung-Box")
plot(Tomato_Niger_model$residuals)
Box.test(Yam_Niger_model$residuals,lag=4,type="Ljung-Box")
Box.test(Yam_Niger_model$residuals,lag=5,type="Ljung-Box")
plot(Yam_Niger_model$residuals)
Box.test(Beans_benue_model$residuals,lag=4,type="Ljung-Box")
Box.test(Beans_benue_model$residuals,lag=5,type="Ljung-Box")
plot(Beans_benue_model$residuals)
Box.test(onion_benue_model$residuals,lag=4,type="Ljung-Box")
Box.test(onion_benue_model$residuals,lag=5,type="Ljung-Box")
plot(onion_benue_model$residuals)
Box.test(Tomato_Benue_model$residuals,lag=4,type="Ljung-Box")
Box.test(Tomato_Benue_model$residuals,lag=5,type="Ljung-Box")
plot(Tomato_Benue_model$residuals)
Box.test(Yam_Benue_model$residuals,lag=4,type="Ljung-Box")
Box.test(Yam_Benue_model$residuals,lag=5,type="Ljung-Box")
plot(Yam_Benue_model$residuals)
Box.test(Beans_Kogi_model$residuals,lag=4,type="Ljung-Box")
Box.test(Beans_Kogi_model$residuals,lag=5,type="Ljung-Box")
plot(Beans_Kogi_model$residuals)
Box.test(onion_Kogi_model$residuals,lag=4,type="Ljung-Box")
Box.test(onion_Kogi_model$residuals,lag=5,type="Ljung-Box")
plot(onion_Kogi_model$residuals)
Box.test(Tomato_Kogi_model$residuals,lag=4,type="Ljung-Box")
Box.test(Tomato_Kogi_model$residuals,lag=5,type="Ljung-Box")
plot(Tomato_Kogi_model$residuals)
Box.test(Yam_Kogi_model$residuals,lag=4,type="Ljung-Box")
Box.test(Yam_Kogi_model$residuals,lag=5,type="Ljung-Box")
plot(Yam_Kogi_model$residuals)
Box.test(Beans_Kwara_model$residuals,lag=4,type="Ljung-Box")
Box.test(Beans_Kwara_model$residuals,lag=5,type="Ljung-Box")
plot(Beans_Kwara_model$residuals)
Box.test(Onion_kwara_model$residuals,lag=4,type="Ljung-Box")
Box.test(Onion_kwara_model$residuals,lag=5,type="Ljung-Box")
plot(Onion_kwara_model$residuals)
Box.test(Tomato_Kwara_model$residuals,lag=4,type="Ljung-Box")
Box.test(Tomato_Kwara_model$residuals,lag=5,type="Ljung-Box")
plot(Tomato_Kogi_model$residuals)
Box.test(Yam_Kogi_model$residuals,lag=4,type="Ljung-Box")
Box.test(Yam_Kogi_model$residuals,lag=5,type="Ljung-Box")
plot(Yam_Kogi_model$residuals)
#NORTH WESTERN STATES

Box.test(Beans_Kad_model$residuals,lag=4,type="Ljung-Box")
Box.test(Beans_Kad_model$residuals,lag=5,type="Ljung-Box")
plot(Beans_Kad_model$residuals)
Box.test(Onion_kad_model$residuals,lag=4,type="Ljung-Box")
Box.test(Onion_Kad_model$residuals,lag=5,type="Ljung-Box")
plot(Onion_kad_model$residuals)
Box.test(Tomato_Kad_model$residuals,lag=4,type="Ljung-Box")
Box.test(Tomato_Kad_model$residuals,lag=5,type="Ljung-Box")
plot(Tomato_Kad_model$residuals)
Box.test(Yam_Kad_model$residuals,lag=4,type="Ljung-Box")
Box.test(Yam_Kad_model$residuals,lag=5,type="Ljung-Box")
plot(Yam_Kad_model$residuals)
Box.test(Beans_Kat_model$residuals,lag=4,type="Ljung-Box")
Box.test(Beans_Kat_model$residuals,lag=5,type="Ljung-Box")
plot(Beans_Kat_model$residuals)
Box.test(onion_Kat_model$residuals,lag=4,type="Ljung-Box")
Box.test(onion_Kat_model$residuals,lag=5,type="Ljung-Box")
plot(Onion_Kat_model$residuals)
Box.test(Tomato_Kat_model$residuals,lag=4,type="Ljung-Box")
Box.test(Tomato_Kat_model$residuals,lag=5,type="Ljung-Box")
plot(Tomato_Kat_model$residuals)
Box.test(Yam_Kat_model$residuals,lag=4,type="Ljung-Box")
Box.test(Yam_Kat_model$residuals,lag=5,type="Ljung-Box")
plot(Yam_Kat_model$residuals)
Box.test(Beans_Keb_model$residuals,lag=4,type="Ljung-Box")
Box.test(Beans_Keb_model$residuals,lag=5,type="Ljung-Box")
plot(Beans_Keb_model$residuals)
Box.test(onion_Keb_model$residuals,lag=4,type="Ljung-Box")
Box.test(onion_Keb_model$residuals,lag=5,type="Ljung-Box")
plot(onion_Keb_model$residuals)
Box.test(Tomato_Keb_model$residuals,lag=4,type="Ljung-Box")
Box.test(Tomato_Keb_model$residuals,lag=5,type="Ljung-Box")
plot(Tomato_Keb_model$residuals)
Box.test(Yam_Keb_model$residuals,lag=4,type="Ljung-Box")
Box.test(Yam_Keb_model$residuals,lag=5,type="Ljung-Box")
plot(Yam_Keb_model$residuals)
Box.test(Beans_Sok_model$residuals,lag=4,type="Ljung-Box")
Box.test(Beans_Sok_model$residuals,lag=5,type="Ljung-Box")
plot(Beans_Sok_model$residuals)
Box.test(Onion_Sok_model$residuals,lag=4,type="Ljung-Box")
Box.test(Onion_Sok_model$residuals,lag=5,type="Ljung-Box")
plot(Onion_Sok_model$residuals)
Box.test(Tomato_Sok_model$residuals,lag=4,type="Ljung-Box")
Box.test(Tomato_Sok_model$residuals,lag=5,type="Ljung-Box")
plot(Tomato_Sok_model$residuals)
Box.test(yam_Sok_model$residuals,lag=4,type="Ljung-Box")
Box.test(yam_Sok_model$residuals,lag=5,type="Ljung-Box")
plot(yam_Sok_model$residuals)
#####FORCASTING 
myforecast1<-forecast(Beans_Niger_model,level=c(95),h=30*6)
myforecast1
plot(myforecast1,main="Forecast plot of Beans in Niger State")
myforecast2<-forecast(Onion_Niger_model,level=c(95),h=30*6)
myforecast2
plot(myforecast2,main="Forecast plot of Onion in Niger State")
myforecast3<-forecast(Tomato_Niger_model,level=c(95),h=30*1)
myforecast3
plot(myforecast3,main="Forecast plot of Tomato in Niger State")
myforecast4<-forecast(Yam_Niger_model,level=c(95),h=30*1)
myforecast4
plot(myforecast4,main="Forecast plot of Yam in Niger State")
myforecast5<-forecast(Beans_benue_model,level=c(95),h=30*1)
myforecast5
plot(myforecast5,main="Forecast plot of Beans in Benue State")
myforecast6<-forecast(onion_benue_model,level=c(95),h=30*1)
myforecast6
plot(myforecast6,main="Forcast for Onion in Benue State")
myforecast7<-forecast(Tomato_Benue_model,level=c(95),h=30*1)
myforecast7
plot(myforecast7,xlab="Years",ylab="Prices",main="Forcast for Tomato in Benue State")
myforecast8<-forecast(Yam_Benue_model,level=c(95),h=30*1)
myforecast8
plot(myforecast8,main="Forcast for Yam in Benue State")
myforecast9<-forecast(Beans_Kogi_model,level=c(95),h=30*6)
myforecast9
plot(myforecast9,main="Forcast for Beans in Kogi State")
myforecast10<-forecast(onion_Kogi_model,level=c(95),h=30*6)
myforecast10
plot(myforecast10,main="Forcast for Onion in Kogi State")
myforecast11<-forecast(Tomato_Kogi_model,level=c(95),h=30*6)
myforecast11
plot(myforecast11,main="Forcast for Tomato in Kogi State")
myforecast12<-forecast(Yam_Kogi_model,level=c(95),h=30*1)
myforecast12
plot(myforecast12,main="Forcast for Yam in Kogi State")
myforecast13<-forecast(Beans_Kwara_model,level=c(95),h=30*1)
myforecast13
plot(myforecast13,main="Forcast for Beans in Kwara State")



myforecast14<-forecast(Onion_kwara_model,level=c(95),h=30*1)
myforecast14
plot(myforecast14,KW_Tomato_Seris$Month_obs,main="Forcast for Onion in Kwara State")

plot(myforecast14,KW_Tomato_Seris$Month_obs, type="l",xaxt="n", xlab="Year", ylab="Prices ", main="Graph of Forcast for Onion in Kwara State",col="red")
months= seq(min(BEN_Tomato_Seris$Month_D), max(BEN_Tomato_Seris$Month_D), "month")
axis(1, months, format(months, "%Y\n%b"))
grid(col="grey")



myforecast15<-forecast(Tomato_Kwara_model,level=c(95),h=30*1)
myforecast15
plot(myforecast15,xlab="Years",ylab="Prices",main=" Forcast for Tomato in Kwara State")
myforecast16<-forecast(yam_kwara_model,level=c(95),h=30*1)
myforecast16
plot(myforecast16,main="Forcast for Yam in Kwara State")



myforecast17<-forecast(Beans_Kad_model,level=c(95),h=30*4)
myforecast17
plot(myforecast17,main="Forecast plot of Beans in Kaduna State")
myforecast18<-forecast(Onion_Kad_model,level=c(95),h=30*4)
myforecast18
plot(myforecast18,main="Forecast plot of Onion in Kaduna State")
myforecast19<-forecast(Tomato_Kad_model,level=c(95),h=30*4)
myforecast19
plot(myforecast19,main="Forecast plot of Tomato in Kaduna State")
myforecast20<-forecast(Yam_Kad_model,level=c(95),h=30*1)
myforecast20
plot(myforecast20,main="Forecast plot of Yam in Kaduna State")
myforecast21<-forecast(Beans_Kat_model,level=c(95),h=30*1)
myforecast21
plot(myforecast21,main="Forecast plot of Beans in Katsina State")
myforecast22<-forecast(onion_Kat_model,level=c(95),h=30*6)
myforecast22
plot(myforecast22,main="Forcast for Onion in Katsina State")
myforecast23<-forecast(Tomato_Kat_model,level=c(95),h=30*7)
myforecast23
plot(myforecast23,xlab="Years",ylab="Prices",main="Forcast for Tomato in Katsina State")
myforecast24<-forecast(Yam_Kat_model,level=c(95),h=30*6)
myforecast24
plot(myforecast24,main="Forcast for Yam in Katsina State")
myforecast25<-forecast(Beans_Keb_model,level=c(95),h=30*6)
myforecast25
plot(myforecast25,main="Forcast for Beans in Kebbi State")
myforecast27<-forecast(onion_Keb_model,level=c(95),h=30*6)
myforecast27
plot(myforecast27,main="Forcast for Onion in Kebbi State")
myforecast28<-forecast(Tomato_Keb_model,level=c(95),h=30*6)
myforecast28
plot(myforecast28,main="Forcast for Tomato in Kebbi State")
myforecast29<-forecast(Yam_Keb_model,level=c(95),h=30*6)
myforecast29
plot(myforecast29,main="Forcast for Yam in Kebbi State")
myforecast30<-forecast(Beans_Sok_model,level=c(95),h=30*6)
myforecast30
plot(myforecast30,main="Forcast for Beans in Sokoto State")
myforecast31<-forecast(Onion_Sok_model,level=c(95),h=30*1)
myforecast31
plot(myforecast31,main="Forcast for Onion in Sokoto State")
myforecast32<-forecast(Tomato_Sok_model,level=c(95),h=30*6)
myforecast32
plot(myforecast32,xlab="Years",ylab="Prices",main=" Forcast for Tomato in Sokoto State")
myforecast33<-forecast(yam_Sok_model,level=c(95),h=30*1)
myforecast33
plot(myforecast33,main="Forcast for Yam in Sokoto State")



###ACF
acf(ts(Beans_Kad_model$residuals),main="ACF of Beans For Kaduna")
acf(ts(Onion_Kad_model$residuals),main="ACF of Onion For Kaduna")
acf(ts(Tomato_Kad_model$residuals),main="ACF of Tomato For Kaduna")
acf(ts(Yam_Kad_model$residuals),main="ACF of Yam For Kaduna")
acf(ts(Beans_Kat_model$residuals),main="ACF of Beans For Katsina")
acf(ts(onion_Kat_model$residuals),main="ACF of Onion For Katsina")
acf(ts(Tomato_Kat_model$residuals),main="ACF of Tomato For Katsina")
acf(ts(Beans_Kat_model$residuals),main="ACF of Yam For Katsina")
acf(ts(Beans_Keb_model$residuals),main="ACF of Beans For Kebbi")
acf(ts(onion_Keb_model$residuals),main="ACF of Onion For Kebbi")
acf(ts(Tomato_Keb_model$residuals),main="ACF of Tomato For Kebbi")
acf(ts(Yam_Keb_model$residuals),main="ACF of Yam For Kebbi")
acf(ts(Beans_Sok_model$residuals),main="ACF of Beans For Sokoto")
acf(ts(Onion_Sok_model$residuals),main="ACF of Onion For Sokoto")
acf(ts(Tomato_Sok_model$residuals),main="ACF of Tomato For Sokoto")
acf(ts(yam_Sok_model$residuals),main="ACF of Yam For Sokoto")


acf(ts(Beans_Niger_model$residuals),main="ACF of Beans For Niger State")
acf(ts(Onion_Niger_model$residuals),main="ACF of Onion For Niger State")
acf(ts(Tomato_Niger_model$residuals),main="ACF of Tomato For Niger State")
acf(ts(Yam_Niger_model$residuals),main="ACF of Yam For Niger State")

###PACF OF RESIDUAL
pacf(ts(Beans_Kad_model$residuals),main="PACF of Beans For Kaduna")
pacf(ts(Onion_Kad_model$residuals),main="PACF of Onion For Kaduna")
pacf(ts(Tomato_Kad_model$residuals),main="PACF of Tomato For Kaduna")
pacf(ts(Yam_Kad_model$residuals),main="PACF of Yam For Kaduna")
pacf(ts(Beans_Kat_model$residuals),main="PACF of Beans For Katsina")
pacf(ts(onion_Kat_model$residuals),main="PACF of Onion For Katsina")
pacf(ts(Tomato_Kat_model$residuals),main="PACF of Tomato For Katsina")
pacf(ts(Yam_Kat_model$residuals),main="PACF of Yam For Katsina")
pacf(ts(Beans_Keb_model$residuals),main="PACF of Beans For Kebbi")
pacf(ts(onion_Keb_model$residuals),main="PACF of Onion For Kebbi")
pacf(ts(Tomato_Keb_model$residuals),main="PACF of Tomato For Kebbi")
pacf(ts(Yam_Keb_model$residuals),main="PACF of Yam For Kebbi")
pacf(ts(Beans_Sok_model$residuals),main="PACF of Beans For Sokoto")
pacf(ts(Onion_Sok_model$residuals),main="PACF of Beans For Sokoto")
pacf(ts(Tomato_Sok_model$residuals),main="PACF of Beans For Sokoto")
pacf(ts(yam_Sok_model$residuals),main="PACF of Beans For Sokoto")






mean1=myforecast1$mean
mean2=myforecast2$mean
mean3=myforecast3$mean
mean4=myforecast4$mean
mean5=myforecast5$mean
mean6=myforecast6$mean
mean7=myforecast7$mean
mean8=myforecast8$mean
mean9=myforecast9$mean
mean10=myforecast10$mean
mean11=myforecast11$mean
mean12=myforecast12$mean
mean13=myforecast13$mean
mean14=myforecast14$mean
mean15=myforecast15$mean
mean16=myforecast16$mean
mean17=myforecast17$mean
mean18=myforecast18$mean
mean19=myforecast19$mean
mean20=myforecast20$mean
mean21=myforecast21$mean
mean22=myforecast22$mean
mean23=myforecast23$mean
mean24=myforecast24$mean
mean25=myforecast25$mean
mean26=myforecast27$mean
mean27=myforecast28$mean
mean28=myforecast29$mean
mean29=myforecast30$mean
mean30=myforecast31$mean
mean31=myforecast32$mean
mean32=myforecast33$mean


forecast_values=cbind(mean1,mean2,mean3,mean4,mean5,mean6,mean7,mean8,mean9,mean10,
      mean11,mean12,mean13,mean14,mean15,mean16,mean17,mean18,mean19,mean20,mean21,mean22,
      mean23,mean24,mean25,mean26,mean27,mean28,mean29,mean30,mean31,mean32)
write.csv(forecast_values, "forecast.csv")

