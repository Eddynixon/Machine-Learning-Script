

##### My Seris Function #####

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

######

#load in north central and north west data as North_C and North_W

##### North Central Data Extrapolation ######

May_17=cbind(Seris(North_C[1,17],31,"2017-05-01"),Seris(North_C[1,2],31,"2017-05-01")[2],
             Seris(North_C[1,3],31,"2017-05-01")[2],Seris(North_C[1,4],31,"2017-05-01")[2],
             Seris(North_C[1,5],31,"2017-05-01")[2],Seris(North_C[1,6],31,"2017-05-01")[2],
             Seris(North_C[1,7],31,"2017-05-01")[2],Seris(North_C[1,8],31,"2017-05-01")[2],
             Seris(North_C[1,9],31,"2017-05-01")[2],Seris(North_C[1,10],31,"2017-05-01")[2],
             Seris(North_C[1,11],31,"2017-05-01")[2],Seris(North_C[1,12],31,"2017-05-01")[2],
             Seris(North_C[1,13],31,"2017-05-01")[2],Seris(North_C[1,14],31,"2017-05-01")[2],
             Seris(North_C[1,15],31,"2017-05-01")[2],Seris(North_C[1,16],31,"2017-05-01")[2])
Jun_17=cbind(Seris(North_C[2,17],30,"2017-06-01"),Seris(North_C[2,2],30,"2017-06-01")[2],
             Seris(North_C[2,3],30,"2017-06-01")[2],Seris(North_C[2,4],30,"2017-06-01")[2],
             Seris(North_C[2,5],30,"2017-06-01")[2],Seris(North_C[2,6],30,"2017-06-01")[2],
             Seris(North_C[2,7],30,"2017-06-01")[2],Seris(North_C[2,8],30,"2017-06-01")[2],
             Seris(North_C[2,9],30,"2017-06-01")[2],Seris(North_C[2,10],30,"2017-06-01")[2],
             Seris(North_C[2,11],30,"2017-06-01")[2],Seris(North_C[2,12],30,"2017-06-01")[2],
             Seris(North_C[2,13],30,"2017-06-01")[2],Seris(North_C[2,14],30,"2017-06-01")[2],
             Seris(North_C[2,15],30,"2017-06-01")[2],Seris(North_C[2,16],30,"2017-06-01")[2])
July_17=cbind(Seris(North_C[3,17],31,"2017-07-01"),Seris(North_C[3,2],31,"2017-07-01")[2],
              Seris(North_C[3,3],31,"2017-07-01")[2],Seris(North_C[3,4],31,"2017-07-01")[2],
              Seris(North_C[3,5],31,"2017-07-01")[2],Seris(North_C[3,6],31,"2017-07-01")[2],
              Seris(North_C[3,7],31,"2017-07-01")[2],Seris(North_C[3,8],31,"2017-07-01")[2],
              Seris(North_C[3,9],31,"2017-07-01")[2],Seris(North_C[3,10],31,"2017-07-01")[2],
              Seris(North_C[3,11],31,"2017-07-01")[2],Seris(North_C[3,12],31,"2017-07-01")[2],
              Seris(North_C[3,13],31,"2017-07-01")[2],Seris(North_C[3,14],31,"2017-07-01")[2],
              Seris(North_C[3,15],31,"2017-07-01")[2],Seris(North_C[3,16],31,"2017-07-01")[2])
Aug_17=cbind(Seris(North_C[4,17],31,"2017-08-01"),Seris(North_C[4,2],31,"2017-08-01")[2],
             Seris(North_C[4,3],31,"2017-08-01")[2],Seris(North_C[4,4],31,"2017-08-01")[2],
             Seris(North_C[4,5],31,"2017-08-01")[2],Seris(North_C[4,6],31,"2017-08-01")[2],
             Seris(North_C[4,7],31,"2017-08-01")[2],Seris(North_C[4,8],31,"2017-08-01")[2],
             Seris(North_C[4,9],31,"2017-08-01")[2],Seris(North_C[4,10],31,"2017-08-01")[2],
             Seris(North_C[4,11],31,"2017-08-01")[2],Seris(North_C[4,12],31,"2017-08-01")[2],
             Seris(North_C[4,13],31,"2017-08-01")[2],Seris(North_C[4,14],31,"2017-08-01")[2],
             Seris(North_C[4,15],31,"2017-08-01")[2],Seris(North_C[4,16],31,"2017-08-01")[2])  
Sept_17=cbind(Seris(North_C[5,17],30,"2017-09-01"),Seris(North_C[5,2],30,"2017-09-01")[2],
              Seris(North_C[5,3],30,"2017-09-01")[2],Seris(North_C[5,4],30,"2017-09-01")[2],
              Seris(North_C[5,5],30,"2017-09-01")[2],Seris(North_C[5,6],30,"2017-09-01")[2],
              Seris(North_C[5,7],30,"2017-09-01")[2],Seris(North_C[5,8],30,"2017-09-01")[2],
              Seris(North_C[5,9],30,"2017-09-01")[2],Seris(North_C[5,10],30,"2017-09-01")[2],
              Seris(North_C[5,11],30,"2017-09-01")[2],Seris(North_C[5,12],30,"2017-09-01")[2],
              Seris(North_C[5,13],30,"2017-09-01")[2],Seris(North_C[5,14],30,"2017-09-01")[2],
              Seris(North_C[5,15],30,"2017-09-01")[2],Seris(North_C[5,16],30,"2017-09-01")[2])
Oct_17=cbind(Seris(North_C[6,17],31,"2017-10-01"),Seris(North_C[6,2],31,"2017-10-01")[2],
             Seris(North_C[6,3],31,"2017-10-01")[2],Seris(North_C[6,4],31,"2017-10-01")[2],
             Seris(North_C[6,5],31,"2017-10-01")[2],Seris(North_C[6,6],31,"2017-10-01")[2],
             Seris(North_C[6,7],31,"2017-10-01")[2],Seris(North_C[6,8],31,"2017-10-01")[2],
             Seris(North_C[6,9],31,"2017-10-01")[2],Seris(North_C[6,10],31,"2017-10-01")[2],
             Seris(North_C[6,11],31,"2017-10-01")[2],Seris(North_C[6,12],31,"2017-10-01")[2],
             Seris(North_C[6,13],31,"2017-10-01")[2],Seris(North_C[6,14],31,"2017-10-01")[2],
             Seris(North_C[6,15],31,"2017-10-01")[2],Seris(North_C[6,16],31,"2017-10-01")[2])
Nov_17=cbind(Seris(North_C[7,17],30,"2017-11-01"),Seris(North_C[7,2],30,"2017-11-01")[2],
             Seris(North_C[7,3],30,"2017-11-01")[2],Seris(North_C[7,4],30,"2017-11-01")[2],
             Seris(North_C[7,5],30,"2017-11-01")[2],Seris(North_C[7,6],30,"2017-11-01")[2],
             Seris(North_C[7,7],30,"2017-11-01")[2],Seris(North_C[7,8],30,"2017-11-01")[2],
             Seris(North_C[7,9],30,"2017-11-01")[2],Seris(North_C[7,10],30,"2017-11-01")[2],
             Seris(North_C[7,11],30,"2017-11-01")[2],Seris(North_C[7,12],30,"2017-11-01")[2],
             Seris(North_C[7,13],30,"2017-11-01")[2],Seris(North_C[7,14],30,"2017-11-01")[2],
             Seris(North_C[7,15],30,"2017-11-01")[2],Seris(North_C[7,16],30,"2017-11-01")[2])
Dec_17=cbind(Seris(North_C[8,17],31,"2017-12-01"),Seris(North_C[8,2],31,"2017-12-01")[2],
             Seris(North_C[8,3],31,"2017-12-01")[2],Seris(North_C[8,4],31,"2017-12-01")[2],
             Seris(North_C[8,5],31,"2017-12-01")[2],Seris(North_C[8,6],31,"2017-12-01")[2],
             Seris(North_C[8,7],31,"2017-12-01")[2],Seris(North_C[8,8],31,"2017-12-01")[2],
             Seris(North_C[8,9],31,"2017-12-01")[2],Seris(North_C[8,10],31,"2017-12-01")[2],
             Seris(North_C[8,11],31,"2017-12-01")[2],Seris(North_C[8,12],31,"2017-12-01")[2],
             Seris(North_C[8,13],31,"2017-12-01")[2],Seris(North_C[8,14],31,"2017-12-01")[2],
             Seris(North_C[8,15],31,"2017-12-01")[2],Seris(North_C[8,16],31,"2017-12-01")[2])
Jan_18=cbind(Seris(North_C[9,17],31,"2018-01-01"),Seris(North_C[9,2],31,"2018-01-01")[2],
             Seris(North_C[9,3],31,"2018-01-01")[2],Seris(North_C[9,4],31,"2018-01-01")[2],
             Seris(North_C[9,5],31,"2018-01-01")[2],Seris(North_C[9,6],31,"2018-01-01")[2],
             Seris(North_C[9,7],31,"2018-01-01")[2],Seris(North_C[9,8],31,"2018-01-01")[2],
             Seris(North_C[9,9],31,"2018-01-01")[2],Seris(North_C[9,10],31,"2018-01-01")[2],
             Seris(North_C[9,11],31,"2018-01-01")[2],Seris(North_C[9,12],31,"2018-01-01")[2],
             Seris(North_C[9,13],31,"2018-01-01")[2],Seris(North_C[9,14],31,"2018-01-01")[2],
             Seris(North_C[9,15],31,"2018-01-01")[2],Seris(North_C[9,16],31,"2018-01-01")[2])
Feb_18=cbind(Seris(North_C[10,17],28,"2018-02-01"),Seris(North_C[10,2],28,"2018-02-01")[2],
             Seris(North_C[10,3],28,"2018-02-01")[2],Seris(North_C[10,4],28,"2018-02-01")[2],
             Seris(North_C[10,5],28,"2018-02-01")[2],Seris(North_C[10,6],28,"2018-02-01")[2],
             Seris(North_C[10,7],28,"2018-02-01")[2],Seris(North_C[10,8],28,"2018-02-01")[2],
             Seris(North_C[10,9],28,"2018-02-01")[2],Seris(North_C[10,10],28,"2018-02-01")[2],
             Seris(North_C[10,11],28,"2018-02-01")[2],Seris(North_C[10,12],28,"2018-02-01")[2],
             Seris(North_C[10,13],28,"2018-02-01")[2],Seris(North_C[10,14],28,"2018-02-01")[2],
             Seris(North_C[10,15],28,"2018-02-01")[2],Seris(North_C[10,16],28,"2018-02-01")[2])
Mar_18=cbind(Seris(North_C[11,17],31,"2018-03-01"),Seris(North_C[11,2],31,"2018-03-01")[2],
             Seris(North_C[11,3],31,"2018-03-01")[2],Seris(North_C[11,4],31,"2018-03-01")[2],
             Seris(North_C[11,5],31,"2018-03-01")[2],Seris(North_C[11,6],31,"2018-03-01")[2],
             Seris(North_C[11,7],31,"2018-03-01")[2],Seris(North_C[11,8],31,"2018-03-01")[2],
             Seris(North_C[11,9],31,"2018-03-01")[2],Seris(North_C[11,10],31,"2018-03-01")[2],
             Seris(North_C[11,11],31,"2018-03-01")[2],Seris(North_C[11,12],31,"2018-03-01")[2],
             Seris(North_C[11,13],31,"2018-03-01")[2],Seris(North_C[11,14],31,"2018-03-01")[2],
             Seris(North_C[11,15],31,"2018-03-01")[2],Seris(North_C[11,16],31,"2018-03-01")[2])
Apr_18=cbind(Seris(North_C[12,17],30,"2018-04-01"),Seris(North_C[12,2],30,"2018-04-01")[2],
             Seris(North_C[12,3],30,"2018-04-01")[2],Seris(North_C[12,4],30,"2018-04-01")[2],
             Seris(North_C[12,5],30,"2018-04-01")[2],Seris(North_C[12,6],30,"2018-04-01")[2],
             Seris(North_C[12,7],30,"2018-04-01")[2],Seris(North_C[12,8],30,"2018-04-01")[2],
             Seris(North_C[12,9],30,"2018-04-01")[2],Seris(North_C[12,10],30,"2018-04-01")[2],
             Seris(North_C[12,11],30,"2018-04-01")[2],Seris(North_C[12,12],30,"2018-04-01")[2],
             Seris(North_C[12,13],30,"2018-04-01")[2],Seris(North_C[12,14],30,"2018-04-01")[2],
             Seris(North_C[12,15],30,"2018-04-01")[2],Seris(North_C[12,16],30,"2018-04-01")[2])
May_18=cbind(Seris(North_C[13,17],31,"2018-05-01"),Seris(North_C[13,2],31,"2018-05-01")[2],
             Seris(North_C[13,3],31,"2018-05-01")[2],Seris(North_C[13,4],31,"2018-05-01")[2],
             Seris(North_C[13,5],31,"2018-05-01")[2],Seris(North_C[13,6],31,"2018-05-01")[2],
             Seris(North_C[13,7],31,"2018-05-01")[2],Seris(North_C[13,8],31,"2018-05-01")[2],
             Seris(North_C[13,9],31,"2018-05-01")[2],Seris(North_C[13,10],31,"2018-05-01")[2],
             Seris(North_C[13,11],31,"2018-05-01")[2],Seris(North_C[13,12],31,"2018-05-01")[2],
             Seris(North_C[13,13],31,"2018-05-01")[2],Seris(North_C[13,14],31,"2018-05-01")[2],
             Seris(North_C[13,15],31,"2018-05-01")[2],Seris(North_C[13,16],31,"2018-05-01")[2])
Jun_18=cbind(Seris(North_C[14,17],30,"2018-06-01"),Seris(North_C[14,2],30,"2018-06-01")[2],
             Seris(North_C[14,3],30,"2018-06-01")[2],Seris(North_C[14,4],30,"2018-06-01")[2],
             Seris(North_C[14,5],30,"2018-06-01")[2],Seris(North_C[14,6],30,"2018-06-01")[2],
             Seris(North_C[14,7],30,"2018-06-01")[2],Seris(North_C[14,8],30,"2018-06-01")[2],
             Seris(North_C[14,9],30,"2018-06-01")[2],Seris(North_C[14,10],30,"2018-06-01")[2],
             Seris(North_C[14,11],30,"2018-06-01")[2],Seris(North_C[14,12],30,"2018-06-01")[2],
             Seris(North_C[14,13],30,"2018-06-01")[2],Seris(North_C[14,14],30,"2018-06-01")[2],
             Seris(North_C[14,15],30,"2018-06-01")[2],Seris(North_C[14,16],30,"2018-06-01")[2])




North_Cent = rbind(May_17,Jun_17,July_17,Aug_17,
                   Sept_17,Oct_17,Nov_17,Dec_17,
                   Jan_18,Feb_18, Mar_18, Apr_18,
                   May_18,Jun_18
)

#####################

colnames(North_Cent)=c(colnames(North_C)[1],colnames(North_C)[17],colnames(North_C)[2:16])


write.csv(North_Cent, "North_Cent.csv")




##### North West Data Extrapotation ######

May_17=cbind(Seris(North_W[1,17],31,"2017-05-01"),Seris(North_W[1,2],31,"2017-05-01")[2],
                    Seris(North_W[1,3],31,"2017-05-01")[2],Seris(North_W[1,4],31,"2017-05-01")[2],
                    Seris(North_W[1,5],31,"2017-05-01")[2],Seris(North_W[1,6],31,"2017-05-01")[2],
                    Seris(North_W[1,7],31,"2017-05-01")[2],Seris(North_W[1,8],31,"2017-05-01")[2],
                    Seris(North_W[1,9],31,"2017-05-01")[2],Seris(North_W[1,10],31,"2017-05-01")[2],
                    Seris(North_W[1,11],31,"2017-05-01")[2],Seris(North_W[1,12],31,"2017-05-01")[2],
                    Seris(North_W[1,13],31,"2017-05-01")[2],Seris(North_W[1,14],31,"2017-05-01")[2],
                    Seris(North_W[1,15],31,"2017-05-01")[2],Seris(North_W[1,16],31,"2017-05-01")[2])
Jun_17=cbind(Seris(North_W[2,17],30,"2017-06-01"),Seris(North_W[2,2],30,"2017-06-01")[2],
                    Seris(North_W[2,3],30,"2017-06-01")[2],Seris(North_W[2,4],30,"2017-06-01")[2],
                    Seris(North_W[2,5],30,"2017-06-01")[2],Seris(North_W[2,6],30,"2017-06-01")[2],
                    Seris(North_W[2,7],30,"2017-06-01")[2],Seris(North_W[2,8],30,"2017-06-01")[2],
                    Seris(North_W[2,9],30,"2017-06-01")[2],Seris(North_W[2,10],30,"2017-06-01")[2],
                    Seris(North_W[2,11],30,"2017-06-01")[2],Seris(North_W[2,12],30,"2017-06-01")[2],
                    Seris(North_W[2,13],30,"2017-06-01")[2],Seris(North_W[2,14],30,"2017-06-01")[2],
                    Seris(North_W[2,15],30,"2017-06-01")[2],Seris(North_W[2,16],30,"2017-06-01")[2])
July_17=cbind(Seris(North_W[3,17],31,"2017-07-01"),Seris(North_W[3,2],31,"2017-07-01")[2],
                     Seris(North_W[3,3],31,"2017-07-01")[2],Seris(North_W[3,4],31,"2017-07-01")[2],
                     Seris(North_W[3,5],31,"2017-07-01")[2],Seris(North_W[3,6],31,"2017-07-01")[2],
                     Seris(North_W[3,7],31,"2017-07-01")[2],Seris(North_W[3,8],31,"2017-07-01")[2],
                     Seris(North_W[3,9],31,"2017-07-01")[2],Seris(North_W[3,10],31,"2017-07-01")[2],
                     Seris(North_W[3,11],31,"2017-07-01")[2],Seris(North_W[3,12],31,"2017-07-01")[2],
                     Seris(North_W[3,13],31,"2017-07-01")[2],Seris(North_W[3,14],31,"2017-07-01")[2],
                     Seris(North_W[3,15],31,"2017-07-01")[2],Seris(North_W[3,16],31,"2017-07-01")[2])
Aug_17=cbind(Seris(North_W[4,17],31,"2017-08-01"),Seris(North_W[4,2],31,"2017-08-01")[2],
                    Seris(North_W[4,3],31,"2017-08-01")[2],Seris(North_W[4,4],31,"2017-08-01")[2],
                    Seris(North_W[4,5],31,"2017-08-01")[2],Seris(North_W[4,6],31,"2017-08-01")[2],
                    Seris(North_W[4,7],31,"2017-08-01")[2],Seris(North_W[4,8],31,"2017-08-01")[2],
                    Seris(North_W[4,9],31,"2017-08-01")[2],Seris(North_W[4,10],31,"2017-08-01")[2],
                    Seris(North_W[4,11],31,"2017-08-01")[2],Seris(North_W[4,12],31,"2017-08-01")[2],
                    Seris(North_W[4,13],31,"2017-08-01")[2],Seris(North_W[4,14],31,"2017-08-01")[2],
                    Seris(North_W[4,15],31,"2017-08-01")[2],Seris(North_W[4,16],31,"2017-08-01")[2])  
Sept_17=cbind(Seris(North_W[5,17],30,"2017-09-01"),Seris(North_W[5,2],30,"2017-09-01")[2],
                     Seris(North_W[5,3],30,"2017-09-01")[2],Seris(North_W[5,4],30,"2017-09-01")[2],
                     Seris(North_W[5,5],30,"2017-09-01")[2],Seris(North_W[5,6],30,"2017-09-01")[2],
                     Seris(North_W[5,7],30,"2017-09-01")[2],Seris(North_W[5,8],30,"2017-09-01")[2],
                     Seris(North_W[5,9],30,"2017-09-01")[2],Seris(North_W[5,10],30,"2017-09-01")[2],
                     Seris(North_W[5,11],30,"2017-09-01")[2],Seris(North_W[5,12],30,"2017-09-01")[2],
                     Seris(North_W[5,13],30,"2017-09-01")[2],Seris(North_W[5,14],30,"2017-09-01")[2],
                     Seris(North_W[5,15],30,"2017-09-01")[2],Seris(North_W[5,16],30,"2017-09-01")[2])
Oct_17=cbind(Seris(North_W[6,17],31,"2017-10-01"),Seris(North_W[6,2],31,"2017-10-01")[2],
                    Seris(North_W[6,3],31,"2017-10-01")[2],Seris(North_W[6,4],31,"2017-10-01")[2],
                    Seris(North_W[6,5],31,"2017-10-01")[2],Seris(North_W[6,6],31,"2017-10-01")[2],
                    Seris(North_W[6,7],31,"2017-10-01")[2],Seris(North_W[6,8],31,"2017-10-01")[2],
                    Seris(North_W[6,9],31,"2017-10-01")[2],Seris(North_W[6,10],31,"2017-10-01")[2],
                    Seris(North_W[6,11],31,"2017-10-01")[2],Seris(North_W[6,12],31,"2017-10-01")[2],
                    Seris(North_W[6,13],31,"2017-10-01")[2],Seris(North_W[6,14],31,"2017-10-01")[2],
                    Seris(North_W[6,15],31,"2017-10-01")[2],Seris(North_W[6,16],31,"2017-10-01")[2])
Nov_17=cbind(Seris(North_W[7,17],30,"2017-11-01"),Seris(North_W[7,2],30,"2017-11-01")[2],
                    Seris(North_W[7,3],30,"2017-11-01")[2],Seris(North_W[7,4],30,"2017-11-01")[2],
                    Seris(North_W[7,5],30,"2017-11-01")[2],Seris(North_W[7,6],30,"2017-11-01")[2],
                    Seris(North_W[7,7],30,"2017-11-01")[2],Seris(North_W[7,8],30,"2017-11-01")[2],
                    Seris(North_W[7,9],30,"2017-11-01")[2],Seris(North_W[7,10],30,"2017-11-01")[2],
                    Seris(North_W[7,11],30,"2017-11-01")[2],Seris(North_W[7,12],30,"2017-11-01")[2],
                    Seris(North_W[7,13],30,"2017-11-01")[2],Seris(North_W[7,14],30,"2017-11-01")[2],
                    Seris(North_W[7,15],30,"2017-11-01")[2],Seris(North_W[7,16],30,"2017-11-01")[2])
Dec_17=cbind(Seris(North_W[8,17],31,"2017-12-01"),Seris(North_W[8,2],31,"2017-12-01")[2],
                    Seris(North_W[8,3],31,"2017-12-01")[2],Seris(North_W[8,4],31,"2017-12-01")[2],
                    Seris(North_W[8,5],31,"2017-12-01")[2],Seris(North_W[8,6],31,"2017-12-01")[2],
                    Seris(North_W[8,7],31,"2017-12-01")[2],Seris(North_W[8,8],31,"2017-12-01")[2],
                    Seris(North_W[8,9],31,"2017-12-01")[2],Seris(North_W[8,10],31,"2017-12-01")[2],
                    Seris(North_W[8,11],31,"2017-12-01")[2],Seris(North_W[8,12],31,"2017-12-01")[2],
                    Seris(North_W[8,13],31,"2017-12-01")[2],Seris(North_W[8,14],31,"2017-12-01")[2],
                    Seris(North_W[8,15],31,"2017-12-01")[2],Seris(North_W[8,16],31,"2017-12-01")[2])
Jan_18=cbind(Seris(North_W[9,17],31,"2018-01-01"),Seris(North_W[9,2],31,"2018-01-01")[2],
                    Seris(North_W[9,3],31,"2018-01-01")[2],Seris(North_W[9,4],31,"2018-01-01")[2],
                    Seris(North_W[9,5],31,"2018-01-01")[2],Seris(North_W[9,6],31,"2018-01-01")[2],
                    Seris(North_W[9,7],31,"2018-01-01")[2],Seris(North_W[9,8],31,"2018-01-01")[2],
                    Seris(North_W[9,9],31,"2018-01-01")[2],Seris(North_W[9,10],31,"2018-01-01")[2],
                    Seris(North_W[9,11],31,"2018-01-01")[2],Seris(North_W[9,12],31,"2018-01-01")[2],
                    Seris(North_W[9,13],31,"2018-01-01")[2],Seris(North_W[9,14],31,"2018-01-01")[2],
                    Seris(North_W[9,15],31,"2018-01-01")[2],Seris(North_W[9,16],31,"2018-01-01")[2])
Feb_18=cbind(Seris(North_W[10,17],28,"2018-02-01"),Seris(North_W[10,2],28,"2018-02-01")[2],
                    Seris(North_W[10,3],28,"2018-02-01")[2],Seris(North_W[10,4],28,"2018-02-01")[2],
                    Seris(North_W[10,5],28,"2018-02-01")[2],Seris(North_W[10,6],28,"2018-02-01")[2],
                    Seris(North_W[10,7],28,"2018-02-01")[2],Seris(North_W[10,8],28,"2018-02-01")[2],
                    Seris(North_W[10,9],28,"2018-02-01")[2],Seris(North_W[10,10],28,"2018-02-01")[2],
                    Seris(North_W[10,11],28,"2018-02-01")[2],Seris(North_W[10,12],28,"2018-02-01")[2],
                    Seris(North_W[10,13],28,"2018-02-01")[2],Seris(North_W[10,14],28,"2018-02-01")[2],
                    Seris(North_W[10,15],28,"2018-02-01")[2],Seris(North_W[10,16],28,"2018-02-01")[2])
Mar_18=cbind(Seris(North_W[11,17],31,"2018-03-01"),Seris(North_W[11,2],31,"2018-03-01")[2],
                    Seris(North_W[11,3],31,"2018-03-01")[2],Seris(North_W[11,4],31,"2018-03-01")[2],
                    Seris(North_W[11,5],31,"2018-03-01")[2],Seris(North_W[11,6],31,"2018-03-01")[2],
                    Seris(North_W[11,7],31,"2018-03-01")[2],Seris(North_W[11,8],31,"2018-03-01")[2],
                    Seris(North_W[11,9],31,"2018-03-01")[2],Seris(North_W[11,10],31,"2018-03-01")[2],
                    Seris(North_W[11,11],31,"2018-03-01")[2],Seris(North_W[11,12],31,"2018-03-01")[2],
                    Seris(North_W[11,13],31,"2018-03-01")[2],Seris(North_W[11,14],31,"2018-03-01")[2],
                    Seris(North_W[11,15],31,"2018-03-01")[2],Seris(North_W[11,16],31,"2018-03-01")[2])
Apr_18=cbind(Seris(North_W[12,17],30,"2018-04-01"),Seris(North_W[12,2],30,"2018-04-01")[2],
                    Seris(North_W[12,3],30,"2018-04-01")[2],Seris(North_W[12,4],30,"2018-04-01")[2],
                    Seris(North_W[12,5],30,"2018-04-01")[2],Seris(North_W[12,6],30,"2018-04-01")[2],
                    Seris(North_W[12,7],30,"2018-04-01")[2],Seris(North_W[12,8],30,"2018-04-01")[2],
                    Seris(North_W[12,9],30,"2018-04-01")[2],Seris(North_W[12,10],30,"2018-04-01")[2],
                    Seris(North_W[12,11],30,"2018-04-01")[2],Seris(North_W[12,12],30,"2018-04-01")[2],
                    Seris(North_W[12,13],30,"2018-04-01")[2],Seris(North_W[12,14],30,"2018-04-01")[2],
                    Seris(North_W[12,15],30,"2018-04-01")[2],Seris(North_W[12,16],30,"2018-04-01")[2])
May_18=cbind(Seris(North_W[13,17],31,"2018-05-01"),Seris(North_W[13,2],31,"2018-05-01")[2],
                    Seris(North_W[13,3],31,"2018-05-01")[2],Seris(North_W[13,4],31,"2018-05-01")[2],
                    Seris(North_W[13,5],31,"2018-05-01")[2],Seris(North_W[13,6],31,"2018-05-01")[2],
                    Seris(North_W[13,7],31,"2018-05-01")[2],Seris(North_W[13,8],31,"2018-05-01")[2],
                    Seris(North_W[13,9],31,"2018-05-01")[2],Seris(North_W[13,10],31,"2018-05-01")[2],
                    Seris(North_W[13,11],31,"2018-05-01")[2],Seris(North_W[13,12],31,"2018-05-01")[2],
                    Seris(North_W[13,13],31,"2018-05-01")[2],Seris(North_W[13,14],31,"2018-05-01")[2],
                    Seris(North_W[13,15],31,"2018-05-01")[2],Seris(North_W[13,16],31,"2018-05-01")[2])
Jun_18=cbind(Seris(North_W[14,17],30,"2018-06-01"),Seris(North_W[14,2],30,"2018-06-01")[2],
                    Seris(North_W[14,3],30,"2018-06-01")[2],Seris(North_W[14,4],30,"2018-06-01")[2],
                    Seris(North_W[14,5],30,"2018-06-01")[2],Seris(North_W[14,6],30,"2018-06-01")[2],
                    Seris(North_W[14,7],30,"2018-06-01")[2],Seris(North_W[14,8],30,"2018-06-01")[2],
                    Seris(North_W[14,9],30,"2018-06-01")[2],Seris(North_W[14,10],30,"2018-06-01")[2],
                    Seris(North_W[14,11],30,"2018-06-01")[2],Seris(North_W[14,12],30,"2018-06-01")[2],
                    Seris(North_W[14,13],30,"2018-06-01")[2],Seris(North_W[14,14],30,"2018-06-01")[2],
                    Seris(North_W[14,15],30,"2018-06-01")[2],Seris(North_W[14,16],30,"2018-06-01")[2])



#### Row Bind for Yam_KW Seris #######

North_West = rbind(May_17,Jun_17,July_17,Aug_17,
                     Sept_17,Oct_17,Nov_17,Dec_17,
                     Jan_18,Feb_18, Mar_18, Apr_18,
                     May_18,Jun_18
)

#####################

colnames(North_West)=c(colnames(North_W)[1],colnames(North_W)[17],colnames(North_W)[2:16])


write.csv(North_West, "North_West.csv")
