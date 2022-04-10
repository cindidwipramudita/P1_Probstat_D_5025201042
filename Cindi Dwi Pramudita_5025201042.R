install.packages("MASS")
library (MASS)
#1a
x_1a<-3-1
p_1a<-0.2
prob_1a<-dgeom(x_1a,p_1a)
prob_1a

 #1b
 mean_1b<-mean(rgeom(10000,prob_1a))
 mean_1b
 
 #1c
 cat("setelah dibandingkan probabilitas dari no 1a da no 1b ditemukan bahwa dengan peluang yang sama,","\n",
     "pada 1a orang pertama yang menjawab tidak ikut program vaksinasi saat disurvey adalah orang ketiga","\n",
     "sementara pada 1b, orang ketujuh")
 #1d
 hist(rgeom(10000,prob_1a),main="Histogram distribusi geometrik",xlab="x")

 #1e
 rataan1 <-1/p_1a
 rataan1
 varian1 <-(1-p_1a)/(p_1a^2)
 varian1
 
 #2a
 x_2a<-4
 n_2a<-20
 p_2a<-0.2

 prob_2a<-dbinom(x_2a,n_2a,p_2a) 
 prob_2a
 
 #2b
 #misal diambil 100 percobaan
 hist(rbinom(100,20,prob_2a),
      main="Histogram distribusi binomial",
      cex.main=0.8,
      xlab="x")
 
 #2c
 rataan2 <- 100*prob_2a
 rataan2
 varian2 <- 100*prob_2a*(1-prob_2a)
 varian2
 
 #3a
 x_3a<-6
 lambda1<-4.5
 
 prob_3a<-dpois(x_3a,lambda1)
 prob_3a
 
 #3b
 hist(rpois(365,lambda1),
      main="Histogram distribusi poisson",
      cex.main=0.8,
      xlab="bayi yang lahir")
 #3c
 cat("setelah dibandingkan hasil dari no 1a da no 1b ditemukan bahwa dengan lambda yang sama,","\n",
     "pada 1a peluang 6 bayi lahir pada satu hari lebih rendah daripada 5 bayi yanglahir, demikian juga ","\n",
     "pada 1b, dari histogram dapat dilihat bahwa jumlah hari dengan 5 bayi lahir lebih banyak daripada ","\n",
     "jumlah hari dengan 6 bayi lahir pada setahun")
 
#3d
cat("dikarenakan rataan dan varian dari distribusi poisson bernilai sama yaitu = lambda maka rataan = varians = ",lambda1)

#4a
x_4a<-2
df_4a<-10
prob_4a<-dchisq(x_4a,df_4a)
prob_4a

#4b
hist(rchisq(100,df_4a),
     main="Histogram distribusi Chi-Square dengan 100 data random",
     cex.main=0.8,
     xlab="x")
#4c
rataan4<-df_4a
rataan4
varian4<-df_4a*2
varian4

#5a
lambda2=3
tetha=1/lambda2
cat(lambda2,"* e^(",-lambda2,"x)")

#5b
set.seed(1)
par(mfrow=c(2,2))
hist(rexp(10),
    main="10 bilangan random",
    cex.main=0.8,
    xlab="x")
hist(rexp(100),
     main="100 bilangan random",
     cex.main=0.8,
     xlab="x")
hist(rexp(1000),
     main="1000 bilangan random",
     cex.main=0.8,
     xlab="x")
hist(rexp(10000),
     main="10000 bilangan random",
     cex.main=0.8,
     xlab="x")
#5c
rataan5=tetha
rataan5
varian5=tetha^2
varian5