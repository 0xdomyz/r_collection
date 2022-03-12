library(stats)
library(dplyr)
library(ggplot2)
library(moments)
library(rmutil)
library(actuar)
library(extraDistr)
options(stringsAsFactors = FALSE)

#plot func
plt_dists = function(plot_df,discrete = F,int_x_axis = F){
  
  print(head(plot_df))
  
  plot_df_long =
    plot_df %>%
    reshape2::melt(id.vars = c('x'),
                   variable.name='dist',
                   value.name='y')
  #print(head(plot_df_long))
  
  p = 
  ggplot(data = plot_df_long)
  
  if(discrete){
    p= p + geom_col(aes(x=x,y=y,fill=dist),position = 'dodge')
  }else{
    p= p + geom_line(aes(x=x,y=y,color=dist))
  }
  
  if(int_x_axis){
    p = p + 
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))
  }
  
  p
}

#binomial
plot_df = 
  data.frame(x=seq(0,10,1)) %>%
  mutate('binom_10_0.1'=dbinom(x, size=10, prob=0.1),
         'binom_10_0.2'=dbinom(x, size=10, prob=0.2),
         'binom_50_0.1'=dbinom(x, size=50, prob=0.1)
  )
plt_dists(plot_df,discrete = T)

#binomial & poisson
plot_df = 
  data.frame(x=seq(0,20,1)) %>%
  mutate('binom_100_0.1'=dbinom(x, size=100, prob=0.1),
         'binom_1000_0.01'=dbinom(x, size=1000, prob=0.01),
         'pois_10'=dpois(x, lambda = 10)
  )
plt_dists(plot_df,discrete = T)

#binomial & poisson & normal
plot_df = 
  data.frame(x=seq(0,20,1)) %>%
  mutate('binom_1000_0.01'=dbinom(x, size=1000, prob=0.01),
         'pois_10'=dpois(x, lambda = 10),
         'norm_10_s9.9'=
           pnorm(x+0.5,mean = 10,sd = sqrt(9.9))-
           pnorm(x-0.5,mean = 10,sd = sqrt(9.9))
  )
plt_dists(plot_df,discrete = T)

plot_df = 
  data.frame(x=seq(90,110,1)) %>%
  mutate('binom_10000_0.01'=pbinom(x, size=10000, prob=0.01),
         'pois_100'=ppois(x, lambda = 100),
         'norm_100_s99'=pnorm(x,mean = 100,sd = sqrt(99))
  )
plt_dists(plot_df,discrete = T)

#qq plot poisson vs normal
y = rpois(n = 10000,lambda = 10)
qqnorm(y);qqline(y, col = 2)

set.seed(1)
y = rnorm(n = 10000,mean = 0,sd = 1)
qqplot(qnorm(ppoints(1000), mean = 0,sd = 1), y)
qqline(y, distribution = function(p) qnorm(p, mean = 0,sd = 1),
       prob = c(0.25, 0.75), col = 2)
qqline(y, distribution = function(p) qnorm(p, mean = 0,sd = 1),
       prob = c(0.4, 0.6), col = 3)

#std normal quantiles
plot_df = 
  data.frame(x=seq(-5,5,0.1)) %>%
  mutate('norm_std'=dnorm(x,mean = 0,sd = 1)
  )
plt_dists(plot_df,discrete = F,int_x_axis = T)

pnorm(-2)
qnorm(0.025)
qnorm(0.05)
qnorm(0.16)
pnorm(-1)
pnorm(1)-pnorm(-1)

#t kurtosis
plot_df = 
  data.frame(x=seq(-5,5,0.1)) %>%
  mutate('norm_std'=dnorm(x,mean = 0,sd = 1),
         't_1' = dt(x,1),
         't_2' = dt(x,2),
         't_5' = dt(x,5)
  )
plt_dists(plot_df,discrete = F,int_x_axis = T)

kurtosis(rt(10000,1))
kurtosis(rt(10000,2))
kurtosis(rt(10000,5))

pt(-2,1)
pt(-2,2)
pt(-2,5)
pt(-2,10)
pt(-2,500)

n=10000
y = rt(n,1)
qqplot(qnorm(ppoints(n), mean = 0,sd = 1), y)
qqline(y, distribution = function(p) qnorm(p, mean = 0,sd = 1),
       prob = c(0.25, 0.75), col = 2)

n=10000
y = rt(n,5)
qqplot(qnorm(ppoints(n), mean = 0,sd = 1), y)
qqline(y, distribution = function(p) qnorm(p, mean = 0,sd = 1),
       prob = c(0.25, 0.75), col = 2)
kurtosis(y)
kurtosis(rnorm(1000))

#t degree of freedom: dist of stat of sample from t
m= 1000#number of calc of stats
df_m = data.frame(
  min = 1,q1 = 1,med = 1,mu = 1,q3 = 1,max = 1,
  var = 1
)

for(i in 1:m){
  #i=1
  n=1000#stat sample size
  y = rt(n,1)
  df_m[i,c('min','q1','med','mu','q3','max')]=
    as.vector(summary(y))
  df_m[i,'var']=var(y)
  df_m[i,'skew']=skewness(y)
  df_m[i,'kurt']=kurtosis(y)
}

cdf_mu = ecdf(df_m$mu)
cdf_var = ecdf(df_m$var)
cdf_skew = ecdf(df_m$skew)
cdf_kurt = ecdf(df_m$kurt)
seq_min = -30;seq_max = 30;seq_step = 1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('norm_std'=dnorm(x,mean = 0,sd = 1),
         't_mu' = (cdf_mu(x+seq_step/2)-cdf_mu(x-seq_step/2))/seq_step,
         't_var' = (cdf_var(x+seq_step/2)-cdf_var(x-seq_step/2))/seq_step,
         't_skew' = (cdf_skew(x+seq_step/2)-cdf_skew(x-seq_step/2))/seq_step,
         't_kurt' = (cdf_kurt(x+seq_step/2)-cdf_kurt(x-seq_step/2))/seq_step
  )
plt_dists(plot_df,discrete = F)

hist(df_m$kurt)
hist(df_m$kurt,breaks = 10)
hist(df_m$kurt,breaks = 50)
boxplot(df_m$kurt)
summary(df_m$kurt)

#t tail power law
seq_min = 5;seq_max = 10;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('norm_std'=dnorm(x,mean = 0,sd = 1),
         't_1' = dt(x,1),
         'x^-2s' = 0.3*x^-2
  )
plt_dists(plot_df,discrete = F)

x=c(1,2,3,4,5,6,7,8,9,10)
y=dt(x,1)
y2=x^(-(1+1))
y;y2;y/y2

#gumbel
dgb = function(x,alpha,beta) {
  z=(x-alpha)/beta
  1/beta*exp(-(z+exp(-z)))
}
pgb = function(q,alpha,beta){
  z=(q-alpha)/beta
  exp(-exp(-z))
}
qgb = function(p,alpha,beta){
  -1*beta*log(-log(p))+alpha
}
pgb(0.1,0,1)
qgb(0.4046077,0,1)

rgumbel = function(n,alpha,beta){
  #n=1000
  qgb(runif(n),alpha,beta)
}

seq_min = -5;seq_max = 10;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('norm_std'=dnorm(x,mean = 0,sd = 1),
         'gumbel_0_1' = dgb(x,0,1),
         'gumbel_1_1' = dgb(x,1,1),
         'gumbel_0_2' = dgb(x,0,2)
  )
plt_dists(plot_df,discrete = F)

skewness(rgumbel(1000000,0,1))
skewness(rgumbel(1000000,1,1))
skewness(rgumbel(1000000,0,2))

#log normal
seq_min = 0;seq_max = 10;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('norm_std'=dnorm(x,mean = 0,sd = 1),
         'ln_0_1' = dlnorm(x,0,1)
  )
plt_dists(plot_df,discrete = F,int_x_axis = T)

n=10000
y = rlnorm(n,0,1)
logy=log(y)

summary(y)
skewness(y)
kurtosis(y)
boxplot(y)
hist(y,breaks = 10)

hist(y,breaks = c(seq(0,10,0.1),50),xlim = c(0,10))
ggplot(data.frame(y)) +
  geom_histogram(aes(y),binwidth = 0.1) +
  scale_x_continuous(breaks = seq(0,10,1),limits = c(0,10))

hist(logy,breaks = 30)
qqnorm(logy)
qqline(logy, col = 2)

#wald
seq_min = 0;seq_max = 10;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'wald_1_1' = dwald(x,1,1)
  )
plt_dists(plot_df,int_x_axis = T)

n=10000
y = rlnorm(n,0,1)
z = rwald(n,1,1)

skewness(y)
skewness(z)

#chisq
seq_min = 0;seq_max = 10;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'chisq_1' = dchisq(x,1),
         'chisq_2' = dchisq(x,2),
         'chisq_3' = dchisq(x,3),
         'chisq_5' = dchisq(x,5)
  )
plt_dists(plot_df,int_x_axis = T)

#f
seq_min = 0;seq_max = 4;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'f_1_1' = df(x,1,1),
         'f_5_5' = df(x,5,5),
         'f_10_10' = df(x,10,10),
         'f_50_50' = df(x,50,50)
  )
plt_dists(plot_df,int_x_axis = T)

seq_min = 0;seq_max = 4;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'f_5_5' = df(x,5,5),
         'f_5_10' = df(x,5,10),
         'f_5_50' = df(x,5,50),
         'f_5_500' = df(x,5,500)
  )
plt_dists(plot_df,int_x_axis = T)

seq_min = 0;seq_max = 4;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'f_50_10' = df(x,50,10),
         'f_500_10' = df(x,500,10),
         'f_1000_10' = df(x,1000,10)
  )
plt_dists(plot_df,int_x_axis = T)

seq_min = 0;seq_max = 4;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'f_500_10' = df(x,500,10),
         'f_10_500' = df(x,10,500)
  )
plt_dists(plot_df,int_x_axis = T)

seq_min = 0;seq_max = 4;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'f_10_10' = df(x,10,10),
         'f_100_100' = df(x,100,100),
         'f_1000_1000' = df(x,1000,1000)
  )
plt_dists(plot_df,int_x_axis = T)


#weibull
seq_min = 0;seq_max = 5;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'wb_1_1' = dweibull(x,1,1),
         'wb_1_2' = dweibull(x,1,2),
         'wb_1_3' = dweibull(x,1,3)
  )
plt_dists(plot_df,int_x_axis = T)

seq_min = 0;seq_max = 5;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'wb_1_1' = dweibull(x,1,1),
         'wb_0.8_1' = dweibull(x,0.8,1),
         'wb_0.5_1' = dweibull(x,0.5,1),
         'wb_0.2_1' = dweibull(x,0.2,1)
  )
plt_dists(plot_df,int_x_axis = T)

seq_min = 0;seq_max = 5;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'wb_1_1' = dweibull(x,1,1),
         'wb_1.5_1' = dweibull(x,1.5,1),
         'wb_2_1' = dweibull(x,2,1),
         'wb_5_1' = dweibull(x,5,1)
  )
plt_dists(plot_df,int_x_axis = T)

#burr
seq_min = 0;seq_max = 5;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'b_1_1' = dburr(x,1,1,1),
         'b_2_1' = dburr(x,2,1,1),
         'b_1_2' = dburr(x,1,2,1)
  )
plt_dists(plot_df,int_x_axis = T)

#levy
seq_min = 0.1;seq_max = 5;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'lv_1' = dlevy(x,0,1),
         'lv_2' = dlevy(x,0,2),
         'lv_5' = dlevy(x,0,5)
  )
plt_dists(plot_df,int_x_axis = T)

y= seq(1,30,1)
y1=dlevy(y,0,1)
y2=y^(-3/2)
y1;y2;y1/y2

seq_min = 1;seq_max = 10;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'lv_1' = dlevy(x,0,1),
         'pw' = x^(-3/2)*0.392
  )
plt_dists(plot_df,int_x_axis = T)

#gamma
seq_min = 0;seq_max = 10;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'gm_1_1' = dgamma(x,1,scale = 1),
         'gm_1_2' = dgamma(x,1,scale = 2),
         'gm_1_0.5' = dgamma(x,1,scale = 0.5)
  )
plt_dists(plot_df,int_x_axis = T)

seq_min = 0;seq_max = 10;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate('ln_0_1' = dlnorm(x,0,1),
         'gm_1_2' = dgamma(x,1,scale = 2),
         'gm_2_2' = dgamma(x,2,scale = 2),
         'gm_5_2' = dgamma(x,5,scale = 2)
  )
plt_dists(plot_df,int_x_axis = T)

seq_min = 0;seq_max = 10;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate(#'gm_1_1' = dgamma(x,1,scale = 1),
         #'gm_1_2' = dgamma(x,1,scale = 2),
         #'gm_1_3' = dgamma(x,1,scale = 3)
         #'gm_2_1' = dgamma(x,2,scale = 1),
         #'gm_2_2' = dgamma(x,2,scale = 2),
         #'gm_2_3' = dgamma(x,2,scale = 3)
         'gm_3_1' = dgamma(x,3,scale = 1),
         'gm_3_2' = dgamma(x,3,scale = 2),
         'gm_3_3' = dgamma(x,3,scale = 3)
  )
plt_dists(plot_df,int_x_axis = T)

#inverse gamma
seq_min = 0;seq_max = 10;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step)) %>%
  mutate(
    'igm_1_1' = dinvgamma(x,1,1),
    'igm_1_0.5' = dinvgamma(x,1,.5),
    'gm_1_1' = dgamma(x,1,scale = 1)
  )
plt_dists(plot_df,int_x_axis = T)

pgamma(3,1,scale=1)-pgamma(2,1,scale=1)
pinvgamma(1/2,1,1)-pinvgamma(1/3,1,1)

seq_min = 0;seq_max = 10;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step))
for(a in 1){
  for(b in 1:6){
    plot_df[,paste0('igm_',a,'_',b)] = dinvgamma(plot_df[,'x'],a,b)
  }}
plt_dists(plot_df,int_x_axis = T)

seq_min = 0;seq_max = 5;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step))
for(a in c(.3,.5,1,2,3)){
  for(b in 1){
    plot_df[,paste0('igm_',a,'_',b)] = dinvgamma(plot_df[,'x'],a,b)
  }}
plt_dists(plot_df,int_x_axis = T)

#inverse gaussian
seq_min = 0;seq_max = 5;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step))
for(a in 1:3){
  for(b in 1){
    plot_df[,paste0('invg_',a,'_',b)] = dinvgauss(plot_df[,'x'],a,b)
  }}
plt_dists(plot_df,int_x_axis = T)

#exponential
seq_min = 0;seq_max = 5;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step))
for(a in seq(0.5,1.5,0.5)){
  for(b in 1){
    plot_df[,paste0('exp_',a,'_',b)] = dexp(plot_df[,'x'],a)
  }}
plt_dists(plot_df,int_x_axis = T)

#frechet
seq_min = 0;seq_max = 5;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step))
for(a in seq(0.5,1.5,0.5)){
  for(b in 1){
    plot_df[,paste0('fre_',a,'_',b)] = dfrechet(plot_df[,'x'],lambda = a)
  }}
plt_dists(plot_df,int_x_axis = T)

#pareto
seq_min = 0;seq_max = 5;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step))
for(a in seq(1,1,1)){
  for(b in seq(1,3,1)){
    plot_df[,paste0('pare_',a,'_',b)] = dpareto(plot_df[,'x'],a,b)
  }}
plt_dists(plot_df,int_x_axis = T)

#gen pareto
seq_min = 0;seq_max = 5;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step))
for(a in seq(0,3,1)){
  for(b in seq(1,1,1)){
    plot_df[,paste0('gprt_',a,'_',b)] = extraDistr::dgpd(plot_df[,'x'],1,2,a)
  }}
plt_dists(plot_df,int_x_axis = T)

#triangle
seq_min = 0;seq_max = 5;seq_step = 0.1
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step))
for(a in seq(2,2,1)){
  for(b in seq(4,4,1)){
    plot_df[,paste0('tri_',a,'_',b)] = dtriang(plot_df[,'x'],a,b)
  }}
plt_dists(plot_df,int_x_axis = T)

#beta
seq_min = 0;seq_max = 1;seq_step = 0.05
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step))
for(a in seq(1,5,1)){
  for(b in seq(1,1,1)){
    plot_df[,paste0('beta_',a,'_',b)] = dbeta(plot_df[,'x'],a,b)
  }}
plt_dists(plot_df,int_x_axis = T)

seq_min = 0;seq_max = 1;seq_step = 0.05
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step))
for(a in seq(0.2,1,0.2)){
  for(b in seq(1,1,1)){
    plot_df[,paste0('beta_',a,'_',b)] = dbeta(plot_df[,'x'],a,b)
  }}
plt_dists(plot_df,int_x_axis = T)

seq_min = 0;seq_max = 1;seq_step = 0.05
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step))
for(a in seq(0.5,2,0.5)){
  for(b in seq(1,1,1)){
    plot_df[,paste0('beta_',a,'_',a)] = dbeta(plot_df[,'x'],a,a)
  }}
plt_dists(plot_df,int_x_axis = T)

seq_min = 0;seq_max = 1;seq_step = 0.05
plot_df = 
  data.frame(x=seq(seq_min,seq_max,seq_step))
for(a in seq(1,1,1)){
  for(b in seq(0.2,1,0.2)){
    plot_df[,paste0('beta_',a,'_',b)] = dbeta(plot_df[,'x'],a,b)
  }}
plt_dists(plot_df,int_x_axis = T)

