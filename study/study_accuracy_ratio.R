library(Hmisc)
library(dplyr)
library(ggplot2)

f=function(n,v,out='cat'){
#n=10
#v=c(1,2)
x=seq(1,n,1)
s=rep(0,n)
s[v]=1
plot(x=x,y=s)

rc=rcorr.cens(x,s)
if(out=='cat'){
  cat(sum(s==1),'\n')
  cat(paste0('Dxy: ',round(rc['Dxy']*100,2),', ',
             'Pair: ',rc['Relevant Pairs'],', ',
             'Conc: ',rc['Concordant'],', ','\n'))
}else{
  return(round(rc['Dxy']*100,2))
}

}

n=5;n*(n-1)/factorial(2)
n=100;n*(n-1)*(n-2)*(n-3)*(n-4)/factorial(5)

f(n=100,v=c(100,99,98,97,90))
f(n=100,v=c(100,99,98,97,50))
f(n=100,v=c(100,99,98,97,10))
f(n=100,v=c(100,99,98,97,1))
f(n=100,v=c(96,91,85,72,71))
f(n=100,v=c(55,54,53,52,51))
f(n=100,v=c(100,54,53,52,51))
f(n=100,v=c(90,54,53,52,51))
f(n=100,v=c(90,91,92,93,1))
f(n=100,v=c(90,91,92,2,1))
f(n=100,v=c(70,71,72,73,1))

#q
library(Hmisc)
x=seq(1,4,1)
s=rep(0,4)
s[c(2,4)]=1

rcorr.cens(x,s)


#simulation
choose(100,5)

n=6*100000
s_df=data.frame(
  row_id = 0,
  case_id = 0,
  itm_id = 0,
  itm=round(runif(n,1,100),0)
  #itm_r=round(runif(n,1,100),0),
  #itm_r1=ceiling(sqrt(runif(n,0,1))*100),
  #itm_r2=ceiling(100-sqrt(runif(n,0,1))*100),
  #itm_r0=ceiling(runif(n,0,3))
)

#s_df=s_df %>%
#  mutate(itm=ifelse(itm_r0==1,itm_r,
#                ifelse(itm_r0==2,itm_r1,
#                       ifelse(itm_r0==3,itm_r2,NaN))))
s_df$row_id=seq(1,n,1)
s_df$case_id=ceiling(s_df$row_id/5)

s_df = 
  s_df %>%#remove dups
  group_by(case_id) %>%
  summarise(cnt=n_distinct(itm)) %>%
  ungroup() %>%
  filter(cnt == 5) %>%
  select(case_id) %>%
  left_join(s_df,by='case_id')

nrow(s_df)
n2=nrow(s_df)

s_df$row_id=seq(1,n2,1)
s_df$case_id=ceiling(s_df$row_id/5)
s_df$itm_id=(s_df$row_id - 1) %% 5 + 1

nrow(s_df)
s_df[seq(1,10,1),]

s_df_sum = s_df %>%
  group_by(case_id) %>%
  summarise(itm1=min(itm),
            itm2=sort(itm)[2],
            itm3=sort(itm)[3],
            itm4=sort(itm)[4],
            itm5=max(itm)) %>%
  ungroup()

#s_df_sum=s_df_sum[seq(1,10,1),]
s_df_sum$ar=
mapply(function(x1,x2,x3,x4,x5){
  f(n=100,
    v=c(x1,x2,x3,x4,x5),
    out='res')},
       x1=s_df_sum$itm1,
       x2=s_df_sum$itm2,
       x3=s_df_sum$itm3,
       x4=s_df_sum$itm4,
       x5=s_df_sum$itm5)
  
nrow(s_df_sum)
head(s_df_sum)
s_df %>% filter(case_id == 1)
s_df_sum %>% filter(ar>90) %>% nrow()
hist(s_df_sum$ar)

s_df_sum %>% filter(ar>0 & ar<10) %>% select(itm1) %>% hist()


save(s_df_sum,file = 's_df_sum.rData')
save(s_df,file = 's_df.rData')
load('s_df_sum.rData')
load('s_df.rData')


