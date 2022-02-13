
#sharp shooter D3 demon hunter talent
library(parallel)
transpose=t

ss_sim=function(n,t,cc,incre){

v_ct=vector(mode = 'numeric',length = n)
v_cc=rep_len(cc,length.out = n)
v_ced=vector(mode = 'logical',length = n)

for (i in seq_len(t)){
  #update cc
  v_cc[v_ced==TRUE]=cc
  v_cc[v_ced==FALSE]=v_cc[v_ced==FALSE]+incre
  #simulate
  v=runif(n)
  #result
  v_ced[v<v_cc]=TRUE
  v_ced[v>=v_cc]=FALSE
  v_ct[v_ced]=v_ct[v_ced]+1
}

return(
  c(
    mean(v_ct/t)+2*sqrt(var(v_ct/t)),
    mean(v_ct/t),
    mean(v_ct/t)-2*sqrt(var(v_ct/t))
  )
)

}

#improvements

#for loop
# res=matrix(ncol = 3,nrow = 100)
# for(i in seq_len(100)){
#   cat('doing',i,'\n')
#   res[i,]=ss_sim(n = 1000,t = 5000,cc = i/100,incre = .04)
# }

#lappy
# res=
# lapply(seq_len(100),function(i){
#   cat('doing',i,'\n')
#   ss_sim(n = 1000,t = 5000,cc = i/100,incre = .04)
# })
# res=transpose(matrix(unlist(res),nrow = 3))

#parallel
cl <- makeCluster(5)
clusterExport(cl, "ss_sim")
res=
parLapply(cl,seq_len(100),
          function(i){
            cat('doing',i,'\n')
            ss_sim(n = 1000,t = 5000,cc = i/100,incre = .04)
          })
res=transpose(matrix(unlist(res),nrow = 3))
stopCluster(cl)

#results
#View(res)
plot(x = seq_len(100)/100,y=res[,2]-seq_len(100)/100)
lines(x = seq_len(100)/100,y=res[,1]-seq_len(100)/100)
lines(x = seq_len(100)/100,y=res[,3]-seq_len(100)/100)


#deepdive

n=1000
t=200
cc=.2
incre=.04

m_v=matrix(nrow = n,ncol=t)
m_cc=matrix(nrow = n,ncol=t)
m_ced=matrix(nrow = n,ncol=t)

v_ct=rep_len(0,length.out = n)
v_cc=rep_len(cc,length.out = n)
v_ced=rep_len(FALSE,length.out = n)

for (i in seq_len(t)){
  #update cc
  v_cc[v_ced==TRUE]=cc
  v_cc[v_ced==FALSE]=v_cc[v_ced==FALSE]+incre
  #simulate
  v=runif(n)
  #result
  v_ced[v<v_cc]=TRUE
  v_ced[v>=v_cc]=FALSE
  v_ct[v_ced]=v_ct[v_ced]+1
  #record
  m_v[,i]=v
  m_cc[,i]=v_cc
  m_ced[,i]=v_ced

  cat('did',i,' ',mean(v_cc),'\n')
}

c(
  mean(v_ct/t)+2*sqrt(var(v_ct/t)),
  mean(v_ct/t),
  mean(v_ct/t)-2*sqrt(var(v_ct/t))
)
sum(m_ced)/n/t

cat(' samples:',n,'\n',
    'length:',t,'\n',
    'cc',cc,'\n',
    'incre',incre,'\n')
hist(v_ct/t,main = 'Crit chance histogram')
plot(x = seq_len(t),y = m_v[1,])
lines(x = seq_len(t),y = m_cc[1,])
#
# lst=
# lapply(seq_len(t),function(i){
#   #update cc
#   v_cc[v_ced==TRUE]=cc
#   v_cc[v_ced==FALSE]=v_cc[v_ced==FALSE]+incre
#   #simulate
#   v=runif(n)
#   #result
#   v_ced[v<v_cc]=TRUE
#   v_ced[v>=v_cc]=FALSE
#   cat('did',i,' ',mean(v_cc),'\n')
#   v_ced
# })
# m=matrix(unlist(lst),nrow = n)
# sum(m)/t/n
################################################################
#can't use lapply to do state dependent simulation???###########
################################################################


