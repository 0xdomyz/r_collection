library(Hmisc)

#sample data
set.seed(1992)
size=1500
x=sample(seq(from=-50,to=50,by=1),size=size,replace = T)
y=sample(c(0,1),prob=c(.95,.05),size=size,replace = T)
summary(x)
table(y)

#somers function
somers0=function(x,y){
  
  res=NA
  
  if(length(x)!=length(y)){return(res)}
  
  size=length(x)
  
  #concordant discordant function
  f_cd=function(x,y){
    res=NULL
    if( (x[1]-y[1])*(x[2]-y[2]) > 0 ){
      res='con'}
    else{
      if( (x[1]-y[1])*(x[2]-y[2]) < 0 ){
        res='dis'}
      else{
        res='tie'}
    }
    res
  }
  
  lst1=
    unlist(
      lapply(combn(seq_len(size),2,simplify = F),
             function(a){
               f_cd(c(x[a[1]],y[a[1]]),
                    c(x[a[2]],y[a[2]]))
             })
    )
  
  lst2=
    unlist(
      lapply(combn(seq_len(size),2,simplify = F),
             function(a){
               f_cd(c(y[a[1]],y[a[1]]),
                    c(y[a[2]],y[a[2]]))
             })
    )
  
  (sum(lst1=='con') - sum(lst1=='dis'))/(sum(lst2=='con') - sum(lst2=='dis'))
  
}

#check
somers0(x,y)
somers2(x,y)

#Appendix###############

#concordant discordant function
f_cd=function(x,y){
  res=NULL
  if( (x[1]-y[1])*(x[2]-y[2]) > 0 ){
    res='con'}
  else{
    if( (x[1]-y[1])*(x[2]-y[2]) < 0 ){
      res='dis'}
    else{
        res='tie'}
    }
  res
  }
#f_cd(c(3,5),c(3,4))

#somerd funct8ion
size=10
x=c(1,2,3,4,5,6,7,8,9,10)
y=c(0,0,1,0,0,0,0,1,1,1)
  
lst1=
unlist(
  lapply(combn(seq_len(size),2,simplify = F),
         function(a){
           f_cd(c(x[a[1]],y[a[1]]),
                c(x[a[2]],y[a[2]]))
         })
)

lst2=
  unlist(
    lapply(combn(seq_len(size),2,simplify = F),
           function(a){
             f_cd(c(y[a[1]],y[a[1]]),
                  c(y[a[2]],y[a[2]]))
           })
  )

(sum(lst1=='con') - sum(lst1=='dis'))/(sum(lst2=='con') - sum(lst2=='dis'))


