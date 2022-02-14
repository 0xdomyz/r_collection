#na remove
class(dat)=='data.frame'
length(dat)
na_idx = is.na(dat[,cols])
na_rows = rowSums(na_idx) > 0
dat[!na_rows,]
dat[na_rows,]
pct_na_row = round(sum(na_rows)/nrow(dat)*100, 2)


#extreme value remove
q_low = quantile(dat[,cols],pct_low)
q_high = quantile(dat[,cols], 1 - pct_high)
ext_rows_low = dat[,cols] < q_low
ext_rows_high = dat[,cols] > q_high
ext_rows = ext_rows_low | ext_rows_high


#checksum
tools:md5sum('path')

library(digest)
digest(str,algo='md5',serialize=F)


#checkpoint
library(checkpoint)
checkpoint('2020-01-01',verbose=T)


#regexp
tbl <- readr::read_csv('path') %>% as.data.frame
names(tbl) <- tolower(names(tbl))

res <- tbl %>%
    filter(!is.na(id)) %>%
    mutate(
        nc = gsub("(\\b[A_Z]{1,3})|.",'\\1', desc, perl = TRUE)
    )

grep('^n_(\\w[^\\d].+)$',stuff,value=T,perl=T)


#factor
factor(col_a, levels = c('a','b'))


#read from database
options(stringAsFactors = F)
cons = paste('asdfasd',Sys.getenv('sdf'),sep='')
con = RODBC::odbcDriverConnect(cons)
dat = RODBC::sqlquery(con, 'select * from dual')
RODBC::odbcClose(con)
colnames(dat) = tolower(colnames(dat))

runqry = function(qry,cons){
    con = RODBC::odbcDriverConnect(cons)
    dat = RODBC::sqlquery(con, qry)
    RODBC::odbcClose(con)
    colnames(dat) = tolower(colnames(dat))
    return(dat)
}


#write into database
RODBC::sqlSave(chanel, dat, tablename = 'AAA', rownames = FALSE,
    fast = FALSE)
RODBC::odbcClose(channel)


#read file
readr::read_file('path')
dat = readr::read_csv('path') %>% as.data.frame
read.delim('clipboard',header = F)
readxl::read_excel('path', sheet = 'sht')

names(dat) = tolower(names(dat))


#write file
write.table(dat, "clipboard", sqp = '\t', row.names = FALSE,
    col.names = FALSE, na = '')


#save load
save(get(nme), file = 'path.RData')
load('path.RData')


#file system manipulation
file.copy(from, to, overwrite = recursive, recursive = FALSE,
    copy.mode = TRUE, copy.date = FALSE)
dir.create('folder')
unlink('asdf.csv')
unlink('dir',recursive = TRUE)


#rlm
m = rlm(dat[,nmes] ~ dat[,nme])
SSe = sum(m$w*(m$resid)^2)
obs = m$resid + m$fitted
SSt = sum(m$w*(obs - weighted.mean(obs,m))^2)
m_a = coef(m)[1]
m_b = coef(m)[2]
m_r2 = 1-SSe/SSt
fml = paste('y = ',m_a,' + ',m_b,' * x ', sep='')
m$converged


#lm
m = lm(x ~ y)
coef(m)
summary(m)
cor(x,y,method='pearson')
rcorr.cens(x,y)['Dxy']


#misc
-Inf, Inf
eval(parse(text = words))
order()
pmax()
subset()
View()






