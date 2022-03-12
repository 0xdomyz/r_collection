#wide table
dat = data.frame(
    cat = c(1,1,1,2,2),
    id = c(1,2,3,4,5),
    cnt = c(11,22,33,44,55)
)

#long table
long <- reshape2::melt(
    data = dat,
    id.vars = c('id'),
    measure.vars = c('cnt'),
    variable.name = 'var',
    value.name = 'val'
)

#wide
wide <- reshape2::dcast(
    data = long,
    formula = id+cat~cnt,
    value.var = 'val'
)

#long
long2 <- tidyr::gather(
    data = wide,
    key = 'var',
    value = 'val',
    c('cnt')
)


