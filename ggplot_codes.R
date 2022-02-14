library(ggplot2)

ggplot(data = dt)


#geom
geom_point(size = 4)

geom_line(aes(x=,y=,color=),size=))
geom_line(aes(y=mean(),color='b'))

geom_area(aes(x = x, y = y, fill = type, colour = type),
    position = 'identity', alpha = 0.5)

geom_bar(aes(x = aas.factor(x), y = y, fill = Data),
    stat = 'identity', position = 'dodge', width = 0.5
)

geom_abline(intercept = 0, slope = 1)


#texts
xlab("data")
ylab("% of x")
ggtitle("y by x (%)")
ggtitle()

guides(colour = guide_colourbar(
    title = 'y (k)', order = 1, nbin = 20, raster = FALSE
))


#scale axis
scale_colour_manual(values = palette1)
scale_color_gradient(high = 'red') +
    geom_abline(intercept = 0, slope = 1, linetype = 'dashed',
        size = 0.75)+
    xlab('x') + ylab('y') +
    xlim(c(1:10,'NA')) + ylim(c(1:10, 'NA'))
scale_x_continuous(breaks = seq(1, 10, 1), minor_breaks = NULL)
scale_y_continuous(breaks = seq(), limits = c(), labels = scales::percent)
scale_x_date(date_breaks = '3 month', date_minor_breaks = '3 month',
    date_labels = '%y %b')
scale_y_continuous(label = scales::percent)


#theme
plot.title = element_text(size = 15, face = 'bold')
strip.text = element_text(size = 20)
axis.title
axis.text
axis.line

legend.title = element_blank()
legend.position = 'bottom'

theme(
    plot.title = element_text(size = 10),
    ...
)


#save
ggsave(paste(title_n, '.jpeg', sep = ''),
    plt, height = 7, width = 10)


#palette
numColors = length(unique(col))
myPalette = brewer_pal(type = 'qual', palette = 3)(7)
myPalette <- rep(myPalette[1:7],10)[1:numColors]
names(myPalette) <- sort(unique(col))

myPallete = c('000000', '111111')


#multiple
par(mfrow=c(2,1))
plot()
plot()


#lables
labs(
    title = '',
    subtitle = '',
    caption = ''
)


#misc
stat_smooth(method = 'lm', col = 'black', fullrange = T)
coord_cartesian(xlim = , ylim = )
coord_flip()
sub()
union()
sapply()

