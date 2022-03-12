library(stats)
library(dplyr)
library(ggplot2)
library(moments)
library(rmutil)
library(actuar)
library(extraDistr)
options(stringsAsFactors = FALSE)


a=matrix(c(1,2,1, 4,5,4, 9,8,7), nrow = 3, ncol = 3)
det(a)
b=solve(a)

a;b;a%*%b


#contour
data(volcano)
head(volcano)
dim(volcano)

contour(volcano)

as.data.frame(volcano) %>% #convert the matrix to data frame
  rownames_to_column() %>% #get row coordinates
  gather(key, value, -rowname) %>% #convert to long format
  mutate(key = as.numeric(gsub("V", "", key)), #convert the column names to numbers
         rowname = as.numeric(rowname)) %>%
  ggplot() +
  geom_contour(aes(x = rowname, y = key, z = value))

as.data.frame(volcano) %>%
  rownames_to_column() %>%
  gather(key, value, -rowname) %>%
  mutate(key = as.numeric(gsub("V", "", key)),
         rowname = as.numeric(rowname)) %>%
  ggplot() +
  geom_contour(aes(x = rowname,
                   y = key,
                   z = value,
                   colour = ..level..)) -> some_plot

head(mpg)
a=reshape2::melt(mpg,id=c('manufacturer','model'),measure=c('cty','hwy'),
                 variable.name = 'var',measure.name='val')
a=tidyr::gather(mpg,'var','val',cty,hwy)

head(a)
b=reshape2::dcast(a,manufacturer+model~variable)
head(b)

#https://stackoverflow.com/questions/50242331/r-ggplot2-contour-plot
#https://ggplot2.tidyverse.org/reference/geom_contour.html
#https://www.r-statistics.com/2016/07/using-2d-contour-plots-within-ggplot2-to-visualize-relationships-between-three-variables/
#http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization
