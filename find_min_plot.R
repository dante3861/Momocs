library(Momocs)
## Here we use a small function to find least number of plots describing a contour without losing 1% perimeter,
## which means 99% perimeter accuracy 

# subset a group of vases and transform into data type can be recognized by Momocs
long<-read.csv("RW_vases_long.csv")
long_1<-long[,c("x","y","vaseseries","timestep")]
head(long_1)
long_1.1<-subset(long_1,long_1$vaseseries==1)
head(long_1.1)
a<-split(long_1.1,long_1.1$timestep)
head(a)
b<-lapply(a, function(x){as.matrix(x[, c(1,2)])})
head(b)
class(b[1])
o<-Out(b)
o

# check data type
o[1] %>% is_shp()
o[1] %>% head()
o %>% class()
o %>% is.list
o$coo %>% class()

p<-c()
for (i in 1:200){
  p[i] = o[i] %>% coo_perim()
}

p
print(max(p))
q=0.99*max(p)
o[which.max(p)]

for (i in 4:191){
  if(o[which.max(p)] %>% coo_sample(i) %>% coo_perim()>q){
    print(i)
    o[which.max(p)] %>% coo_sample(i) %>% coo_plot()
    break
  }
}