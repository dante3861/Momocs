long<-read.csv("RW_vases_long.csv")
wide<-read.csv("RW_vases_wide.csv")
install.packages("devtools")
devtools::install_github("MonX/Momocs")
devtools::install_github("MomX/Momit",bulid_vignettes=TRUE)
browseVignettes("Momit")
library(Momocs)
long_1<-long[,c("x","y","vaseseries","timestep")]
head(long_1)
long_1.1<-subset(long_1,long_1$vaseseries==1)
head(long_1.1)
a<-split(long_1.1,long_1.1$timestep)
head(a)
b<-lapply(a, function(x){as.matrix(x[, c(1,2)])})
head(b)
o<-Out(b)
o

o[1] %>% is_shp()
o[1] %>% head()
o %>% class()
o %>% is.list
o$coo %>% class()

o %>% stack
o %>% coo_center %>% stack()
panel(o)
coo_oscillo(o[1],"efourier")
Ptolemy(o[1])
o.f<-efourier(o,nb.h = 10)
o.f
boxplot(o.f)
o.p<- PCA(o.f)
plot(o.p)

pile(o)
options(Momocs_verbose = FALSE)
o.al<-fgProcrustes(o)
pile(o.al)

ot<-measure(o,coo_area,coo_circularity,d(1,3))
ot %>% PCA() %>% plot_PCA()

KMEANS(o.p,centers = 5)
o.f%>%MSHAPES()%>% coo_plot()