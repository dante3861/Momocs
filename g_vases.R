library(Momocs)
library(dendextend)
library(magrittr)
library(data.table)
library(fdasrvf)
library(ggplot2)
library(ggdendro)
library(ape)
library(BiocManager)
library(ggtree)
library(dplyr)
library(phytools)
library(ggimage)
library(tidytree)
library(multiplex)

options(scipen = 200)
r<-fread("Athenian_BF_test_1.csv")
r<-r[,c("x","y","order")]
r<-split(r,r$order)
m<-list()

for (i in 1:322) {
  m[[i]]<-min(r[[i]][,c(1)])
}

r<-lapply(r,function(x){(x[,c(1,2)])})
df<-list()
for (i in 1:322) {
  df[[i]]<-data.frame(x=rev(c(2*m[[i]]-r[[i]]$x)), y=rev(c(r[[i]]$y)))
  r[[i]]<-rbind(r[[i]],(df[[i]]))
}

r<-lapply(r,function(x){as.matrix(x[,c(1,2)])})
#r<-Out(r)

length(r)
sapply(r,class)

fa<-fread("key_to_hans_testgroups.csv")
fa <- fa[,c(10,11)]
fac<-data.frame(type = fa$type, group = fa$subtype)
r<-Out(r,fac)

r %>% stack()
r<-r %>% coo_centre
r<- r%>% coo_scale()
#r<- coo_align(r)
#r<-coo_slidedirection(r,"right") %>% coo_untiltx() 
r<-r%>%coo_rotate(pi)
stack(r)
panel(r)



r.f<-efourier(r,nb.h = 10,norm = TRUE)
r.fr<-rfourier(r,nb.h = 10,norm = TRUE)
r.ft<-tfourier(r,nb.h = 10, norm = TRUE)
r.st<-sfourier(r,nb.h = 10)



r.pr<-PCA(r.fr)
r.pt<-PCA(r.ft)
r.ps<-PCA(r.st)
r.p<-PCA(r.f)

PCcontrib(r.p, nax = 1:3)
PCcontrib(r.pr, nax = 1:3)
PCcontrib(r.pt, nax = 1:3)

MSHAPES(r.f) %>% coo_plot()
MSHAPES(r.fr) %>% coo_plot()
MSHAPES(r.ft) %>% coo_plot()
MSHAPES(r.st)%>% coo_plot()

plot_PCA(r.p,points = TRUE, points_transp = 0.1, morphospace = FALSE, chull = FALSE, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)

plot_PCA(r.pr,~type,points = TRUE, points_transp = 0, morphospace = FALSE, chull = TRUE, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)

plot_PCA(r.pt,points = TRUE, points_transp = 0.1, morphospace = FALSE, chull = FALSE, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)
plot_PCA(r.ps,points = TRUE, points_transp = 0.1, morphospace = FALSE, chull = FALSE, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)
KMEANS(r.p,centers=17,nax=1:2, cex = 1.8)
KMEANS(r.pr,centers=17,nax=1:2, cex = 1.8)
KMEANS(r.pt,centers=17,nax=1:2, cex = 1.8)
KMEANS(r.ps,centers=17,nax=1:2, cex = 1.8)
CLUST(r.f,type = "fan", cex=1,hclust_method = "complete")
CLUST(r.fr,type = "fan", cex=0.2,hclust_method = "complete", ~type, palette = pal_manual(c("green","yellow","red","purple","pink","orange","grey","blue")))
CLUST(r.ft,type = "fan", cex=1,hclust_method = "complete")
CLUST(r.st,type = "fan", cex=1,hclust_method = "complete")



# clear data: get rid of wrong contours, draw whole contours from half
## get rid of bad contours
df<-as_df(r)
v<-df$coo
b<-list()
c<-list()
e<-list()
f<-list()
order<-list()
a<-fread("Athenian_BF_test_1.csv")
a<-a$image_id %>% unique()
a<-a[-123] # here a[123] is a bad extracted contour
## draw symmetric contour from half
for (i in 1:322) {
  c[[i]]<-(rep(a[i],times = length(v[[i]])/2))
  
}

for (i in 1:322){
  e[[i]]<-(rep(df$type[i],times = length(v[[i]])/2)) %>% as.character()
  order[[i]]<-rep(i,times = length(v[[i]])/2)
  f[[i]]<-(rep(df$group[i],times = length(v[[i]])/2))
}


for (i in 1:322) {
  v[[i]]<-cbind(v[[i]],c[[i]],e[[i]],f[[i]],order[[i]])
  
}



for (i in v){
  i %>% write.csv(file = "G.csv", row.names = TRUE ,col.names = TRUE)
}

for (i in 1:322) {
  v[[i]] %>% write.table('T_0.1.csv',append = TRUE, sep = ',')
}
lapply(v, function(x) write.table( data.frame(x), 'G.csv'  , append= T, sep=',' ))
lapply(a, function(x)write.table(data.frame(x),'G.csv',append = T, sep = ','))

g<-fread("G.csv")
g6<-g$V6
g6<-g6%>%as.list()

###################





# clust e-fourier result
r.ef<-r.f %>% as_df()
r.ef_1<-r.ef[,c(3:22)]
d.e<-r.ef_1 %>% dist()
dre<-d.e %>% hclust()
#drc<- drc %>% as.dendrogram()
dre<-as.phylo(dre)
#drc$tip.label<-r$group
#x<-list()
#x<-getStates(drc3,"tips")
cols <- c("red","yellow","#6A1B9A","orange","#C5CAE9","#4DD0E1","#00ACC1","#006064","#18FFFF","#B2DFDB","#64FFDA","#004D40","#A5D6A7","#4CAF50","pink","#C5E1A5","#7CB342","#B2FF59","#76FF03","FFF59D","#F57F17","#BCAAA4","#6D4C41")
#fitER <- ape::ace(drc$tip.label,drc,model="ER",type="discrete")
#ancstats <- as.data.frame(drc2$group)
#ancstats$node <- 1:drc$Nnode+Ntip(drc)
## cols parameter indicate which columns store stats
#pies<-nodepie()
ti<-tibble(label = c(1:322) %>% as.character(),group = r$group %>% as.character(), type = r$type %>% as.character())
ti_dre<-as_tibble(dre)
shp<-c(16,8,11,7,3,17,18)
group_1<-tibble(label = c(1:322) %>% as.character, shpp = c(rep(1,10),2,2,2,rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(7,6),rep(1,10),rep(1,10),rep(2,10),rep(3,4),rep(4,18),rep(5,10),rep(6,10),rep(7,10),rep(1,10),rep(2,8),rep(1,3),rep(1,10),rep(1,10),rep(1,9),rep(1,9),rep(2,8),rep(3,10),rep(4,10),rep(1,8),rep(1,10),rep(1,10),rep(1,8),rep(1,9),rep(2,10),rep(1,9),rep(1,10)) %>% as.character())

dre2 <- full_join(ti_dre, ti, by = 'label')
dre2<-full_join(dre2,group_1, by = "label")
#as.treedata(drc2)
for (i in 1:322) {
  dre2$label[i] <- dre2$group[i]
  
}
dre3<-dre2 %>% as.phylo()
dre4<-dre2 %>% as.treedata()

#p + geom_inset(bars, width = .08, height = .05, x = "branch") 


cols1<-c("cadetblue","aliceblue","antiquewhite","aquamarine","chartreuse","chartreuse4","chocolate","cyan","blue","burlywood4","violet","darkblue","brown4","darkgoldenrod1","darkgreen","darkorchid1","darkolivegreen1","hotpink","red","orange","yellow")
ggtree(dre4,layout = "fan") + 
  scale_shape_manual(values = shp) +
  scale_color_manual(values = cols1)+
  geom_tippoint(aes(color = type, shape = shpp),size=0.7)


