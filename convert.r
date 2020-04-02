library(geomorph)
library(data.table)
library(multiplex)
library(crayon)
r<-fread("PC_RW_large_Mantamados_closed.csv")
r_l<-split(r,list(r$run,r$timestep),drop = TRUE)
r1<-lapply(r_l,function(x){as.matrix(x[,c(4,5)])})
r_c<-r[,4:5]

#try write.dat
### turn?vases id into alphabet for Mathematica using, a==0,b==1,c==2,....,j==9
id<-unique(r$virtualvase)
id_1<-list()
id_a<-list()
for (i in 1:100001) {
  id_1[i]<-strsplit(id[i],"_")
  id_1[[i]][1]<-id_1[[i]][1]%>%strsplit("")
  id_1[[i]][[1]]<-id_1[[i]][[1]]%>%a?.numeric()
  id_1[[i]][[2]][1]<-id_1[[i]][[2]][1]%>%strsplit("")
  id_1[[i]][[2]][[1]]<-id_1[[i]][[2]][[1]]%>% as.numeric()
  id_a[[i]]<-paste(letters[id_1[[i]][[1]]+1]%>%paste(sep = "",collapse = ""), letters[id_1[[i]][[2]][[1]]+1] %>% paste(sep = "",coll?pse = ""))
}


v<-data.frame()
for (i in 1:100001) {
  v<-rbind(v,id_a[[i]],r_c[((i-1)*140+1) : (i*140),],fill=TRUE)
  print(i)
}

write.dat(v,"v.dat")
#note that the out put may including "" and NA, please get rid of them using editer for Mathematica read?ng
