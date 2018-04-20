install.packages('phytools')
library(phytools)

EURASIA PLOSIVES

new=readChar('/Users/jercol/Documents/Epicentres/glottologtree.txt',file.info('/Users/jercol/Documents/Epicentres/glottologtree.txt')$size)
newtree=read.tree(text=new)
newtree$edge.length[newtree$edge[,1]==480]=10
newtree$edge.length[!newtree$edge[,1]==480]=0.0001
plot(newtree,type='radial')

traits=fastBM(newtree,internal=T)
traits=traits-min(traits)
traits=traits/max(traits)
traits=ceiling(traits*25)

contMap(newtree,traits[1:length(newtree$tip)])

epicentersdata=read.csv("~/Documents/Epicentres/epicentersworld.txt",header=T,stringsAsFactors=F,sep='\t')
epicentersdata=epicentersdata[epicentersdata[,3]<85,]
epicentersdata=epicentersdata[epicentersdata[,3]>-10,]
epicentersdata=epicentersdata[epicentersdata[,2]>20,]
epicentersdata[,6]=epicentersdata[,12]
values=rep(NA,length(epicentersdata[,6]))
for (member in 1:length(epicentersdata[,6])){
  if (epicentersdata[member,8] %in% newtree$tip){
    values[member]=traits[which(names(traits)==epicentersdata[member,8])]
  }  
}
epicentersdata$values=values
epicentersdata=epicentersdata[!is.na(epicentersdata$values),]
map("worldHires",xlim=c(-10,90), ylim=c(20,75))
min=min(epicentersdata[,6])
max=sort(epicentersdata[,6],decreasing=T)[4]
if (length(sort(epicentersdata[,6],decreasing=T)[sort(epicentersdata[,6],decreasing=T)>max])<3){
  max=unique(sort(epicentersdata[,6],decreasing=T))[which(unique(sort(epicentersdata[,6],decreasing=T))==max)+1]
}
for (n in (min:max)){
  drawcurve(n,heat.colors(max-min,alpha=0.2)[n-min+1])
}
real=c()
drawcurve(23,'blue')
for (n in unique(sort(epicentersdata[,6]))[!unique(sort(epicentersdata[,6]))>max]){
  testpts=structure(list(x=epicentersdata[,3][epicentersdata[,6]>n],y=epicentersdata[,2][epicentersdata[,6]>n]))
  points(testpts,pch=1,col='black')
  chuld <- lapply(testpts,"[",chull(testpts))
  polygon(chuld,lty=2,border='white')
  polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2)
  
  real=append(real,area(spline.poly(as.matrix(as.data.frame(chuld)),100)))
} 
real
significances=rep(0,length(real))
#comparison with randomly generated data
for (turn in 1:100){
  traits=fastBM(newtree,internal=T)
  traits=traits-min(traits)
  traits=traits/max(traits)
  traits=ceiling(traits*25)
  epicentersdata=read.csv("~/Documents/Epicentres/epicentersworld.txt",header=T,stringsAsFactors=F,sep='\t')
  epicentersdata=epicentersdata[epicentersdata[,3]<85,]
  epicentersdata=epicentersdata[epicentersdata[,3]>-10,]
  epicentersdata=epicentersdata[epicentersdata[,2]>20,]
  epicentersdata[,6]=epicentersdata[,12]
  values=rep(NA,length(epicentersdata[,6]))
  for (member in 1:length(epicentersdata[,6])){
    if (epicentersdata[member,8] %in% newtree$tip){
      values[member]=traits[which(names(traits)==epicentersdata[member,8])]
    }  
  }
  epicentersdata$values=values
  epicentersdata=epicentersdata[!is.na(epicentersdata$values),]
  epicentersdata[,6]=epicentersdata$values
  fake=c()
  min=min(epicentersdata[,6])
  max=sort(epicentersdata[,6],decreasing=T)[4]
  if (length(sort(epicentersdata[,6],decreasing=T)[sort(epicentersdata[,6],decreasing=T)>max])<3){
    max=unique(sort(epicentersdata[,6],decreasing=T))[which(unique(sort(epicentersdata[,6],decreasing=T))==max)+1]
  }
  map("worldHires",xlim=c(-10,90), ylim=c(20,75))
  for (n in unique(sort(epicentersdata[,6]))[!unique(sort(epicentersdata[,6]))>max]){
    testpts=structure(list(x=epicentersdata[,3][epicentersdata[,6]>n],y=epicentersdata[,2][epicentersdata[,6]>n]))
    points(testpts,pch=1,col='black')
    chuld <- lapply(testpts,"[",chull(testpts))
    polygon(chuld,lty=2,border='white')
    polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2)
    
    fake=append(fake,area(spline.poly(as.matrix(as.data.frame(chuld)),100)))
  } 
  
  
  for (n in 1:min(length(fake),length(real))){
    if (!abs(rev(real)[n])<abs(rev(fake)[n])){
      significances[n]=significances[n]+1
    }
  } 
}
significances





