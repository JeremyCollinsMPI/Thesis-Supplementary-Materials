install.packages("picante")
install.packages("ape")
install.packages("adephylo")
install.packages("ade4")
install.packages("phylobase")
install.packages("geiger")
install.packages("phytools")
install.packages('maps')
install.packages('vegan')
install.packages('ppcor')
install.packages(gdata)

library("ppcor")
library("picante")
library("ape")
library("adephylo")
library("ade4")
library("phylobase")
library("geiger")
library("phytools")
library('maps')
library('vegan')
library("gdata")

#DATA
#glottolog tree
new=readChar('/Users/jercol/Documents/Epicentres/glottologtree.txt',file.info('/Users/jercol/Documents/Epicentres/glottologtree.txt')$size)
newtree=read.tree(text=new)
newtree$edge.length[newtree$edge[,1]==480]=10
newtree$edge.length[!newtree$edge[,1]==480]=0.0001
#phonotactics data
epicentersdata=read.csv("~/Documents/Epicentres/epicentersworld.txt",header=T,stringsAsFactors=F,sep='\t')


#FUNCTIONS
jsample=function(x,y,probs=NULL,replace=FALSE){
  if(length(x)==1){
    return(x)
    
  }
  if(length(x)>1){
    return(sample(x,y,replace=replace,prob=probs))
  }
}

spline.poly <- function(xy, vertices, k=3, ...) {
  # Assert: xy is an n by 2 matrix with n >= k.
  
  # Wrap k vertices around each end.
  n <- dim(xy)[1]
  if (k >= 1) {
    data <- rbind(xy[(n-k+1):n,], xy, xy[1:k, ])
  } else {
    data <- xy
  }
  
  # Spline the x and y coordinates.
  data.spline <- spline(1:(n+2*k), data[,1], n=vertices, ...)
  x <- data.spline$x
  x1 <- data.spline$y
  x2 <- spline(1:(n+2*k), data[,2], n=vertices, ...)$y
  
  # Retain only the middle part.
  cbind(x1, x2)[k < x & x <= n+k, ]
}

areaPolygonkm=function(x){
  areaPolygon(x)/1000000
}

drawcurve=function(min,colour){
  testpts=structure(list(x=epicentersdata[,3][epicentersdata[,6]>min],y=epicentersdata[,2][epicentersdata[,6]>min]))
  points(testpts,col='black',pch=16,cex=0.5)
  chuld <- lapply(testpts,"[",chull(testpts))
  polygon(chuld,lty=2,border=colour)
  polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border=colour,lwd=2,col=colour)
  points(epicentersdata[,3],epicentersdata[,2],col='black',pch=16,cex=0.5)
  areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100))  
}

generaldrawcurve=function(min,colour,dataframe,lat,lon,value){
  testpts=structure(list(x=dataframe[,lon][dataframe[,value]>min],y=dataframe[,lat][dataframe[,value]>min]))
  points(testpts,col='black',pch=16,cex=0.5)
  chuld <- lapply(testpts,"[",chull(testpts))
  polygon(chuld,lty=2,border=colour)
  polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border=colour,lwd=2,col=colour)
  points(dataframe[,lon],dataframe[,lat],col='black',pch=16,cex=0.5)
  areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100))  
}

humidity=function(x){
  return(climate$specH.mean[which(climate$Language==x)[1]])
}

autotyp=function(x){
  return(climate$Autotyp.area[which(climate$Language==x)[1]])
}

drawcurvearoundfamily=function(x,colour){
  testpts=structure(list(x=epicentersdata[,3][epicentersdata[,7]==x],y=epicentersdata[,2][epicentersdata[,7]==x]))
  points(testpts,pch=1,col='black')
  chuld <- lapply(testpts,"[",chull(testpts))
  polygon(chuld,lty=2,border=colour)
  polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border=colour,lwd=2,col=colour)
  areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100))  
}

pickrandomtip=function(newtree){
  nodes=c()
  current=newtree$edge[1]
  while(length(which(newtree$edge[,1]==current))>0){
    current=newtree$edge[jsample(which(newtree$edge[,1]==current),1),2]
  }
  newtree$tip[current]
  return(newtree$tip[current])  
}

#MAPS

#AFRICA

#load data
epicentersdata=read.csv("epicentersafrica.txt",header=F,stringsAsFactors=F,sep='\t')

#map
map("world",xlim=c(-20,60), ylim=c(-40,40))
points(epicentersdata[,3],epicentersdata[,2],pch=16,cex=0.5)
min=1
max=4
drawcurve(1,heat.colors(max-2,alpha=0.4)[1])
for (n in (min:max)){
  drawcurve(n,heat.colors(max-min,alpha=0.4)[n-min+1])
}

#better map
min=min(epicentersdata[,6])
max=max(epicentersdata[,6])
n=2
drawcurve(n,heat.colors(max-min,alpha=0.4)[n-min+1])
n=3
drawcurve(n,heat.colors(max-min,alpha=0.4)[n-min+1])
n=4
drawcurve(n,heat.colors(max-min,alpha=0.9)[n-min+1])
n=5
drawcurve(n,heat.colors(max-min,alpha=0.7)[n-min+1])
n=6
drawcurve(n,heat.colors(max-min,alpha=0.4)[n-min+1])
n=7
drawcurve(n,heat.colors(max-min,alpha=0.4)[n-min+1])
n=8
drawcurve(n,heat.colors(max-min,alpha=0.4)[n-min+1])
n=9
drawcurve(n,heat.colors(max-min,alpha=0.4)[n-min+1])
n=10
drawcurve(n,heat.colors(max-min,alpha=0.4)[n-min+1])

#diamond in cameroon
points(c(11.52),c(3.87),pch=1,cex=5,col='blue',lwd=4)

#permutation test
real=c()
for (n in min:max){
  testpts=structure(list(x=epicentersdata[,3][epicentersdata[,6]>n],y=epicentersdata[,2][epicentersdata[,6]>n]))
  points(testpts,pch=1,col='black')
  chuld <- lapply(testpts,"[",chull(testpts))
  polygon(chuld,lty=2,border='white')
  polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2)  real=append(real,areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100)))
} 
significances=rep(0,length(real))
for (turn in 1:100){
  fake=c()
  epicentersdata[,6]=sample(epicentersdata[,6])
  for (n in min:max){
    testpts=structure(list(x=epicentersdata[,3][epicentersdata[,6]>n],y=epicentersdata[,2][epicentersdata[,6]>n]))
    points(testpts,pch=1,col='black')
    chuld <- lapply(testpts,"[",chull(testpts))
    polygon(chuld,lty=2,border='white')
    polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2)    fake=append(fake,areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100)))
  } 
  for (n in 1:min(length(fake),length(real))){
    if (!abs(unique(rev(real)[n]))<abs(unique(rev(fake)[n]))){
      significances[n]=significances[n]+1
    }
  }
}
significances

#ASIA
#load data
epicentersdata=read.csv("epicentersasia.txt",header=F,stringsAsFactors=F,sep='\t')

#map
map("worldHires",xlim=c(70,130), ylim=c(6,50))
min=1
max=7
for (n in (min:max)){
  drawcurve(n,heat.colors(max-min,alpha=0.3)[n-min+1])
}
drawcurve(8,'black')
epicentersdata[,6]=sample(epicentersdata[,6])

# a version which has the two Hmong-Mien languages as their own curve:
epicentersdata=read.csv("~/Documents/Epicentres/epicentersasia.txt",header=F,stringsAsFactors=F,sep='\t')
epicentersdata[674,]=c('Jiatong_Miao',25.84,109,NA,NA,9,'Hmong-Mien',NA,'Eurasia','Hmong-Mien')
map("worldHires",xlim=c(70,130), ylim=c(6,50))
min=1
max=9
for (n in (min:max)){
  drawcurve(n,heat.colors(max-min,alpha=0.3)[n-min+1])
}
drawcurve(8,heat.colors(8,alpha=0.7)[8-min+1])

#origin of rice in Zhejiang
points(c(120.7),c(28),pch=1,cex=5,col='blue')
origin=c(120.7,28)

#origin of rice above Hainan according to genetics paper
points(c(110),c(22.5),pch=1,cex=5,col='blue',lwd=3)

#permutation test
real=c()
for (n in min:max){
  testpts=structure(list(x=epicentersdata[,3][epicentersdata[,6]>n],y=epicentersdata[,2][epicentersdata[,6]>n]))
  points(testpts,pch=1,col='black')
  chuld <- lapply(testpts,"[",chull(testpts))
  polygon(chuld,lty=2,border='white')
  polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2) real=append(real,areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100)))
} 
significances=rep(0,length(real))
for (turn in 1:100){
  fake=c()
  epicentersdata[,6]=sample(epicentersdata[,6])
  for (n in min:max){
    testpts=structure(list(x=epicentersdata[,3][epicentersdata[,6]>n],y=epicentersdata[,2][epicentersdata[,6]>n]))
    points(testpts,pch=1,col='black')
    chuld <- lapply(testpts,"[",chull(testpts))
    polygon(chuld,lty=2,border='white')
    polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2)   fake=append(fake,areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100)))
  } 
  for (n in 1:min(length(fake),length(real))){
    if (!abs(unique(rev(real)[n]))<abs(unique(rev(fake)[n]))){
      significances[n]=significances[n]+1
    }
  }
}
significances

#restricted permutation test
threshold=9
epicentersdata2=epicentersdata
epicentersdata2[,6][epicentersdata2[,6]<threshold]=sample(epicentersdata2[,6][epicentersdata2[,6]<threshold])
epicentersdata=epicentersdata2
significances
fake

#PAPUA

#load data
epicentersdata=read.csv("epicenterspapua.txt",header=F,stringsAsFactors=F,sep='\t')
epicentersdata=epicentersdata[epicentersdata[,7]=='Trans New Guinea',]
epicentersdata=epicentersdata[!epicentersdata[,3]<131,]
epicentersdata=epicentersdata[!epicentersdata[,3]>150,]
epicentersdata=epicentersdata[epicentersdata[,2]>-10,]
epicentersdata=epicentersdata[epicentersdata[,2]<5,]

#map
map("worldHires",xlim=c(125,160), ylim=c(-15,5))
min=1
max=6
for (n in (min:max)){
  drawcurve(n,heat.colors(max-min,alpha=0.5)[n-min+1])
}
map("worldHires",xlim=c(125,160), ylim=c(-15,5))

#a better map
drawcurve(1,heat.colors(5,alpha=0.2)[1])
drawcurve(2,heat.colors(5,alpha=0.5)[3]) 
drawcurve(3,heat.colors(5,alpha=0.6)[4]) 
drawcurve(4,heat.colors(5,alpha=0.7)[5]) 

#kuk swamp from unesco site
points(c(144.19136),c(-5.47542),pch=1,cex=4,col='blue',lwd=3)
S5 47 1.36 E144 19 54.2
location=c(144.19136,-5.47542)

#permutation test
min=1
max=5
real=c()
for (n in min:max){
  testpts=structure(list(x=epicentersdata[,3][epicentersdata[,6]>n],y=epicentersdata[,2][epicentersdata[,6]>n]))
  points(testpts,pch=1,col='black')
  chuld <- lapply(testpts,"[",chull(testpts))
  polygon(chuld,lty=2,border='white')
  polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2)  real=append(real,areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100)))
} 
real
significances=rep(0,length(real))
for (turn in 1:100){
  fake=c()
  epicentersdata[,6]=sample(epicentersdata[,6])
  for (n in min:max){
    testpts=structure(list(x=epicentersdata[,3][epicentersdata[,6]>n],y=epicentersdata[,2][epicentersdata[,6]>n]))
    points(testpts,pch=1,col='black')
    chuld <- lapply(testpts,"[",chull(testpts))
    polygon(chuld,lty=2,border='white')
    polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2)  fake=append(fake,areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100)))
  } 
  for (n in 1:min(length(fake),length(real))){
    if (!abs(unique(rev(real)[n]))<abs(unique(rev(fake)[n]))){
      significances[n]=significances[n]+1
    }
  }
}
significances

#MEXICO

#load data
epicentersdata=read.csv("~/Documents/Epicentres/epicentersmexico.txt",header=F,stringsAsFactors=F,sep='\t')

#map
map("world",xlim=c(-120,-70), ylim=c(-10,40))
min=1
max=8
for (n in (min:max)){
  drawcurve(n,heat.colors(max-min,alpha=0.3)[n-min+1])
}
drawcurve(1,heat.colors(4,alpha=0.2)[1])
drawcurve(2,heat.colors(4,alpha=0.6)[3])
long=c(-85,-35)
lat=c(-30,20)

#balsas basin
points(c(-96.85),c(18),pch=1,cex=3,col='blue',lwd=3)
origin=c(-96.85,18)

#permutation test
min=0
max=2
real=c()
for (n in min:max){
  testpts=structure(list(x=epicentersdata[,3][epicentersdata[,6]>n],y=epicentersdata[,2][epicentersdata[,6]>n]))
  points(testpts,pch=1,col='black')
  chuld <- lapply(testpts,"[",chull(testpts))
  polygon(chuld,lty=2,border='white')
  polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2)
real=append(real,areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100)))
} 
real
significances=rep(0,length(real))
for (turn in 1:100){
  fake=c()
  epicentersdata[,6]=sample(epicentersdata[,6])
  for (n in min:max){
    testpts=structure(list(x=epicentersdata[,3][epicentersdata[,6]>n],y=epicentersdata[,2][epicentersdata[,6]>n]))
    points(testpts,pch=1,col='black')
    chuld <- lapply(testpts,"[",chull(testpts))
    polygon(chuld,lty=2,border='white')
    polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2)    fake=append(fake,areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100)))
  } 
  for (n in 1:min(length(fake),length(real))){
    if (!abs(unique(rev(real)[n]))<abs(unique(rev(fake)[n]))){
      significances[n]=significances[n]+1
    }
  }
}
significances


#CORRELATION TESTS
distancefromorigin=function(x,y,z){
  return(distHaversine(c(epicentersdata[x,3],epicentersdata[x,2]),c(y,z)))
}
findmedian=function(x){
  median(epicentersdata$fromorigin[epicentersdata[,6]==x])
}

#load data in an area and/or language family

#AFRICA
epicentersdata=read.csv("epicentersafrica.txt",header=F,stringsAsFactors=F,sep='\t')
epicentersdata=epicentersdata[epicentersdata[,6]>1,]
origin=c(19.2,5.114)
origin=c(11.52,3.87)
min=2
max=11
epicentersdata=epicentersdata[epicentersdata[,7]=='Afro-Asiatic',]
epicentersdata=epicentersdata[epicentersdata[,7]=='Niger-Kongo',]
epicentersdata=epicentersdata[!epicentersdata[,7]=='Niger-Kongo',]
epicentersdata=epicentersdata[epicentersdata[,7]=='Nilo-Saharan',]

epicentersdata[,11][epicentersdata[,11]==" "]=epicentersdata[,7]
epicentersdata[which(epicentersdata[,11]==" "),11]=epicentersdata[which(epicentersdata[,11]==" "),7]

#ASIA
epicentersdata=read.csv("epicentersasia.txt",header=F,stringsAsFactors=F,sep='\t')
origin=c(109,25.84)
#better origin from Wenzhou in Zhejiang
origin=c(120.7,28)
#better origin in nature paper
origin=c(110,22.5)
lat=c(6,50)
long=c(70,130) 
map("worldHires",xlim=c(70,130), ylim=c(6,50))
min=2
max=8
epicentersdata=epicentersdata[epicentersdata[,3]<long[2],]
epicentersdata=epicentersdata[epicentersdata[,3]>long[1],]
epicentersdata=epicentersdata[epicentersdata[,2]>lat[1],]
epicentersdata=epicentersdata[epicentersdata[,2]<lat[2],]
epicentersdata=epicentersdata[epicentersdata[,7]=='Tibeto-Burman',]
epicentersdata=epicentersdata[epicentersdata[,7]=='Tai-Kadai',]
epicentersdata=epicentersdata[epicentersdata[,7]=='Austroasiatic',]
epicentersdata=epicentersdata[!epicentersdata[,7]=='Tibeto-Burman',]

#PAPUA
epicentersdata=read.csv("epicenterspapua.txt",header=F,stringsAsFactors=F,sep='\t')
min=2
max=6
origin=c(138.25,-3.25)
origin=c(144.19,-5.47)
epicentersdata=epicentersdata[epicentersdata[,7]=='Trans New Guinea',]
points(138.25,-3.25)\

#MEXICO
epicentersdata=read.csv("epicentersmexico.txt",header=F,stringsAsFactors=F,sep='\t')
map("worldHires",xlim=c(-120,-70), ylim=c(-10,40))
min=0
max=8
#from Balsas basin
origin=c(-96.85,18)
#from Otomanguean
origin=c(-97.25,16.5)
origin=sample(which(epicentersdata[,7]=='Otomanguean'),1)
origin=c(epicentersdata[origin,3],epicentersdata[origin,2])
epicentersdata=epicentersdata[epicentersdata[,7]=='Otomanguean',]

#tests

#testing correlation between distance from point with most tones and number of tones
fromorigin=c()
for (n in 1:length(epicentersdata[,6])){
fromorigin=append(fromorigin,distancefromorigin(n,origin[1],origin[2]))
}
epicentersdata$fromorigin=fromorigin
cor.test(epicentersdata[,6],epicentersdata$fromorigin,method='pearson')

#testing correlation between number of tones and humidity
climateiso=read.csv('humidityiso.csv',stringsAsFactors=F)
climate=read.csv('humidity.csv',stringsAsFactors=F)
climateiso$ISO.code=gsub(" ","",climateiso$ISO.code)
humidityvalues=sapply(epicentersdata[,1],humidity,simplify='vector')
summary(lm(epicentersdata[,6]~humidityvalues))
cor.test(epicentersdata[,6],humidityvalues,method='pearson')
epicentersdata$humidityvalues=humidityvalues
cor.test(epicentersdata$fromorigin,humidityvalues,method='pearson')
epicentersdata$humidityvalues=humidityvalues
epicentersdata=epicentersdata[!is.na(epicentersdata$humidityvalues),]

#testing correlation between number of tones and distance from point with most tones, controlling for humidity
pcor.test(epicentersdata[,6],epicentersdata$fromorigin,epicentersdata$humidityvalues,method='spearman')
summary(lm(epicentersdata[,6]~epicentersdata$fromorigin+epicentersdata$humidityvalues))

#MIXED EFFECTS MODELS
#FAMILY
s=lmer(epicentersdata[,6]~epicentersdata$fromorigin+(1|epicentersdata[,7]))
t=lmer(epicentersdata[,6]~(1|epicentersdata[,7]))
anova(s,t)

#GENUS
s=lmer(epicentersdata[,6]~epicentersdata$fromorigin+(1|epicentersdata[,11]))
t=lmer(epicentersdata[,6]~(1|epicentersdata[,11]))
anova(s,t)

#BOTH
s=lmer(epicentersdata[,6]~epicentersdata$fromorigin+(1|epicentersdata[,11])+(1|epicentersdata[,7]))
t=lmer(epicentersdata[,6]~(1|epicentersdata[,11])+(1|epicentersdata[,7]))
anova(s,t)

#HUMIDITY
s=lmer(epicentersdata[,6]~epicentersdata$humidityvalues+(1|epicentersdata[,11])+(1|epicentersdata[,7]))
t=lmer(epicentersdata[,6]~(1|epicentersdata[,11])+(1|epicentersdata[,7]))
anova(s,t)


#SIMULATION ILLUSTRATING THE PROCESS OF TONE SPREADING

num=function(x){
  length(which(epicentersdata[,6]==x))/length(epicentersdata[,6])
} 
fromlang=function(x){  distHaversine(epicentersdata[x,c(3,2)],epicentersdata[lang,c(3,2)])/1000
}
prior=sapply(0:20,num)
sim=sample(0:20,length(epicentersdata[,6]),replace=T,prob=prior)
lang=sample(1:length(epicentersdata[,1]),1)
epicentersdata[,12]=sim
epicentersdata[,13]=sapply(1:length(epicentersdata[,6]),fromlang)
epicentersdata[,14]=rep(0,length(epicentersdata[,6]))
for (member in 1:length(epicentersdata[,6])){
  if(epicentersdata[member,13]<1000){
    epicentersdata[member,14]=1
  }
}
noise=5
number=50
for (iteration in 1:300){
  radius2=sample(ceiling((max(0,rnorm(500,2000,0)))),1)
  lang2=sample(which(epicentersdata[,14]==1),1)
  list=c()
  for (member in 1:length(epicentersdata[,1])){
    value=epicentersdata[lang,12]
    if (distHaversine(epicentersdata[member,c(3,2)],epicentersdata[lang2,c(3,2)])/1000<radius2 & !epicentersdata[member,14]==1){
      list=append(list,member)
    }
  }
  if(!length(list)==0){
    list=sample(list,number,replace=T)
    for (member in list){
      epicentersdata[member,12]=max(0,value-1)
      epicentersdata[member,14]=1
    }      
  }
  list=sample(which(epicentersdata[,14]==1),noise,replace=T)
  for (member in list){
    epicentersdata[member,12]=epicentersdata[member,12]+1
  }  
}
map("world",xlim=c(-20,60), ylim=c(-40,40))
for (n in min(epicentersdata[,12]):max(epicentersdata[,12])){
  generaldrawcurve(n,heat.colors(max(epicentersdata[,12])-min(epicentersdata[,12]),alpha=0.4)[n-min(epicentersdata[,12])+1],epicentersdata,2,3,12)
}


#TESTING HOW ROBUST THE INFERENCE IS TO NOISE IN THE DATA

#modifying genera
pickrandomnode=function(tree,times){
  answers=c()
  for (time in 1:times){
    nodes=c()
    current=tree$edge[1]
    while(length(which(tree$edge[,1]==current))>0){
      current=sample(which(tree$edge[,1]==current),1)
      nodes=append(nodes,current)
    }
    answers=append(answers,(sample(nodes,1)))
  }
  return(answers)
}
epicentersdata$fake=epicentersdata[,6]
noiseadd=15
noiselose=0
distancesoff=c()
tonesoff=c()
rounds=100
for (round in 1:rounds){
  epicentersdata$fake=epicentersdata[,6]
  listadd=pickrandomnode(newtree,noiseadd)
  listlose=pickrandomnode(newtree,noiselose)
  for (member in listadd){
    for (language in 1:length(epicentersdata[,1])){
      if (isancestor(newtree,c(newtree$tip,newtree$node)[member],epicentersdata[language,8])==1){
        epicentersdata$fake[language]=epicentersdata$fake[language]+1
      }    
    }
  }
#subtract as well as add
  for (member in listlose){
    for (language in 1:length(epicentersdata[,1])){
      if (isancestor(newtree,newtree$node.label[member],epicentersdata[language,8])==1){
       epicentersdata$fake[language]=epicentersdata$fake[language]-1
      }    
   }
  }  
  #assessing how close the inferred center is to the real center
  lang=which(epicentersdata[,6]==max(epicentersdata[,6]))[1]
  distancesoff=append(distancesoff,min(fromlang(which(epicentersdata$fake==max(epicentersdata$fake))))) 
  tonesoff=append(tonesoff,max(epicentersdata$fake)-epicentersdata$fake[lang])
}
sort(distancesoff)
sort(tonesoff)


#result which tells you on average how far away the origin is from the inferred origin
mean(distancesoff)

#PHYLOGENETIC TEST
fisherresults=c()
pearsonresults=c()
pearsonresults2=c()
humiditypearson=c()
taikadaifisherresults=c()

#set whether you want to use asjp branchlengths, 'asjp' or 'not':
#familysetting='asjp'
#familysetting='not'

#set area:
#setting='africa'
#setting='asia'
#setting='newguinea'
if (setting=='asia'){
  new=readChar('/Users/jercol/Documents/Epicentres/asiatree.txt',file.info('/Users/jercol/Documents/Epicentres/asiatree.txt')$size)
  familyname='Sino-Tibetan'
  altfamilyname='Tibeto-Burman'
  contact='Indo-European'
  contact2='Hmong-Mien'
}
if (setting=='africa'){
  new=readChar('/Users/jercol/Documents/Epicentres/africatree.txt',file.info('/Users/jercol/Documents/Epicentres/africatree.txt')$size)
  familyname='Atlantic-Congo'
  altfamilyname='Niger-Kongo'
  contact='Nilo-Saharan'
  contact2='Afro-Asiatic'
}
if (setting=='newguinea'){
  new=readChar('/Users/jercol/Documents/Epicentres/glottologtree.txt',file.info('/Users/jercol/Documents/Epicentres/glottologtree.txt')$size)
  familyname='Trans_New_Guinea'
  altfamilyname='Trans New Guinea'
  contact='Lakes Plains'
  contact2='Sepik'
}
epicentersdata=read.csv("~/Documents/Epicentres/epicentersworld.txt",header=F,stringsAsFactors=F,sep='\t')
epicentersdata=epicentersdata[epicentersdata[,7]==altfamilyname,]
for(time in 1:1){
newtree=read.tree(text=new)
values=epicentersdata[,6]
names(values)=epicentersdata[,8]
#choose a particular family
treeset=subtrees(newtree)
findsubtree=function(family){
  for (n in 1:length(treeset)){
    if (treeset[[n]]$node.label[1]==family){
      return(n)
    }
  }
}
newtree2=treeset[[findsubtree(familyname)]]
newtree2=drop.tip(newtree2,newtree2$tip[!newtree2$tip %in% epicentersdata[,8]])
if (familysetting=='asjp'){
  newtree=asjptree
  epicentersdata=epicentersdata[epicentersdata[,7]==altfamilyname,]
  newtree2=drop.tip(newtree,newtree$tip[!newtree$tip %in% epicentersdata[,8]])
  values=epicentersdata[,6]
  names(values)=epicentersdata[,8]
  
}
values=values[names(values) %in% newtree2$tip]
values2=c()
for (member in unique(names(values))){
  values2=append(values2,values[which(names(values)==member)[1]])
}
names(values2)=unique(names(values))
newtree3=compute.brlen(multi2di(newtree2,random=T),1)
if(familysetting=='asjp'){
  newtree3=multi2di(newtree2,random=T)
}
results2=ace(values2,newtree3,type='discrete',method='ML',model='ARD')
ancstates=c()
for (n in 1:newtree3$Nnode){
  ancstates=append(ancstates,colnames(results2$lik.anc)[which(Re(results2$lik.anc[n,])==max(Re(results2$lik.anc[n,])))])
}
ancstates=as.numeric(ancstates)
names(ancstates)=newtree3$node.label
#newtree2=compute.brlen(newtree2,method='Grafen')
#newtree2=compute.brlen(newtree2,1)
#newtree3=multi2di(newtree2,random=T)
makemap=function(x){
  if(x==1){
    map("world",xlim=c(-20,60), ylim=c(-40,40))
  }
  if(x==2){
    map("world",xlim=c(70,150), ylim=c(-9,50))
  }
  if(x==3){
    map("world",xlim=c(-120,-70), ylim=c(-10,40))
  }  
}
#calculating ancestral locations
lats=epicentersdata[,2]
longs=epicentersdata[,3]
names(lats)=epicentersdata[,8]
names(longs)=epicentersdata[,8]
lats=lats[names(lats) %in% newtree3$tip]
longs=longs[names(longs) %in% newtree3$tip]
lats2=c()
for (member in unique(names(lats))){
  lats2=append(lats2,lats[which(names(lats)==member)[1]])
}
names(lats2)=unique(names(lats))
longs2=c()
for (member in unique(names(longs))){
  longs2=append(longs2,longs[which(names(longs)==member)[1]])
}
names(longs2)=unique(names(longs))
#sorting lats2 and longs2 so that they match newtree3$tip
sortedlats2=c()
sortedlongs2=c()
sortedvalues2=c()
for(member in newtree3$tip){
  for(member2 in 1:length(lats2)){
    if(names(lats2[member2])==member){
      sortedlats2=append(sortedlats2,lats2[member2])
      sortedlongs2=append(sortedlongs2,longs2[member2]) 
      sortedvalues2=append(sortedvalues2,values2[member2])    
    }
  }
}
lats2=sortedlats2
longs2=sortedlongs2
values2=sortedvalues2
anclats=ace(lats2,newtree3,type='continuous',method='ML')
anclongs=ace(longs2,newtree3,type='continuous',method='ML')
anclats=anclats$ace
anclongs=anclongs$ace
names(anclats)=newtree3$node
names(anclongs)=newtree3$node
#recalculating number of tonogenesis events taking location into account
nearest=function(lat,long,family){
  epicentersdata=set(setting)
  distances=c()
  tempf=epicentersdata[epicentersdata[,7]==family,]
  for(member in 1:length(tempf[,1])){
    distances=append(distances,distHaversine(c(tempf[member,3],tempf[member,2]),c(long,lat))/1000)
  }
  return(min(distances))
}
location=c(sample(epicentersdata[,3][epicentersdata[,6]==max(epicentersdata[,6])],1),sample(epicentersdata[,2][epicentersdata[,6]==max(epicentersdata[,6])],1))
additions=0
losses=0
addlanguages=c()
losslanguages=c()
totaikadaigains=0
totaikadailosses=0
fromtaikadaigains=0
fromtaikadailosses=0
for (n in 1:newtree3$Nnode){
  a=ancstates[n]
  descendants=immdescnumbers(newtree3,n+length(newtree3$tip))
  descstates=c(values2,ancstates)[descendants]
  a1=anclats[n]
  a2=anclongs[n]
  desclats=c(lats2,anclats)[descendants]
  desclongs=c(longs2,anclongs)[descendants]  
  for (m in 1:length(desclats)){
    if ((distHaversine(c(desclongs[m],desclats[m]),c(location[1],location[2]))/1000)<(distHaversine(c(a2,a1),c(location[1],location[2]))/1000)){     
      if (descstates[m]>a){
        additions=additions+descstates[m]-a
        addlanguages=append(addlanguages,c(newtree3$tip,newtree3$node)[descendants[m]])
      }
      if (descstates[m]<a){
        losses=losses+a-descstates[m]
        losslanguages=append(losslanguages,c(newtree3$tip,newtree3$node)[descendants[m]])        
      }      
    }
    if (nearest(desclats[m],desclongs[m],contact)<nearest(a1,a2,contact)){
      if (descstates[m]>a){
       totaikadaigains=totaikadaigains+1 
      }
      if (descstates[m]<a){
        totaikadailosses=totaikadailosses+1 
      }      
    } 
    if (nearest(desclats[m],desclongs[m],contact)>nearest(a1,a2,contact)){
      if (descstates[m]>a){
        fromtaikadaigains=fromtaikadaigains+1 
      }
      if (descstates[m]<a){
        fromtaikadailosses=fromtaikadailosses+1 
      }      
    }      
  }  
}
additions2=additions
losses2=losses
location=c(sample(epicentersdata[,3][epicentersdata[,6]==max(epicentersdata[,6])],1),sample(epicentersdata[,2][epicentersdata[,6]==max(epicentersdata[,6])],1))
additions=0
losses=0
addlanguages=c()
losslanguages=c()
for (n in 1:newtree3$Nnode){
  a=ancstates[n]
  descendants=immdescnumbers(newtree3,n+length(newtree3$tip))
  descstates=c(values2,ancstates)[descendants]
  a1=anclats[n]
  a2=anclongs[n]
  desclats=c(lats2,anclats)[descendants]
  desclongs=c(longs2,anclongs)[descendants]  
  for (m in 1:length(desclats)){
    if ((distHaversine(c(desclongs[m],desclats[m]),c(location[1],location[2]))/1000)>(distHaversine(c(a2,a1),c(location[1],location[2]))/1000)){
      
      if (descstates[m]>a){
        additions=additions+descstates[m]-a
        addlanguages=append(addlanguages,c(newtree3$tip,newtree3$node)[descendants[m]])
      }
      if (descstates[m]<a){
        losses=losses+a-descstates[m]
        losslanguages=append(losslanguages,c(newtree3$tip,newtree3$node)[descendants[m]])        
      }      
    }
  }  
}
fisherresults=append(fisherresults,fisher.test(as.matrix(rbind(c(additions,losses),c(additions2,losses2))))[1])
taikadaifisherresults=append(taikadaifisherresults,fisher.test(as.matrix(rbind(c(totaikadaigains,totaikadailosses),c(fromtaikadaigains,fromtaikadailosses))))[1])
location=c(sample(epicentersdata[,3][epicentersdata[,6]==max(epicentersdata[,6])],1),sample(epicentersdata[,2][epicentersdata[,6]==max(epicentersdata[,6])],1))
translats=c()
translongs=c()
transtones=c()
transdists=c()
desttones=c()
transcontact=c()
transcontact2=c()
transhumidities=c()
for (n in 1:newtree3$Nnode){
  a=ancstates[n]
  descendants=immdescnumbers(newtree3,n+length(newtree3$tip))
  descstates=c(values2,ancstates)[descendants]
  a1=anclats[n]
  a2=anclongs[n]
  desclats=c(lats2,anclats)[descendants]
  desclongs=c(longs2,anclongs)[descendants]  
  for (m in 1:length(desclats)){
    
      if (descstates[m]>a){
        transtones=append(transtones,descstates[m]-a)
        translats=append(translats,desclats[m])
        translongs=append(translongs,desclongs[m])
        transdists=append(transdists,distHaversine(c(desclongs[m],desclats[m]),c(location[1],location[2]))/1000)
        desttones=append(desttones,descstates[m])
        transcontact=append(transcontact,nearest(desclats[m],desclongs[m],contact))
        transcontact2=append(transcontact2,nearest(desclats[m],desclongs[m],contact2))
        transhumidities=append(transhumidities,nearesthumidity(desclats[m],desclongs[m]))
        
      }
      if (descstates[m]<a){
        transtones=append(transtones,descstates[m]-a)
        translats=append(translats,desclats[m])
        translongs=append(translongs,desclongs[m])
        transdists=append(transdists,distHaversine(c(desclongs[m],desclats[m]),c(location[1],location[2]))/1000)
        desttones=append(desttones,descstates[m])
        transcontact=append(transcontact,nearest(desclats[m],desclongs[m],contact))
        transcontact2=append(transcontact2,nearest(desclats[m],desclongs[m],contact2))
        transhumidities=append(transhumidities,nearesthumidity(desclats[m],desclongs[m]))       
      }      
    }
}
pearsonresults=append(pearsonresults,cor.test(desttones,transcontact,method='pearson')[3])
pearsonresults2=append(pearsonresults2,cor.test(desttones,transdists,method='pearson')[3])
humiditypearson=append(humiditypearson,cor.test(desttones[!is.na(transhumidities)],transhumidities[!is.na(transhumidities)],method='pearson')[3])
}
taikadaifisherresults
fisherresults
pearsonresults
pearsonresults2
points(translongs[transhumidities>0.015],translats[transhumidities>0.015])
humiditypearson

#results
length(taikadaifisherresults[taikadaifisherresults<0.05])
length(pearsonresults[pearsonresults<0.05])
length(pearsonresults2[pearsonresults2<0.05])

asiafisherresults=fisherresults
asiapearsonresults=pearsonresults
africafisherresults
africapearsonresults

#SIMULATIONS OF TRAIT EVOLUTION
#1. USING RESULTS2 MATRIX
#NB the rate i -> j is found in [j,i]
matrix1=results2$index.matrix
matrix2=c()
for(member in matrix1){
  if(is.na(member)){
    matrix2=append(matrix2,0)
  }
  if(!is.na(member)){
    matrix2=append(matrix2,results2$rates[member])
  }
}
matrix2=matrix(matrix2,max(epicentersdata[,6])+1,max(epicentersdata[,6])+1)
matrix2=matrix(matrix2,2,2)
matrix2=t(matrix2)
newtree4=sim.history(newtree3,matrix2,anc=1)
newtree4=sim.char(newtree3,matrix2)
epicentersdata2=epicentersdata[epicentersdata[,8] %in% names(newtree4$states),]
values=c()
lats=c()
longs=c()
names=c()
for(member in 1:length(epicentersdata[,1])){
  if(epicentersdata[member,8] %in% names(newtree4$states) & ! epicentersdata[member,8] %in% names){
    names=append(names,epicentersdata[member,8])
    lats=append(lats,as.numeric(epicentersdata[member,2]))
    longs=append(longs,as.numeric(epicentersdata[member,3]))
#    values=append(values,as.numeric(epicentersdata[member,6]))    values=append(values,as.numeric(newtree4$states[names(newtree4$states)==epicentersdata[member,8]]))    
  }
}


#permutation test simulating the random development of tones in langauge families
#2. USING FITDISCRETE IN GEIGER
#n.b. turn 0 into 1 in values2, as it cannot take 0s
for(member in 1:length(values2)){
  if(values2[member]==0){
    values2[member]=1
  }
}
values2
matrix2=fitDiscrete(newtree3,values2,model='ARD')
matrix2=fitDiscrete(newtree3,values2,model='meristic')
matrix3=read.csv('/Users/jercol/Documents/Epicentres/Matrix2.txt',sep=' ',header=F,stringsAsFactors=F)
matrix3=matrix3[,!is.na(matrix3[1,])]
matrix2=t(matrix3)
#matrix2=matrix(matrix2,12,12)
matrix2=matrix(t(matrix3),8,8)

newtree4=sim.history(newtree3,matrix2)
epicentersdata2=epicentersdata[epicentersdata[,8] %in% names(newtree4$states),]
for (member in 1:length(epicentersdata2[,1])){
  for(member2 in 1:length(newtree4$states)){
    if(epicentersdata2[member,8]==names(newtree4$states[member2])){
      epicentersdata2[member,6]=newtree4$states[member2]
    }
  }
}
epicentersdata2[,6]=as.numeric(epicentersdata2[,6])

makemap(2)
generaldrawcurve(3,'red',epicentersdata2,2,3,6)
newtree4$states

#permutation test
newtree4=sim.history(newtree3,matrix2)
epicentersdata2=epicentersdata[epicentersdata[,8] %in% names(newtree4$states),]
min=min(epicentersdata2[,6])
max=max(epicentersdata2[,6])
real=c()
for (n in min:max){
  testpts=structure(list(x=epicentersdata2[,3][epicentersdata2[,6]>n],y=epicentersdata2[,2][epicentersdata2[,6]>n]))
  points(testpts,pch=1,col='black')
  chuld <- lapply(testpts,"[",chull(testpts))
  polygon(chuld,lty=2,border='white')
  polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2)
  
  real=append(real,areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100)))
} 
real
significances=rep(0,length(real))
for (turn in 1:100){
  newtree4=sim.history(newtree3,matrix2)
  epicentersdata2=epicentersdata[epicentersdata[,8] %in% names(newtree4$states),]
  for (member in 1:length(epicentersdata2[,1])){
    for(member2 in 1:length(newtree4$states)){
      if(epicentersdata2[member,8]==names(newtree4$states[member2])){
        epicentersdata2[member,6]=newtree4$states[member2]
      }
    }
  }
  epicentersdata2[,6]=as.numeric(epicentersdata2[,6])
  fake=c()
  min=min(epicentersdata2[,6])
  max=max(epicentersdata2[,6])-1
  if(length(epicentersdata2[,6][epicentersdata2[,6]>max])<3){
    while(length(epicentersdata2[,6][epicentersdata2[,6]>max])<3){
      max=max-1
    }
  }
  for (n in min:max){
    testpts=structure(list(x=epicentersdata2[,3][epicentersdata2[,6]>n],y=epicentersdata2[,2][epicentersdata2[,6]>n]))
    points(testpts,pch=1,col='black')
    chuld <- lapply(testpts,"[",chull(testpts))
    polygon(chuld,lty=2,border='white')
    polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2)  fake=append(fake,areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100)))
  } 
  for (n in 1:min(length(fake),length(real))){
    if (!abs(unique(rev(real)[n]))<abs(unique(rev(fake)[n]))){
      significances[n]=significances[n]+1
    }
  }
}
significances

#mapping the path of languages
#e.g.languagename='Cantonese'
language=epicentersdata[,8][epicentersdata[,1]==languagename] 
#or language ='yue'
languagenumber=which(c(newtree3$tip,newtree3$node)==language)  
points(c(longs2,anclongs)[languagenumber],c(lats2,anclats)[languagenumber],pch=16)


#HUMIDITY SIMULATIONS

#function for running a Monte Carlo sampling test with one language per family, and seeing the humidity of tonal languages at a certain percentile ('threshold')
montecarlo=function(x,y,families,threshold){
  result=c()
  while(length(result)<1){
    group1=c()
    group2=c()
    for(member in unique(families)){
      vector1=x[families==member]
      vector2=y[families==member]      
      group1=append(group1,jsample(vector2[vector1==max(x)],1))
      group2=append(group2,jsample(vector2[vector1==min(x)],1))
    }
    group1=group1[!is.na(group1)]
    group2=group2[!is.na(group2)]  
    group1=jsample(group1,min(length(group1),length(group2)))
    group2=jsample(group2,min(length(group1),length(group2)))
    #  return(mean(group1)-mean(group2))
    result=sort(group1)[round(length(group1)*threshold/100)]-sort(group2)[round(length(group2)*threshold/100)]   
  }
  return(result)
}

#main Monte Carlo sampling test
values=c()
for(member in epicentersdata[,6]){
  if(member>2){
    values=append(values,1)
    
  }
  if(member<3){
    values=append(values,0)
  }
}
faketone=faketone[!is.na(globalhumidities)]
values=values[!is.na(globalhumidities)]
families=epicentersdata[,7][!is.na(globalhumidities)]
families=1:length(families)
globalhumidities=globalhumidities[!is.na(globalhumidities)]
results=rep(0,100)
for(time in 1:100){
  if(montecarlo(values,globalhumidities,families,15)>0){
    results[time]=1
  }
}
#number of samples in which tonal languages had greater humidity than non-tonal languages
length(results[results>0])

#permutation test
megaresults=rep(0,100)
for(time2 in 1:100){
  valuesperm=sample(values,length(values),replace=F)
  results=rep(0,100)
  for(time in 1:100){
    if(montecarlo(valuesperm,globalhumidities,families,15)>0){
      results[time]=1
    }
  }
  if(length(results[results>0])==100){
    megaresults[time2]=1
  }
}
#result
length(megaresults[megaresults>0])


#permutation test by simulating contact
pvalues=c()
tvalues=c()
mc=rep(0,100)
region1='Africa'
region2='Eurasia'
region3='Pacific'
region4='North America'
region5='South America'
region1res=rep(0,100)
region2res=rep(0,100)
region3res=rep(0,100)
region4res=rep(0,100)
region5res=rep(0,100)
region=epicentersdata[,9]
#number of simulations that you want to run:
numberOfSimulations = 1
for(time in 1:numberOfSimulations){
faketone=rep(0,length(epicentersdata[,7]))  
# number of clusters of tone that you want to simulate e.g. 6:
numberOfClusters = 6  
for (time2 in 1:numberOfClusters){
randlang=sample(1:length(epicentersdata[,1]),1)
#randlang=pickrandomtip(newtree2)
#randlang=jsample(which(epicentersdata[,8]==randlang),1)
dists=c()
for (member in 1:length(epicentersdata[,1])){
  dists=append(dists,distHaversine(c(epicentersdata[member,3],epicentersdata[member,2]),c(epicentersdata[randlang,3],epicentersdata[randlang,2]))/1000)
}
surround=c()
limit=jsample(c(1,500),1,c(0.8,0.2))
limit=100
for (member in 1:length(epicentersdata[,1])){
  if(dists[member] %in% sort(dists)[1:limit]){
#  if(dists[member] < 500){      
    surround=append(surround,epicentersdata[member,1])
  }
}
for(member in 1:length(epicentersdata[,1])){
  if(epicentersdata[member,1] %in% surround){
    faketone[member]=1
    
  }
}  
}
map('world')
points(epicentersdata[,3][faketone==1],epicentersdata[,2][faketone==1],pch=16)
pvalues=append(pvalues,summary(glm(faketone~humidities))$coefficients[8])
tvalues=append(tvalues,summary(glm(faketone~humidities))$coefficients[6])
mctemp=rep(0,100)
for(time3 in 1:100){
  if(montecarlo(faketone,humidities,epicentersdata[,7],15)>0){
    mctemp[time3]=1
  }
}
if(length(mctemp[mctemp==1])==100){
  mc[time]=1
}
regiont=region1
if(!is.na(summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[8])){
  if(summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[8]<0.05 & summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[6]>0){
    region1res[time]=1
  }  
}
regiont=region2
if(!is.na(summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[8])){
  if(summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[8]<0.05 & summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[6]>0){
    region2res[time]=1
  }  
}
regiont=region3
if(!is.na(summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[8])){
  if(summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[8]<0.05 & summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[6]>0){
    region3res[time]=1
  }  
}
regiont=region4
if(!is.na(summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[8])){
  if(summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[8]<0.05 & summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[6]>0){
    region4res[time]=1
  }  
}
regiont=region5
if(!is.na(summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[8])){
  if(summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[8]<0.05 & summary(glm(faketone[region==regiont]~humidities[region==regiont]))$coefficients[6]>0){
    region5res[time]=1
  }  
}

}
length(tvalues[pvalues<0.05][tvalues[pvalues<0.05]>0])
length(tvalues[pvalues<0.05][tvalues[pvalues<0.05]<0])
length(mc[mc==1])
length(faketone[faketone==1])
regiontres=region1res
length(regiontres[regiontres==1])
regiontotalres=region1res+region2res+region3res+region4res+region5res
length(regiontotalres[regiontotalres>2])

#same but with random families plus contact
pvalues=c()
tvalues=c()
mc=rep(0,100)
for(time in 1:100){
  faketone=rep(0,length(epicentersdata[,7]))    
  randfams=sample(unique(epicentersdata[,7]),6)
  faketone=rep(0,length(epicentersdata[,7]))
  for(member in 1:length(epicentersdata[,7])){
    if(epicentersdata[member,7] %in% randfams){
      faketone[member]=1
      
    }
  }
  newtree7=drop.tip(newtree2,epicentersdata[,8][!faketone==1])
  for (time2 in 1:10){
    #randlang=sample(1:length(epicentersdata[,1]),1)
    randlang=pickrandomtip(newtree7)
    randlang=jsample(which(epicentersdata[,8]==randlang),1)
    dists=c()
    for (member in 1:length(epicentersdata[,1])){
      dists=append(dists,distHaversine(c(epicentersdata[member,3],epicentersdata[member,2]),c(epicentersdata[randlang,3],epicentersdata[randlang,2]))/1000)
    }
    surround=c()
    for (member in 1:length(epicentersdata[,1])){
      if(dists[member] %in% sort(dists)[1:300]){
        #  if(dists[member] < 500){
        
        surround=append(surround,epicentersdata[member,1])
      }
    }
    for(member in 1:length(epicentersdata[,1])){
      if(epicentersdata[member,1] %in% surround){
        faketone[member]=1
        
      }
    }  
    
  }
  map('world')
  points(epicentersdata[,3][faketone==1],epicentersdata[,2][faketone==1],pch=16)
  pvalues=append(pvalues,summary(glm(faketone~humidities))$coefficients[8])
  tvalues=append(tvalues,summary(glm(faketone~humidities))$coefficients[6])
  mctemp=rep(0,100)
  for(time3 in 1:100){
    if(montecarlo(faketone,humidities,epicentersdata[,7],15)>0){
      mctemp[time3]=1
    }
  }
  if(length(mctemp[mctemp==1])==100){
    mc[time]=1
  }
}

}
length(tvalues[pvalues<0.05][tvalues[pvalues<0.05]>0])
length(mc[mc==1])

