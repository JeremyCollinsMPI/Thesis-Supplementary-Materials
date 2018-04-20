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
#  listadd=sample(1:length(newtree$node.label),noiseadd,replace=T)
#  listlose=sample(1:length(newtree$node.label),noiselose,replace=T)
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


#CODE THAT STILL NEEDS TO BE ORGANISED, UP TO ABOUT LINE 1600

MAXIMUM PARSIMONY ANALYSIS OF TREES
#premade trees and matrices
newtree3=sinotibetantree
matrix2=sinotibetanmatrix
#working out how many times tones have increased or decreased in particular families
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

newtree2=treeset[[findsubtree('Sino-Tibetan')]]
newtree2=treeset[[findsubtree('Atlantic-Congo')]]

newtree2=treeset[[findsubtree(c(newtree$tip,newtree$node)[newtree$edge[which(newtree$edge[,1]==1+length(newtree$tip)),2]][14])]]
newtree2=drop.tip(newtree2,newtree2$tip[!newtree2$tip %in% epicentersdata[,8]])
newtree2
c(newtree$tip,newtree$node)[newtree$edge[which(newtree$edge[,1]==1+length(newtree$tip)),2]]


values=values[names(values) %in% newtree2$tip]
values2=c()
for (member in unique(names(values))){
  values2=append(values2,values[which(names(values)==member)[1]])
}
names(values2)=unique(names(values))
newtree3=compute.brlen(multi2di(newtree2,random=T),1)
results2=ace(values2,newtree3,type='discrete',method='ML',model='ARD')

#rates=matrix(results2$rates[results2$index.matrix],nrow=12,ncol=12)
#sim.history(newtree3,rates)
#rerootingMethod(newtree3,values2,model='ER')

ancstates=c()
for (n in 1:length(newtree3$node.label)){
  ancstates=append(ancstates,colnames(results2$lik.anc)[which(Re(results2$lik.anc[n,])==max(Re(results2$lik.anc[n,])))])
}
ancstates=as.numeric(ancstates)
names(ancstates)=newtree3$node.label

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
matrix2=matrix(matrix2,12,12)
newtree4=sim.history(newtree3,matrix2)
newtree4=sim.char(newtree3,matrix2)
epicentersdata2=epicentersdata[epicentersdata[,8] %in% names(newtree4$states),]
newtree4$states=as.numeric(newtree4$states)
epicentersdata2[,6]=newtree4$states
makemap(1)
generaldrawcurve(6,'red',epicentersdata2,2,3,6)
newtree4$states

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
    polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2)
    
    fake=append(fake,areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100)))
  } 
  for (n in 1:min(length(fake),length(real))){
    if (!abs(unique(rev(real)[n]))<abs(unique(rev(fake)[n]))){
      significances[n]=significances[n]+1
    }
  }
}
significances

epicentersdata[,]

newtree2=compute.brlen(newtree2,method='Grafen')
newtree2=compute.brlen(newtree2,1)
newtree3=multi2di(newtree2,random=T)
newtree3$edge.length=newtree3$edge.length+0.0001
for (member in which(c(newtree3$tip,newtree3$node)=='')){
  for (member2 in 1:length(newtree3$edge[,1])){
    if(newtree3$edge[member2,2]==member){
      newtree3$edge.length[member2]=0.0001
    }
  }
}


results=ace(values2,newtree3,type='continuous',method='ML',model='ARD')
results$ace[which(names(results$ace)==which(c(newtree3$tip.label,newtree3$node.label)=="Benue-CongoPlateau"))]




length(ancstates)
n=1
descendants=immdescnumbers(newtree3,n+length(newtree3$tip))
c(values2,ancstates)[descendants]
additions=0
losses=0
lossgenera=c()
for (n in 1:length(newtree3$node)){
  a=ancstates[n]
  descendants=immdescnumbers(newtree3,n+length(newtree3$tip))
  descstates=c(values2,ancstates)[descendants]
  for (member in descstates){
    if (member>a){
      additions=additions+member-a

      }
    if (member<a){
      losses=losses+a-member
    }
    }

}
additions
losses
lossgenera


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
#map("world",xlim=c(-20,60), ylim=c(-40,40))
map("world",xlim=c(70,150), ylim=c(-9,50))
#map("world",xlim=c(-120,-70), ylim=c(-10,40))
showgenusnumbers(epicentersdata,8,2,3,newtree3,lossgenera[n])
n=n+1


#ancestral locations
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


for (member in which(c(newtree2$tip,newtree2$node)=='World')){
  for (member2 in 1:length(newtree2$edge[,1])){
    if(newtree2$edge[member2,1]==member){
      newtree2$edge.length[member2]=100
    }
  }
}
newtree2=compute.brlen(newtree2,method='Grafen')
newtree2=compute.brlen(newtree2,1)
newtree3=multi2di(newtree2,random=T)
newtree3$edge.length=newtree3$edge.length+0.0001

for (member in which(c(newtree3$tip,newtree3$node)=='')){
  for (member2 in 1:length(newtree3$edge[,1])){
    if(newtree3$edge[member2,2]==member){
      newtree3$edge.length[member2]=0.1
    }
  }
}
newtree3$edge[length(newtree3$edge),]
anclats=ace(lats2,newtree3,type='continuous',method='ML')
anclats$ace[which(names(results$ace)==which(c(newtree3$tip.label,newtree3$node.label)=="Sinitic"))]
anclongs=ace(longs2,newtree3,type='continuous',method='ML')
anclongs[which(names(results$ace)==which(c(newtree3$tip.label,newtree3$node.label)=="Sinitic"))]

anclats=anclats$ace
anclongs=anclongs$ace
names(anclats)=newtree3$node
names(anclongs)=newtree3$node

movestowards=c()
location=c(sample(epicentersdata[,3][epicentersdata[,6]==max(epicentersdata[,6])],1),sample(epicentersdata[,2][epicentersdata[,6]==max(epicentersdata[,6])],1))
location=c(120.7,28)
for (n in 1:length(newtree3$node)){
  a1=anclats[n]
  a2=anclongs[n]
  descendants=immdescnumbers(newtree3,n+length(newtree3$tip))
  desclats=c(lats2,anclats)[descendants]
  desclongs=c(longs2,anclongs)[descendants]
  for (m in 1:length(desclats)){
    if ((distHaversine(c(desclongs[m],desclats[m]),c(location[1],location[2]))/1000)<(distHaversine(c(a2,a1),c(location[1],location[2]))/1000)){
      movestowards=append(movestowards,c(newtree3$tip,newtree3$node)[descendants[m]])
      
    }
  }
}

newtree3$node
makemap(2)
genus='Mak-Ai-Cham'
points(anclongs[which(newtree3$node==genus)],anclats[which(newtree3$node==genus)])

n=102
points(anclongs[n],anclats[n])
newtree3$node[n]
n=n+1
immdesc(newtree3,'SouthChinese') 

#recalculating number of tonogenesis events taking location into account
location=c(sample(epicentersdata[,3][epicentersdata[,6]==max(epicentersdata[,6])],1),sample(epicentersdata[,2][epicentersdata[,6]==max(epicentersdata[,6])],1))
additions=0
losses=0
addlanguages=c()
losslanguages=c()
for (n in 1:length(newtree3$node)){
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

additions
losses
addlanguages
losslanguages

#mapping the path of languages

'' %in% epicentersdata[,1]
languagename='Cantonese'
languagename='Fuzhou'
languagename='Malay'
language=epicentersdata[,8][epicentersdata[,1]==languagename] 

language='mpz'
language='nan'
language='rau'
language='Biu-MandaraAA.7'
language='xed'
language='hak'
language='cmn'
newtree3$tip
language='swh'
language='zul'
languagenumber=which(c(newtree3$tip,newtree3$node)==language)  
languagenumber=sample(1:length(values),1)


points(c(longs2,anclongs)[languagenumber],c(lats2,anclats)[languagenumber],pch=16)
#text(c(longs2,anclongs)[languagenumber],c(lats2,anclats)[languagenumber],labels=c(values2,ancstates)[languagenumber],cex=1.5)
c(newtree3$tip,newtree3$node)[languagenumber]
#c(values2,ancstates)[languagenumber]
languagenumber=newtree3$edge[,1][newtree3$edge[,2]==languagenumber]
text(longs,lats,values)
points(longs,lats)
epicentersdata[,1][epicentersdata[,8]=='mpz']

ancstates
epicentersdata[,7][epicentersdata[,8]=='mnc']

#some good ones: mpz, Fuzhou, CentralChinese

#mapping transition events

#functions for finding distance to nearest unrelated languages in a particular family

nearest=function(lat,long,family){
  epicentersdata=set(setting)
  distances=c()
  for(member in epicentersdata[epicentersdata[,7]==family]){
    distances=append(distances,distHaversine(c(member[3],member[2]),c(long,lat)))
  }
  return(min(distances))
}
nearesthumidity=function(lat,long){
  distances=c()
  humidities=c()
  for(member in 1:length(epicentersdata[,1])){
    distances=append(distances,distHaversine(c(epicentersdata[member,3],epicentersdata[member,2]),c(long,lat)))
    humidities=append(humidities,humidity(epicentersdata[member,1]))
  }
 return(humidities[sample(which(distances==min(distances)),1)]) 
}
nearesthumidity(-3,133)
#maxname=names(newtree4$states)[which(as.numeric(newtree4$states)==max(as.numeric(newtree4$states)))]
#location=c(sample(epicentersdata[,3][epicentersdata[,8]==maxname],1),sample(epicentersdata[,2][epicentersdata[,8]==maxname],1))



location=c(sample(epicentersdata[,3][epicentersdata[,6]==max(epicentersdata[,6])],1),sample(epicentersdata[,2][epicentersdata[,6]==max(epicentersdata[,6])],1))

translats=c()
translongs=c()
transtones=c()
transdists=c()
desttones=c()
transsites=c()
for (n in 1:length(newtree3$node)){
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
        transsites=append(transsites,findnearestsite(desclongs[m],desclats[m]))
      }
      if (descstates[m]<a){
        transtones=append(transtones,descstates[m]-a)
        translats=append(translats,desclats[m])
        translongs=append(translongs,desclongs[m])
        transdists=append(transdists,distHaversine(c(desclongs[m],desclats[m]),c(location[1],location[2]))/1000)
        desttones=append(desttones,descstates[m])
        transsites=append(transsites,findnearestsite(desclongs[m],desclats[m]))
        
      }      
    }
  }  
}
makemap(2)
for(m in 1:length(transtones)){
  text(translongs[m],translats[m],desttones[m])
}

binarytranstones=c()
for(m in 1:length(transdists)){
  if(transtones[m]<0){
    binarytranstones=append(binarytranstones,0)
    points(translongs[m],translats[m],col='red')
  }
  if(transtones[m]>0){
    binarytranstones=append(binarytranstones,1)
    points(translongs[m],translats[m],col='green')
    
  }
}
summary(glm(binarytranstones~transdists))
for(m in 1:length(transtones)){
  text(translongs[m],translats[m],desttones[m])
}
cor.test(desttones,translats,method='pearson')
makemap(1)
min=5

for(m in 1:length(transtones)){
  if(transtones[m]>min){
    text(translongs[m],translats[m],transtones[m])
  }  
}

#correlation with number of splitting events
splittings=c()
for(m1 in 1:length(newtree3$tip)){
  answer=0
  for(m2 in 1:length(newtree3$node)){
    if(isancestornumbers(newtree3,m2+length(newtree3$tip),m1)){
      answer=answer+1
    }
  }
  splittings=append(splittings,answer)
}
cor.test(splittings,values2,method='pearson')

splittings=c()


#correlations with the spread of agriculture
findnearestsite=function(long,lat){
  sitedists=c()
  for(m in 1:length(sites[,1])){
    sitedists=append(sitedists,distHaversine(sites[m,1:2],c(long,lat))/1000)
  }
  return(sites[which(sitedists==min(sitedists)),3])
}
#ASIA:
makemap(2)
#some made up locations and dates
sites=data.frame(c(120,30,5000),c(110,25,4500),c(90,25,2000),c(104,20,3500),c(127,35,1500),c(108,15,4000))
sites=t(sites)
rownames(sites)=c()

#sites from vietnam paper
#sites=data.frame(c(120,30,7000),c(112,27,8000),c(110,25,6000),c(90,25,3500),c(104,20,7000),c(108,15,6000))
sites=data.frame(c(120,30,7000),c(112,27,8000),c(110,25,6000),c(96,25,3500),c(100,18,5000))
sites=t(sites)
rownames(sites)=c()

makemap(2)
points(117,28)
#sites from rice paper
sites=data.frame(c(120,30,10000),c(117,28,10000),c(86,25,4000),c(110,35,5000))
sites=t(sites)
rownames(sites)=c()

makemap(1)
points(9,7)
#sites from africa paper
sites=data.frame(c(9,7,7000),c(30,25,3000),c(30,-10,2000),c(34,-20,1500))
sites=t(sites)
rownames(sites)=c()

n=5
points(sites[n,1],sites[n,2])

epicentersdata$sites=rep(0,length(epicentersdata[,1]))
for(m in 1:length(epicentersdata[,1])){
  epicentersdata$sites[m]=findnearestsite(epicentersdata[m,3],epicentersdata[m,2])
}
cor.test(epicentersdata$sites,epicentersdata[,6],method='pearson')

constraints=c()

----------
AUTOMATED VERSION OF THE ABOVE PHYLOGENETIC TEST
fisherresults=c()
pearsonresults=c()
pearsonresults2=c()
humiditypearson=c()
taikadaifisherresults=c()
familysetting='asjp'
familysetting='not'
#setting='africa'
setting='asia'
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
epicentersdata=set(setting)
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
#ancestral locations
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
transsites=c()
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
        transsites=append(transsites,findnearestsite(desclongs[m],desclats[m]))
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
        
        transsites=append(transsites,findnearestsite(desclongs[m],desclats[m]))
        
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
length(taikadaifisherresults[taikadaifisherresults<0.05])
length(pearsonresults[pearsonresults<0.05])
length(pearsonresults2[pearsonresults2<0.05])

asiafisherresults=fisherresults
asiapearsonresults=pearsonresults

africafisherresults
africapearsonresults

#correlations with the spread of agriculture
findnearestsite=function(long,lat){
  sitedists=c()
  for(m in 1:length(sites[,1])){
    sitedists=append(sitedists,distHaversine(sites[m,1:2],c(long,lat))/1000)
  }
  return(sites[which(sitedists==min(sitedists)),3])
}
#ASIA:
makemap(2)
#some made up locations and dates
sites=data.frame(c(120,30,5000),c(110,25,4500),c(90,25,2000),c(104,20,3500),c(127,35,1500),c(108,15,4000))
sites=t(sites)
rownames(sites)=c()

#sites from vietnam paper
#sites=data.frame(c(120,30,7000),c(112,27,8000),c(110,25,6000),c(90,25,3500),c(104,20,7000),c(108,15,6000))
sites=data.frame(c(120,30,7000),c(112,27,8000),c(110,25,6000),c(96,25,3500),c(100,18,5000))
sites=t(sites)
rownames(sites)=c()

makemap(2)
points(117,28)
#sites from rice paper
sites=data.frame(c(120,30,10000),c(117,28,10000),c(86,25,4000),c(110,35,5000))
sites=t(sites)
rownames(sites)=c()
text(sites[2,],sites[1,],'h')
makemap(1)
points(9,7)
#sites from africa paper
sites=data.frame(c(9,7,7000),c(30,25,3000),c(30,-10,2000),c(34,-20,1500))
sites=t(sites)
rownames(sites)=c()

n=5
points(sites[n,1],sites[n,2])

epicentersdata$sites=rep(0,length(epicentersdata[,1]))
for(m in 1:length(epicentersdata[,1])){
  epicentersdata$sites[m]=findnearestsite(epicentersdata[m,3],epicentersdata[m,2])
}
cor.test(epicentersdata$sites,epicentersdata[,6],method='pearson')

constraints=c()
n=1
which(epicentersdata2[,8]==names(newtree4$states)[n])
n=n+1









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
#matrix2=matrix(matrix2,max(epicentersdata[,6])+1,max(epicentersdata[,6])+1)
matrix2=matrix(matrix2,2,2)
matrix2=t(matrix2)
newtree4=sim.history(newtree3,matrix2,anc=1)
#newtree4=sim.char(newtree3,matrix2)
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
#    values=append(values,as.numeric(epicentersdata[member,6]))
    values=append(values,as.numeric(newtree4$states[names(newtree4$states)==epicentersdata[member,8]]))    
  }
}


matrix2=c(1,0.01,0.01,1)

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
length(results[results>0])
#permutation
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

length(megaresults[megaresults>0])

pvalues=c()
tvalues=c()
answers=rep(0,100)
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
famsample=rep(0,100)
autotypsample=rep(0,100)
mc=rep(0,100)
#africapvalues=c()
#africatvalues=c()
#eurasiapvalues=c()
#eurasiatvalues=c()
for(time in 1:100){
  
rate1=0.01
rate2=0.24
matrix2=c(-rate1,rate1,rate2,-rate2)
matrix2=matrix(matrix2,2,2)
newtree3=compute.brlen(multi2di(newtree2,random=T),1)
newtree4=sim.history(newtree3,matrix2,anc=1)
#newtree4=sim.char(newtree3,matrix2)
#epicentersdata2=epicentersdata[epicentersdata[,8] %in% names(newtree4$states),]
values=c()
lats=c()
longs=c()
names=c()
langnames=c()
region=c()
family=c()
for(member in 1:length(epicentersdata[,1])){
  if(epicentersdata[member,8] %in% names(newtree4$states) & ! epicentersdata[member,8] %in% names){
    names=append(names,epicentersdata[member,8])
    langnames=append(langnames,epicentersdata[member,1])
    lats=append(lats,as.numeric(epicentersdata[member,2]))
    longs=append(longs,as.numeric(epicentersdata[member,3]))
    #    values=append(values,as.numeric(epicentersdata[member,6]))
    values=append(values,as.numeric(newtree4$states[names(newtree4$states)==epicentersdata[member,8]]))   
    region=append(region,epicentersdata[member,9])
    family=append(family,epicentersdata[member,7])
  }
}
#simulating contact
for (time2 in 1:6){

  randlang=jsample(which(values==2),1)
  dists=c()
  for (member in 1:length(langnames)){
    dists=append(dists,distHaversine(c(longs[member],lats[member]),c(longs[randlang],lats[randlang]))/1000)
  }
  surround=c()
  for (member in 1:length(langnames)){
    if(dists[member] %in% sort(dists)[1:100]){
      surround=append(surround,langnames[member])
    }
  }
  for(member in 1:length(langnames)){
    if(langnames[member] %in% surround){
      values[member]=2
      
    }
  }  
}

map('world')
n=1
points(longs[values>n],lats[values>n],pch=16)
#points(longs[globalhumidities>n],lats[globalhumidities>n],pch=16)
globalhumidities=sapply(langnames,humidity)
autotyps=sapply(langnames,autotyp)
values=values[!is.na(globalhumidities)]
region=region[!is.na(globalhumidities)]
longs=longs[!is.na(globalhumidities)]
lats=lats[!is.na(globalhumidities)]
globalhumidities=globalhumidities[!is.na(globalhumidities)]

#cor.test(values[region==region1],globalhumidities[region==region1],method='pearson')
tvalues=append(tvalues,summary(glm(values~globalhumidities))$coefficients[6])
pvalues=append(pvalues,summary(glm(values~globalhumidities))$coefficients[8])
#africapvalues=append(africapvalues,summary(glm(values[region=='Africa']~globalhumidities[region=='Africa']))$coefficients[8])
#africatvalues=append(africatvalues,summary(glm(values[region=='Africa']~globalhumidities[region=='Africa']))$coefficients[6])
#if(summary(glm(values~globalhumidities))$coefficients[8]<0.05 & summary(glm(values~globalhumidities))$coefficients[6]>0){
  
#if(summary(glm(values[region==region1]~globalhumidities[region==region1]))$coefficients[8]<0.05 & summary(glm(values[region=='Africa']~globalhumidities[region=='Africa']))$coefficients[6]>0){
#  answers[time]=1
#}

#}
regiont=region1
if(summary(glm(values[region==regiont]~globalhumidities[region==regiont]))$coefficients[8]<0.05 & summary(glm(values[region==regiont]~globalhumidities[region==regiont]))$coefficients[6]>0){
  region1res[time]=1
}
regiont=region2
if(summary(glm(values[region==regiont]~globalhumidities[region==regiont]))$coefficients[8]<0.05 & summary(glm(values[region==regiont]~globalhumidities[region==regiont]))$coefficients[6]>0){
  region2res[time]=1
}
regiont=region3
if(summary(glm(values[region==regiont]~globalhumidities[region==regiont]))$coefficients[8]<0.05 & summary(glm(values[region==regiont]~globalhumidities[region==regiont]))$coefficients[6]>0){
  region3res[time]=1
}
regiont=region4
if(summary(glm(values[region==regiont]~globalhumidities[region==regiont]))$coefficients[8]<0.05 & summary(glm(values[region==regiont]~globalhumidities[region==regiont]))$coefficients[6]>0){
  region4res[time]=1
}
regiont=region5
if(summary(glm(values[region==regiont]~globalhumidities[region==regiont]))$coefficients[8]<0.05 & summary(glm(values[region==regiont]~globalhumidities[region==regiont]))$coefficients[6]>0){
  region5res[time]=1
}
fam=c()
famhum=c()
for(member in unique(family)){
  number=which(family==member)
  number=jsample(number,1)
  fam=append(fam,values[number])
  famhum=append(famhum,globalhumidities[number])
}
if(summary(glm(fam~famhum))$coefficients[8]<0.05 & summary(glm(fam~famhum))$coefficients[6]>0){
  famsample[time]=1
}
autotypvals=c()
autotyphums=c()
for(member in unique(autotyps)){
  number=which(autotyps==member)
  number=jsample(number,1)
  autotypvals=append(autotypvals,values[number])
  autotyphums=append(autotyphums,globalhumidities[number])
}
if(summary(glm(autotypvals~autotyphums))$coefficients[8]<0.05 & summary(glm(autotypvals~autotyphums))$coefficients[6]>0){
  autotypsample[time]=1
}
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
#africatvalues=africatvalues[pvalues<0.05]
#africapvalues=africapvalues[pvalues<0.05]
length(tvalues[pvalues<0.05][tvalues[pvalues<0.05]>0])
regiontres=region1res
length(regiontres[regiontres==1])
regiontotalres=region1res+region2res+region3res+region4res+region5res
length(regiontotalres[regiontotalres>1])
length(famsample[famsample==1])
length(autotypsample[autotypsample==1])
length(mc[mc==1])

length(answers[answers==1])

tvalues=tvalues[pvalues<0.05]
tvalues[tvalues>0]


region1='Eurasia'
humidities=sapply(epicentersdata[,1],humidity)
pvalues=c()
tvalues=c()
region=c()
for(time in 1:100){
#  map('world')
  randfams=sample(unique(epicentersdata[,7]),4)
  randlangs=sample(unique(epicentersdata[,1]),20)
  faketone=rep(0,length(epicentersdata[,7]))
  for(member in 1:length(epicentersdata[,7])){
    if(epicentersdata[member,7] %in% randfams){
      faketone[member]=1
      
    }
  }
#  points(epicentersdata[,3][epicentersdata[,7] %in% randfams],epicentersdata[,2][epicentersdata[,7] %in% randfams],pch=16)
  pvalues=append(pvalues,summary(glm(faketone~humidities))$coefficients[8])
  tvalues=append(tvalues,summary(glm(faketone~humidities))$coefficients[6])
  summary(glm(faketone~humidities))$coefficients[8]
  summary(glm(faketone~humidities))$coefficients[6]
#  region=append(region,summary(glm(faketone[epicentersdata[,9]==region1]~humidities[epicentersdata[,9]==region1]))$coefficients[8])
#  region=append(region,summary(glm(faketone[epicentersdata[,9]==region1]~humidities[epicentersdata[,9]==region1]))$coefficients[6])
  
}
length(tvalues[pvalues<0.05][tvalues[pvalues<0.05]<0])
region[which(tvalues[pvalues<0.05]>0)*2][which(region[which(tvalues[pvalues<0.05]>0)*2]<0.05)]

#simulating contact
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
for(time in 1:1){
faketone=rep(0,length(epicentersdata[,7]))    
for (time2 in 1:6){
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


pickrandomtip=function(newtree){


  nodes=c()
  current=newtree$edge[1]
  while(length(which(newtree$edge[,1]==current))>0){
    current=newtree$edge[jsample(which(newtree$edge[,1]==current),1),2]

  }
  newtree$tip[current]
  return(newtree$tip[current])
  
}
for(time in 1:100){
  lol=pickrandomtip(newtree2)
  points(epicentersdata[,3][epicentersdata[,8]==lol],epicentersdata[,2][epicentersdata[,8]==lol],pch=16)  
  
}

points(epicentersdata[,3][epicentersdata[,7] %in% randfams],epicentersdata[,2][epicentersdata[,7] %in% randfams],pch=16)
humidities=sapply(epicentersdata[,1],humidity)
densities=c()
for(member in 1:length(epicentersdata[,1])){
  densities=append(densities,density(epicentersdata[member,2],epicentersdata[member,3],100))
  length(densities)
}

summary(glm(values[region==region1]~globalhumidities[region==region1]))
values

map('world')
n=1
points(longs[values>n],lats[values>n],pch=16)

newtree4$states=as.numeric(newtree4$states)
epicentersdata2[,6]=newtree4$states
makemap(2)
generaldrawcurve(9,'red',epicentersdata2,2,3,6)
generaldrawcurve(6,'yellow',epicentersdata2,3,4,2)
newtree4$states

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
generaldrawcurve(4,'red',epicentersdata2,2,3,6)
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
    polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border='white',lwd=2)
    
    fake=append(fake,areaPolygonkm(spline.poly(as.matrix(as.data.frame(chuld)),100)))
  } 
  for (n in 1:min(length(fake),length(real))){
    if (!abs(unique(rev(real)[n]))<abs(unique(rev(fake)[n]))){
      significances[n]=significances[n]+1
    }
  }
}
significances


#giving dates as branchlengths to tree
changebranchlengthsdefault=function(tree,nodename,date){
  node=which(c(tree$tip,tree$node)==nodename)
  for(member in tree$tip){
    if(isancestor(tree,nodename, member)){
      edges=c()
      current=which(c(tree$tip,tree$node)==member)
      while(!current==node){
        edges=append(edges,which(tree$edge[,2]==current))
        current=tree$edge[which(tree$edge[,2]==current),1]
      }
      for(m in 1:length(edges)){
        if(m==1){
          tree$edge.length[edges[m]]=date/(2**(length(edges)-1))
        }
        if(m>1){
          tree$edge.length[edges[m]]=date/(2**(length(edges)-m+1))
        }        
        
      }
    }
  }
  return(tree)
}
# changebranchlengths=function(tree,nodename,date,constrainednodename,constrainedage){
#   node=which(c(tree$tip,tree$node)==nodename)
#   constrainednode=which(c(tree$tip,tree$node)==constrainednodename)
#   #find height of node
#   edges=c()
#   current=node
#   
#   while(current>length(tree$tip)){
#     edges=append(edges,which(tree$edge==current)[1])
#     current=tree$edge[which(tree$edge[,1]==current)[1],2]
#   }
#   a=sum(tree$edge.length[edges]) 
#   
#   for(member in tree$tip){
#     if(isancestor(tree,nodename, member)){
#       edges=c()
#       current=which(c(tree$tip,tree$node)==member)
#       while(!current==node){
#         edges=append(edges,which(tree$edge[,2]==current))
#         current=tree$edge[which(tree$edge[,2]==current),1]
#       }
#       for(m in 1:length(edges)){
#         if(m==1){
#           tree$edge.length[edges[m]]=date/(2**(length(edges)-1))
#         }
#         if(m>1){
#           tree$edge.length[edges[m]]=date/(2**(length(edges)-m+1))
#         }        
#         
#       }
#     }
#   }
#   edges=c()
#   current=node
#   while(!current==constrainednode){
#     edges=append(edges,which(tree$edge[,2]==current))
#     current=tree$edge[which(tree$edge[,2]==current),1]
#   }
#   constant=(constrainedage-date)/(constrainedage-a)
#   tree$edge.length[edges[1]]=tree$edge.length[edges[1]]*constant
#   return(tree)
# }

changebranchlengths=function(tree,nodename,date){
  node=which(c(tree$tip,tree$node)==nodename)
  #find height of node
  edges=c()
  current=node
  
  while(current>length(tree$tip)){
    edges=append(edges,which(tree$edge==current)[1])
    current=tree$edge[which(tree$edge[,1]==current)[1],2]
  }
  a=sum(tree$edge.length[edges]) 
  edges=c()
  current=node
  while(!current==length(tree$tip)+1){
    edges=append(edges,which(tree$edge[,2]==current))
    current=tree$edge[which(tree$edge[,2]==current),1]
  }
  b=tree$edge.length[edges[1]]
  for(member in tree$tip){
    if(isancestor(tree,nodename, member)){
      edges=c()
      current=which(c(tree$tip,tree$node)==member)
      while(!current==node){
        edges=append(edges,which(tree$edge[,2]==current))
        current=tree$edge[which(tree$edge[,2]==current),1]
      }
      for(m in 1:length(edges)){
        if(m==1){
          tree$edge.length[edges[m]]=date/(2**(length(edges)-1))
        }
        if(m>1){
          tree$edge.length[edges[m]]=date/(2**(length(edges)-m+1))
        }        
        
      }
    }
  }
  edges=c()
  current=node
  while(!current==length(tree$tip)+1){
    edges=append(edges,which(tree$edge[,2]==current))
    current=tree$edge[which(tree$edge[,2]==current),1]
  }
  tree$edge.length[edges[1]]=b+a-date
  return(tree)
}

newtree5=changebranchlengthsdefault(newtree3,'Sino-Tibetan',5000)
newtree5=changebranchlengths(newtree5,'Min',3000)

306  139
[208,]  305  308

newtree5=changebranchlengthsdefault(newtree3,'Sino-Tibetan',5261)
newtree5=changebranchlengths(newtree5,'Sinitic',2982)
newtree5=changebranchlengths(newtree5,'Tibeto-Burman',4203)
newtree5=changebranchlengths(newtree5,'Kuki-Chin-Naga',2982)
newtree5=changebranchlengths(newtree5,'Karenic',2345)
newtree5=changebranchlengths(newtree5,'Qiangic',4000)
newtree5=changebranchlengths(newtree5,'Himalayish',3182)
newtree5=changebranchlengths(newtree5,'Lolo-Burmese',3436)
newtree5=changebranchlengths(newtree5,'Nungish',1955)


plot(newtree5)
newtree5$node
dist.nodes(newtree5)[which(c(newtree5$tip,newtree$node)=='cpx'),which(c(newtree5$tip,newtree5$node)=='SouthChinese')]

#ASIA DATES
"Sino-Tibetan"            
"Sinitic"                   
"Tibeto-Burman" 
"Kuki-Chin-Naga"
"Karenic"
"Qiangic"
"Himalayish"
"Lolo-Burmese"
"Nungish"

for(m2 in 1:length(newtree3$node)){
  if(isancestor(newtree3,newtree3$node[m2],'hak')){
    answer=append(answer,m2)
  }
}
answer


'Japanese' %in% epicentersdata[,1]

newtree3=subtrees(newtree3)[4]

ace

ancstates


ancstates[10]

isancestornumbers(newtree3,298+length(newtree3$tip),301+length(newtree3$tip))


newtree3$node
isancestor
newtree3$node


results2$lik.anc[,1]



newtree2

epicentersdata[which(epicentersdata[,8]=='taq'),6]
newtree
map("world",xlim=c(-20,60), ylim=c(-40,40))
for (n in min(epicentersdata[,6]):max(epicentersdata[,6])){
  generaldrawcurve(n,heat.colors(max(epicentersdata[,6])-min(epicentersdata[,6]),alpha=0.4)[n-min(epicentersdata[,6])+1],epicentersdata,2,3,6)
}
trim=8
generaldrawcurve(max(epicentersdata[,6])-trim,'red',epicentersdata,2,3,6)
#trims: africa ,asia 5

generaldrawcurve
epicentersdata[,12][epicentersdata[,14]==1]
epicentersdata[lang,6]
quickwhich(epicentersdata,)
epicentersdata[,12]

lang=sample(which(epicentersdata[,13]<radius1))
lang

epicentersdata[lang,12]=sim[lang]

epicentersdata[,12]



lang=sample(which(epicentersdata[,12]==max(epicentersdata[,12])),1)


lang
epicentersdata[lang,12]
epicentersdata[96,12]

quickpoints(epicentersdata,2,3,12,6)
radius=500




epicentersdata[,6]
epicentersdata=epicentersdata[epicentersdata[,8] %in% newtree$tip,]
matrix=dist.nodes(newtree)
geneticdistance=0.0001
#phylogenetic version
for (iteration in 1:1){
  lang=sample(1:length(epicentersdata[,1]),1)
  list=c()
  for (member in 1:length(epicentersdata[,1])){
    value=epicentersdata[lang,12]
    if (matrix[match(epicentersdata[member,8],c(newtree$tip.label,newtree$node.label)),match(epicentersdata[lang,8],c(newtree$tip.label,newtree$node.label))]<geneticdistance){
      list=append(list,member)
    }
  }
  #list=sample(list,number)
  for (member in list){
    epicentersdata[member,12]=value
  }
}
list

SIMULATION OF LANGUAGE FAMILIES EVOLVING TONES
which(c(newtree$tip.label,newtree$node.label)=='Atlantic-Congo')
newtree$edge[newtree$edge[,1]==startingpoint,2]
startingpoint=600
startingvalue=5
traits=rep(0,length(c(newtree$tip.label,newtree$node.label)))
traits[newtree$edge[newtree$edge[,1]==startingpoint,2]]=startingvalue
startingpoint



epicentersdata[lang,7]

spread=function(){
  value=epicentersdata[lang,6]
  distHav
}

epicentersdata=read.csv("~/Documents/Epicentres/epicentersafrica.txt",header=F,stringsAsFactors=F,sep='\t')
map("worldHires",xlim=c(-20,60), ylim=c(-40,40))
points(epicentersdata[,3],epicentersdata[,2],pch=16,cex=0.5)
epicentersdata[,6]
#randomly assign values
prior=c(0.5,0.3,0.1,0.05,0.01,0.0001)
sim=sample(1:6,length(epicentersdata[,6]),replace=T,prob=prior)
epicentersdata[,12]=sim
#select a language, and make languages in a certain radius change their number of tones according to that language

reconcile=function(occupier,occupied){
  if (occupied<occupier){
    return (occupier-1)
  }
  if (occupied>occupier){
    return (occupier+1)
  }
  if (occupier==occupied){
    return(occupier)
  }  
  
}

lang=sample(1:length(epicentersdata[,1]),1)
lang=sample(which(epicentersdata[,12]==max(epicentersdata[,12])),1)

radius=2000
for (member in 1:length(epicentersdata[,1])){
  if (distHaversine(epicentersdata[member,c(3,2)],epicentersdata[lang,c(3,2)])/1000<radius){
    epicentersdata[member,12]=reconcile(epicentersdata[member,12],epicentersdata[lang,12])
  }
}
generaldrawcurve(5,'red',epicentersdata,2,3,12)

epicentersdata[,12]
#draw curve
map("worldHires",xlim=c(-20,60), ylim=c(-40,40))

max=max(sim)
min=min(sim)


generaldrawcurve(8,'red',epicentersdata,2,3,12)

quickpoints=function(frame,lat,long,value,valuesetting){
  points(frame[,long][frame[,value]==valuesetting],frame[,lat][frame[,value]==valuesetting],pch=16,col='black')
}
quickwhich=function(response,frame,value,setting){
  frame[which(frame[,value]==setting),response]
}
isancestor=function(tree,node1,node2){
  a=match(node1,c(tree$tip.label,tree$node.label))
  b=match(node2,c(tree$tip.label,tree$node.label))
  d=c()
  mother=b
  while (mother %in% tree$edge[,2]){
    d=append(d,tree$edge[which(tree$edge[,2]==mother),1])
    mother=tree$edge[which(tree$edge[,2]==mother),1]
  }
  answer=ifelse(a %in% d,1,0)
  return(answer)
}
isancestornumbers=function(tree,node1,node2){
  a=node1
  b=node2
  d=c()
  mother=b
  while (mother %in% tree$edge[,2]){
    d=append(d,tree$edge[which(tree$edge[,2]==mother),1])
    mother=tree$edge[which(tree$edge[,2]==mother),1]
  }
  answer=ifelse(a %in% d,1,0)
  return(answer)
}
isancestor(newtree,'World','Cushitic')
quickpoints(epicentersdata,2,3,6,6)
quickwhich(3,epicentersdata,6,8)
epicentersdata[1,]

showgenus=function(frame,language,genusname,lat,long){
  for (member in frame[,language]){
    if (isancestor(newtree,genusname,member)){
      quickpoints(frame,lat,long,language,member)
    }
  }
}
showgenusnumbers=function(frame,language,lat,long,treeobj,node1){
  for (member in frame[,language]){
    if(member %in% treeobj$tip){
      if (isancestornumbers(treeobj,node1,which(treeobj$tip.label==member))){
        quickpoints(frame,lat,long,language,member)
      }      
    }

  }
}
showgenusnumbers(epicentersdata,8,2,3,newtree3,600)
quickpoints(epicentersdata,)
quickpoints(epicentersdata,2,3,8,'kab')

map("world",xlim=c(-20,60), ylim=c(-40,40))
showgenus(epicentersdata,8,"CentralTai",2,3)
newtree3$node
immdesc=function(tree,node1){
  a=which(c(newtree$tip.label,newtree$node.label)==node1)
  c(newtree$tip.label,newtree$node.label)[tree$edge[,2][tree$edge[,1]==a]]
}
immdescnumbers=function(tree,node1){
  tree$edge[,2][tree$edge[,1]==node1]
}

desc=function(tree,node1,frame,language,value){
  results=c()
  for (member in frame[,language]){
    if (isancestor(tree,node1,member)==1){
      results=append(results,member)
    }
  }
  frame[,value][frame[,language] %in% results]
}

desc(newtree,'Afro-Asiatic',epicentersdata,8,6)
immdesc(newtree3,'Bantoid')
genus='Atlantic-Congo'
showtone=function(genus){
  points(desc(newtree,genus,epicentersdata,8,3),desc(newtree,genus,epicentersdata,8,2),pch=16,col=heat.colors(10)[desc(newtree,genus,epicentersdata,8,6)+1])
}
n=1
map("worldHires",xlim=c(-20,60), ylim=c(-40,40))
showtone(immdesc(newtree,'Atlantic-Congo')[n])
n=n+1



showtone('Balanta')
x='Atlantic-Congo'
x=immdesc(newtree,x)[1]
map("worldHires",xlim=c(-20,60), ylim=c(-40,40))
showtone(x)

x

epicentersdata[,6]
newtree
phylotraits=phylo4d(newtree,)
install.packages('geiger')
library(geiger)
number=function(tree,x){
  list=c(tree$tip.label,tree$node.label)
  which(list==x)
}
number(newtree,'Atlantic-Congo')
nigercongo=extract.clade(newtree,number(newtree,'Atlantic-Congo'))
plot(nigercongo)
nigercongoframe=cbind(epicentersdata[,8][epicentersdata[,7]=='Niger-Kongo'],epicentersdata[,6][epicentersdata[,7]=='Niger-Kongo'])
nigercongoframe=nigercongoframe[nigercongoframe[,1] %in% nigercongo$tip,]
nigercongo=drop.tip(nigercongo,nigercongo$tip[!nigercongo$tip %in% nigercongoframe[,1]])
row.names(nigercongoframe)=nigercongoframe[,1]
nigercongoframe=nigercongoframe[,2]

install.packages("picante")
install.packages("ape")
install.packages("adephylo")
install.packages("ade4")
install.packages("phylobase")
install.packages("geiger")
install.packages("phytools")

### Load them:

library(picante)
library(ape)
library(adephylo)
library(ade4)
library(phylobase)
library(geiger)
library(phytools)
phylotraits=phylo4d(nigercongo,nigercongoframe)
moran.test <- abouheif.moran(phylotraits,method="Abouheif")
moran.test
trait=nigercongoframe[,2]
names(trait)=row.names(nigercongoframe)
trait=as.numeric(trait)
phylosig(nigercongo, trait, method="lambda", test=TRUE, nsim=999)
?phylosig
trait
trait=fastBM(nigercongo)
nigercongoframe



phyloframe=epicentersdata[epicentersdata[,8] %in% newtree$tip,]
which(epicentersdata[,8])

newtree2=drop.tip(newtree,newtree$tip[!newtree$tip %in% phyloframe[,8]])
trait=phyloframe[,6][which(phyloframe[,8]==)]
length(phyloframe[,8])
length(unique(phyloframe[,8]))
list=c()
phyloframe2=c()
phyloframe3=c()

for (member in phyloframe[,1]){
  if (phyloframe[which(phyloframe[,1]==member),8] %in% list){
    phyloframe2=phyloframe2[,!phyloframe2[,1]==phyloframe2[member,1]]
  }
  list=append(list,phyloframe[which(phyloframe[,1]==member),8])
}

for (member in 1:length(phyloframe[,8])){
  if (!phyloframe[member,8] %in% list){
    phyloframe2=append(phyloframe2,phyloframe[member,8])
    phyloframe3=append(phyloframe3,phyloframe[member,6])    
  }
  list=append(list,phyloframe[member,8])
}
trait=phyloframe3
names(trait)=phyloframe2
length(trait)

phylosig(newtree2, trait, method="K", test=TRUE, nsim=999)
phylotraits=phylo4d(newtree2,trait)
moran.test <- abouheif.moran(phylotraits,method="Abouheif")
moran.test
plot(moran.test)

install.packages('RCurl')
library(RCurl)

getURL("https://archive.org/details/WBAL_20090703_110000_Today&output=json")
foo=getURL("http://www.jeremycollins.org")
library(jsonlite)
fromJSON(foo)

ADDING NOISE

AFRICA
epicentersdata=read.csv("~/Documents/Epicentres/epicentersafrica.txt",header=F,stringsAsFactors=F,sep='\t')
epicentersdata=epicentersdata[epicentersdata[,6]>1,]
origin=c(19.2,5.114)
origin=c(11.52,3.87)

number=100
tochange=sample(1:1,number,replace=T)
tochange
epicentersdata[tochange,6]=epicentersdata[tochange,6]+1

epicentersdata[1,]

epicentersdata=read.csv("~/Documents/Epicentres/epicentersworld.txt",header=F,stringsAsFactors=F,sep='\t')
epicentersdata=epicentersdata[c(1:3,6:11)]
#epicentersdata=cbind(epicentersdata,humidityvalues)
write.table(epicentersdata,file='/Users/jercol/Documents/Epicentres/Complete/Supplementary Tables.txt',sep='\t',quote=F,col.names=F,row.names=F)

install.packages('gam')
library(gam)
data(gam.data)


library(XML)
library(RCurl)
latlon2ft <- function(origin,destination){
  xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false')
  xmlfile <- xmlParse(getURL(xml.url))
  dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
  distance <- as.numeric(sub(" km","",dist))
  ft <- distance*3.28084 # FROM METER TO FEET
  return(ft)
}

latlon2ft(origin='37.193489,-121.07395',destination='20,-121.046586')
points(-121,37)
