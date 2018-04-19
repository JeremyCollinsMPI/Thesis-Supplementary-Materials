install.packages('ape')
library(ape)
install.packages('maps')
library(map)
install.packages('OutbreakTools')
library(OutbreakTools)
install.packages('SDMTools')
library(SDMTools)
install.packages('geosphere')
library(geosphere)
install.packages('binr')
library(binr)
mtdnatree=read.annotated.nexus('/Users/jeremycollins/Documents/Phylogeography/MTDNA/Analyses To Publish/World/Run 9/output5bconverted.tree')
nodelocation=function(number,tree){
  if(number==length(tree$tip.label)+1){
    return(c(tree$root.annotation$locationsnewTrait1,tree$root.annotation$locationsnewTrait2))
  }
  else{
    x=which(tree$edge[,2]==number)
    return(c(tree$annotations[[x]]$locationsnewTrait1,tree$annotations[[x]]$locationsnewTrait2))
  }
}
nodetime=function(number,tree){
  if(number==length(tree$tip.label)+1){
    return(tree$root.annotation$height)
  }
  else{
    x=which(tree$edge[,2]==number)
    return(tree$annotations[[x]]$height)
  }  
}
nodelocation2=function(number,tree){
  if(number==length(tree$tip.label)+1){
    return(c(tree$root.annotation$locationslocation1,tree$root.annotation$locationslocation2))
  }
  else{
    x=which(tree$edge[,2]==number)
    return(c(tree$annotations[[x]]$locationslocation1,tree$annotations[[x]]$locationslocation2))
  }
}
nodelocation(1,mtdnatree)
movements=list()
times=c()
times1=c()
times2=c()
for(y in 1:length(mtdnatree$edge[,2])){
  movements=append(movements,list(c(nodelocation(mtdnatree$edge[y,1],mtdnatree),nodelocation(mtdnatree$edge[y,2],mtdnatree))))
  times=append(times,nodetime(mtdnatree$edge[y,1],mtdnatree)-nodetime(mtdnatree$edge[y,2],mtdnatree))
  times1=append(times1,nodetime(mtdnatree$edge[y,1],mtdnatree))
  times2=append(times2,nodetime(mtdnatree$edge[y,2],mtdnatree))
  
}


map('world')
for(m in 1:length(movements)){
  segments(movements[[m]][2],movements[[m]][1],movements[[m]][4],movements[[m]][3],col='red')
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
drawcurve=function(longs,lats,colour){
  testpts=structure(list(x=longs,y=lats))
  chuld <- lapply(testpts,"[",chull(testpts))
  polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border=colour,lwd=2)
}

languagetree=read.annotated.nexus('/Users/jeremycollins/Documents/Phonotactics/New Workflow/Run 53/2.tree')

drawclade=function(number,tree){
  lats=file[which(file[,5] %in% extract.clade(tree,number)$tip.label),2]
  longs=file[which(file[,5] %in% extract.clade(tree,number)$tip.label),3]
  drawcurve(longs,lats,'red')
}
returnpoly=function(number,tree){
  lats=file[which(file[,5] %in% extract.clade(tree,number)$tip.label),2]
  longs=file[which(file[,5] %in% extract.clade(tree,number)$tip.label),3]
  testpts=structure(list(x=longs,y=lats))
  chuld <- lapply(testpts,"[",chull(testpts))
  return(spline.poly(as.matrix(as.data.frame(chuld)),100))
}

p=1
p
map('world')
drawclade(length(languagetree$tip.label)+p,languagetree)
p=p+1
polygon=returnpoly(length(languagetree$tip.label)+p,languagetree)
pnt.in.poly(data.frame(c(70),c(30)),polygon)
result=c()
for(m in 1:length(movements)){
  if(pnt.in.poly(data.frame(c(movements[[m]][2]),c(movements[[m]][1])),polygon)$pip==1){
    if(pnt.in.poly(data.frame(c(movements[[m]][4]),c(movements[[m]][3])),polygon)$pip==1){
      result=append(result,m)
    }
     
  }   
}
for(n in 1:length(result)){
  o=result[n]
  segments(movements[[o]][2],movements[[o]][1],movements[[o]][4],movements[[o]][3],col='red')
}

polynumber1=127
polynumber2=63
polygon1=returnpoly(length(languagetree$tip.label)+polynumber1,languagetree)
polygon2=returnpoly(length(languagetree$tip.label)+polynumber2,languagetree)
drawclade(length(languagetree$tip.label)+polynumber1,languagetree)
drawclade(length(languagetree$tip.label)+polynumber2,languagetree)

movementdists=c()
for(o in 1:length(movements)){
  movementdists=append(movementdists,distHaversine(c(movements[[o]][2],movements[[o]][1]),c(movements[[o]][4],movements[[o]][3]))/1000)
}
bin_number=200
distancebins=bins.quantiles(movementdists,bin_number,bin_number+5)[[1]]

h=hist(movementdists,breaks=100)
xfit<-seq(min(movementdists),max(movementdists),length=40) 
yfit<-dnorm(xfit,mean=0,sd=sd(movementdists)) 
yfit <- yfit*diff(h$mids[1:2])*length(movementdists) 
lines(xfit, yfit, col="black", lwd=2)


threshold1=sort(movementdists)[distancebins[m]]
threshold2=sort(movementdists)[distancebins[m+1]]
movements2=movements[which(movementdists>threshold1 & movementdists<threshold2)]
map('world2')
for(o in 1:length(movements2)){
   segments(movements2[[o]][2],movements2[[o]][1],movements2[[o]][4],movements2[[o]][3],col='red')
    points(movements2[[o]][2],movements2[[o]][1],pch=16,col='red')
} 
m=m+1

times=lapply(times,abs)
speeds=c()
for(o in 1:length(movementdists)){
  speeds=append(speeds,max(0,movementdists[o]/times[[o]][1]))
}
speedbins=bins.quantiles(speeds,200,205)[[1]]
m=180
speedthreshold1=sort(speeds)[speedbins[m]]
speedthreshold2=sort(speeds)[speedbins[m+1]]
movements3=movements[which(speeds>=speedthreshold1 & speeds<speedthreshold2)]
map('world2')
for(o in 1:length(movements3)){
   segments(movements3[[o]][2],movements3[[o]][1],movements3[[o]][4],movements3[[o]][3],col='red')
  points(movements3[[o]][2],movements3[[o]][1],pch=16,col='red')
}
speedthreshold1=speedthreshold1+min(speeds[speeds>0])
m=m+1


countries=list()
poly1poly2=c()
for(m in 1:length(movements)){
  country1=map.where('world',c(movements[[m]][2]),c(movements[[m]][1]))
  country2=map.where('world',c(movements[[m]][4]),c(movements[[m]][3]))
  countries=append(countries,list(c(country1,country2)))
}

poly1poly2=c()
for(m in 1:length(movements)){
  if(pnt.in.poly(data.frame(c(movements[[m]][2]),c(movements[[m]][1])),polygon1)$pip==1){
    if(pnt.in.poly(data.frame(c(movements[[m]][4]),c(movements[[m]][3])),polygon2)$pip==1){
      poly1poly2=append(poly1poly2,m)
    }
  }
}

for(o in poly1poly2){
  segments(movements[[o]][2],movements[[o]][1],movements[[o]][4],movements[[o]][3],col='green')
}
map('world',c('India','Vietnam'))
country_results=c()
country1='Taiwan'
country2='Indonesia'
for(o in 1:length(countries)){
  if(!is.na(countries[[o]][1]) & !is.na(countries[[o]][2])){
    if(countries[[o]][1]==country1 & countries[[o]][2]==country2){
      segments(movements[[o]][2],movements[[o]][1],movements[[o]][4],movements[[o]][3],col='green')
      country_results=append(country_results,o)
    }    
  }
}
length(country_results)
mo=c()
for(o in 1:length(countries)){
  mo=append(mo,countries[[o]][1])
}
unique(mo)
countries[1:500]
sort(unique(mo))
which(mo=='Japan:Honshu')

nearestlanguage=function(lat,long){
  file2=file[!file$Ancient.language==1,]
  cands=c()
  threshold=0
  while(length(cands)<5){
    threshold=threshold+5
    for(m in 1:length(file2[,1])){
      if(abs(file2[m,2]-lat)<threshold & abs(file2[m,3]-long)<threshold){
        cands=append(cands,m)
      }
    }
    cands=unique(cands)
  }
  dists=c()
  for(m in 1:length(cands)){
    dists=append(dists,distHaversine(c(long,lat),c(file2[cands[m],3],file2[cands[m],2])))
  
  }
  return(file2[cands[which(dists==min(dists))],1])
}
nearestlanguage(22.3,114.5)
lat=55
long=0
file[1578,1]
languages=list()
for(m in 1:length(movements)){
  languages=append(languages,list(c(nearestlanguage(movements[[m]][1],movements[[m]][2]),nearestlanguage(movements[[m]][3],movements[[m]][4]))))
}
languages
lat1s=c('lat1')
for(m in 1:length(movements)){
  lat1s=append(lat1s,movements[[m]][1])
}
lat2s=c('lat2')
for(m in 1:length(movements)){
  lat2s=append(lat2s,movements[[m]][2])
}
lat3s=c('lat3')
for(m in 1:length(movements)){
  lat3s=append(lat3s,movements[[m]][3])
}
lat4s=c('lat4')
for(m in 1:length(movements)){
  lat4s=append(lat4s,movements[[m]][4])
}
movements_df=data.frame(lat1s,lat2s,lat3s,lat4s,c('times1',times1),c('times2',times2),c('dists',movementdists),stringsAsFactors = F)
write.table(movements_df,'/Users/jeremycollins/Documents/Phylogeography/MTDNA/Analyses To Publish/World/movements2000.csv',col.names=F,row.names=F,quote=F,sep='\t')

list_to_frame=function(x){
  y=data.frame(matrix(unlist(x),nrow=length(x),byrow=T),stringsAsFactors = F)
  return(y)
}
movement_frame=list_to_frame(movements)
long2=-5
lat1=35
long1=-5
lat2=25
length(movement_frame[movement_frame[,1]>lat1 & movement_frame[,1]<lat1+5 & movement_frame[,2]>long1 & movement_frame[,2]<long1+5 & movement_frame[,3]>lat2 & movement_frame[,3]<lat2+5 & movement_frame[,4]>long2 & movement_frame[,4]<long2+5,1])
long2=long2+5

size=5
movement_frame2=movement_frame
for(m in 1:4){
  for(n in 1:length(movement_frame2[,1])){
    movement_frame2[n,m]=floor(movement_frame2[n,m])
    movement_frame2[n,m]=movement_frame2[n,m]-(movement_frame2[n,m] %% size)
  }
}
movement_frame2=movement_frame2[!movement_frame2[,1]==movement_frame2[,3] & ! movement_frame2[,2]==movement_frame2[,4],]

magic_numbers=c()
for(m in 1:length(unique(movement_frame2)[,1])){
  magic_numbers=append(magic_numbers,length(movement_frame2[movement_frame2[,1]==unique(movement_frame2)[m,1] & movement_frame2[,2]==unique(movement_frame2)[m,2] & movement_frame2[,3]==unique(movement_frame2)[m,3] & movement_frame2[,4]==unique(movement_frame2)[m,4],1]))
}

movement_frame2[which(magic_numbers %in% sort(magic_numbers,decreasing=T)[1:2]),]
map('world')
to_include=30
for(m in 1:to_include){
  member=movement_frame2[which(magic_numbers %in% sort(magic_numbers,decreasing=T)[1:to_include]),][m,]
  segments(member[1,2]+size/2,member[1,1]+size/2,member[1,4]+size/2,member[1,3]+size/2,col='red')
}
member
