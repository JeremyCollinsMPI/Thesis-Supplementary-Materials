happ=match('Hv0a',haps)
temp[,11]
oldtemp=temp
temp=data.frame(phonotactics,phonotacticsgenedata) 
write(temp[74,''])
map("worldHires",xlim=c(-30,180), ylim=c(-40,80))
happ=3
haps[happ]
happ=match('B4a1a1a',haps)
map("worldHires",xlim=c(-30,180), ylim=c(-40,80))
points(temp[,3],temp[,2],pch=19,col='cyan')
points(temp[,3][temp[,happ+10]>0],temp[,2][temp[,happ+10]>0],pch=19,col='blue')
points(temp[,3][temp[,happ+10]>0.3],temp[,2][temp[,happ+10]>0.3],pch=19,col='red')
Codas
[1] "B4" "H"  "HV" "B"  "T"  "JT"
Codas mixed effects
[1] "B4a1a1"  "B4"      "HV0a"    "HV0a"    "B"       "T2"     
[7] "U5a"     "U5b"     "U5b1"    "B4a"     "B4a1"    "M5"     
[13] "HV0"     "B4a1a1"  "B4a1a1a"
Tone
[1] "B5a"  "M7b1" "R11"  "U"   
Tone mixed effects
[1] "B4g"  "B5a"  "F3a"  "M33c" "M33c" "M71"  "M7b1" "F3a"  "N21" 
[10] "R11"  "B5"   "B4a3" "F1a"  "M7b"  "D4d"  "A6"   "M7c1"
Onsets 
[1] "R2"  "U8"  "K"   "T"   "H1"  "U5a" "N"   "U8b"
coda=phonotactics[,5]
pvalues=c()
for (member in 3:254){
  s=lm(data[,member]~coda)
  pvalues=append(pvalues,lmp(s))  
}
pvalues=pvalues[!is.na(pvalues)]
length(pvalues[pvalues<0.05])
length(pvalues[pvalues<0.35 & pvalues>0.3])
hist(pvalues,breaks=20,xlab='P-values',main='')


summary(lm(phonotactics[,6]~phonotacticsgenedata[,match('B5a',haps)+2]))
summary(glm(binarise(phonotactics[,6])~data[,match('HV',haps)+2]))
x=lm(phonotactics[,5]~data[,match('V',haps)+2])
plot(phonotactics[,5]~data[,match('V',haps)+2],xlab='Haplogroup V',ylab='Coda')
plot(x)

coda=phonotactics[,6]
hist(pvalues,breaks=20)
pvalues[!is.na(pvalues)]
install.packages('lme4')
library(lme4)
s=lmer(coda~data[,match('B5a',haps)+2]+(1|phonotactics[,7]))
t=lmer(coda~(1|phonotactics[,7]))
anova(s,t)

s=glmer(binarise(coda)~data[,match('U',haps)+2]+(1|phonotactics[,7]))
t=glmer(binarise(coda)~(1|phonotactics[,7]))
anova(s,t)

anova(s,t)[2,6]
match('HV',haps)
HV=83,H1=108,H3=109,H4=110,H5=111,H8=112,H1a=226,V=85,H=82
coda=phonotactics[,5]
effectsizes=c()
for (member in 3:254){
  s=lmer(coda~data[,member]+(1|phonotactics[,7]))
  t=lmer(coda~(1|phonotactics[,7]))
  effectsizes=append(effectsizes,anova(s,t)[2,6])  
}
results=c()
for (effectsize in effectsizes[!is.na(effectsizes)]){
  if (effectsize>6){
    results=append(results,haps[match(effectsize,effectsizes)])
  }
  
}
results

coda=phonotactics[,5]
s=lm(data[,83]~coda)
summary(s)$adj.r.squared
effectsizes=c()
for (member in 3:254){
  s=lm(data[,member]~coda)
  effectsizes=append(effectsizes,summary(s)$adj.r.squared)  
}
results=c()
for (effectsize in effectsizes[!is.na(effectsizes)]){
  if (effectsize>0.05){
    results=append(results,haps[match(effectsize,effectsizes)])
  }
  
}
results
match('M13a',haps)
effectsizes
temp[25,9]
results=c()
for (pvalue in pvalues[!is.na(pvalues)]){
  if (pvalue<0.01){
    results=append(results,haps[match(pvalue,pvalues)])
  }
  
}
results
match('F1a',haps)

threshold=0.8
mapping=function(x){
  happ=match(x,haps)
  map("worldHires",xlim=c(-20,180), ylim=c(-40,80))
  points(temp[,3],temp[,2],pch=19,col='cyan')
  points(temp[,3][temp[,happ+10]>0],temp[,2][temp[,happ+10]>0],pch=19,col='blue')
  points(temp[,3][temp[,happ+10]>threshold],temp[,2][temp[,happ+10]>threshold],pch=19,col='red')
  return(median(coda[temp[,happ+10]>0]))
}
threshold=0.5
mapping2=function(x){
  happ=x
  map("worldHires",xlim=c(-20,180), ylim=c(-40,80))
  points(temp[,3],temp[,2],pch=19,col='cyan')
  points(temp[,3][temp[,happ+10]>0],temp[,2][temp[,happ+10]>0],pch=19,col='blue')
  points(temp[,3][temp[,happ+10]>threshold],temp[,2][temp[,happ+10]>threshold],pch=19,col='red')
  return(median(coda[temp[,happ+10]>0]))  
}
mapping2(4)
coda=temp[,5]
codathreshold=2
coverage=function(x){
  happ=x
  score=0
  for (member in 1:length(temp[,6])){
    if (temp[member,happ+10]>0){
      if (coda[member]>codathreshold){
        score=score+1
      }
    }
  }
  return(score)
}
familycoverage=function(x){
  happ=x
  families=c()
  score=0
  for (member in 1:length(temp[,6])){
    if (temp[member,happ+10]>0){
      if (coda[member]>codathreshold){
        if(!temp[member,7] %in% families){
          score=score+1
          families=append(families,temp[member,7])
        }
      }
    }
  }
  return(score)
}
number=2
mapping2(number)
coverage(number)
familycoverage(number)
haps[5]
extra ones to add for haplogroup V
points(temp[25:27,3],temp[25:27,2],pch=19,col='blue')
tone
[1] "B4g"  "B5a"  "M7b1" "N10"  "R11"  "U"    "B5"   "F1"   "R9"  
[10] "H"    "HV"   "F"    "J"    "T"    "JT"   "B4a3" "B4f"  "F1a" 
[19] "F1a1" "F1b"  "F1c"  "M7b"  "D4d"  "D5a"  "A6"   "B4e" 
F1a1
codas
[1] "B4a1a1"   "B5a"      "M7b1"     "Q"        "R9b"      "U"       
[7] "W"        "B4"       "B5"       "F1"       "M7"       "R9"      
[13] "H"        "HV"       "HV0a"     "HV0a"     "T1"       "N2"      
[19] "U5"       "U8"       "K"        "B"        "F"        "J"       
[25] "T"        "H1"       "H3"       "H5"       "H8"       "J1b"     
[31] "T2"       "U5a"      "U5a1"     "U5b"      "U5b1"     "JT"      
[37] "B4a"      "B4a1"     "F1a"      "F1a1"     "M7b"      "N"       
[43] "HV0"      "Q1"       "B4a1a1"   "B4a1a1a"  "B4a1a1a1" "U8b"
n=0
n=n+1
n=4
mapping(results[n])
threshold=0.01
mapping('F2')
temp[,1][temp[match(results[n],haps)+10]>0]
temp[,1][temp[match('F1a1',haps)+10]>0]
U5 and U8b and K which is a subclade of U8b, HV0 and H1 with expanded sample, and T1 
Vframe[,1:3]
V=data[,match('V',haps)+2]
MAV[25]=0
V[26]=0.075
V[26]=0.014
V[26]=0.032
V[27]=0.0824
summary(lm(V~phonotactics[,5]))
Vframe=data.frame(freq=V,family=temp[,7],ling=phonotactics[,5])
s=lmer(Vframe$ling~Vframe$freq+(1|Vframe$family))
t=lmer(Vframe$ling~(1|Vframe$family))
anova(s,t)
plot(V~phonotactics[,5],ylab='Haplogroup V frequency',xlab='Number of consonants in the coda')
temp[26,]
data[27,1]
data[,1]
Va=c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10)
b=c(2, 2, 6, 6, 10, 10, 1, 1, 2, 2, 2, 2, 3, 3, 1, 1, 1, 1, 1, 1, 3, 3, 8, 8, 8, 8, 1, 1, 7, 7, 8, 8, 2, 2, 4, 4, 4, 4, 4, 4, 9, 9, 1, 1, 1, 1, 3, 3, 7, 7, 8, 8, 9, 9, 4, 4, 6, 6, 8, 8, 8, 8, 2, 2, 3, 3, 10, 10, 10, 10, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 5, 5, 8, 8, 8, 8, 9, 9, 10, 10, 10, 10, 3, 3, 6, 6, 10, 10)
summary(lm(a~b))


binarise=function(x){
  output=c()
  for (member in x){
    if (member>1){
      output=append(output,c(1))
    }
    if (member<2){
      output=append(output,c(0))
    }
       
  }
  return(output) 
}

binarise(c(1,2,5,6))
threshold1=1
threshold2=3
mapdistance=function(a){
  map("worldHires",xlim=c(-10,180), ylim=c(-40,80))
  points(temp[,3],temp[,2],pch=19,col='cyan')
  vector=c()
  for (member in phonotacticsgenedata[,1]){
    vector=append(vector,geneticdistance2(member,a))
  }
  distframe=data.frame(vector,phonotactics[,2],phonotactics[,3])

  points(distframe[,3],distframe[,2],pch=19,col='cyan')
  points(distframe[,3][distframe[,1]<threshold2],distframe[,2][distframe[,1]<threshold2],pch=19,col='blue')
  points(distframe[,3][distframe[,1]<threshold1],distframe[,2][distframe[,1]<threshold1],pch=19,col='red')
  
}
mapdistance('Scandinavia')
coda=phonotactics[,5]
correlatedistance=function(a){
  vector=c()
  for (member in phonotacticsgenedata[,1]){
    vector=append(vector,geneticdistance2(member,a))
  }
  s=lm(vector~coda)
  return(lmp(s))
}

results=c()
for(member in data[,1]){
  results=append(results,correlatedistance(member))
  
}
names(results) = data[,1]
number=5
data[match(sort(results)[number],results),1]
mapdistance(data[match(sort(results)[number],results),1])
sort(results)
x = sapply(data[,1],correlatedistance)


help(cm.colors)
pcolors=heat.colors(10)[as.numeric(cut(results,10))]
pcolors=heat.colors(5)[as.numeric(cut(results,quantile(sean,seq(0,1,length.out=4))))]
sean=sapply(data[,1],geneticdistance2,b='Hmong')
tom=1-phonotacticsgenedata[,match('HV',haps)+2]
tom=1-Vframe$freq
tom=0-Vframe$ling

pcolors=heat.colors(3)[as.numeric(cut(tom,3))]
pcolors=heat.colors(5)[as.numeric(cut(results,quantile(tom,seq(0,1,length.out=5))))]


help(sapply)
map()
map("worldHires",xlim=c(-20,180), ylim=c(-40,80),bg='grey')
points(phonotactics[,3],phonotactics[,2],col=pcolors,pch=16)
points(c(dataextendedhv[8:13,3],extendedhv[8:13,2], col=pcolors,pch=16)
sean

giveage=function(x){
  return(haplogroupages[match(x,haps)])
}
giveage2=function(x){
  return(haplogroupages[x])
}
giveage('B4c2')
coda=phonotactics[,6]
HAP='U8b'
mapping(HAP)
giveage(HAP)
number=0
number=number+1
mapping2(number)
giveage2(number)

mapping2(1)
haplogroupages=read.csv("~/Documents/Gene data/Complete/haplogroupages.txt",header=F,stringsAsFactors=F)
haplogroupages=haplogroupages$V1
temp[,263]=Vframe$freq
findminimum=function(series){
  newseries=c()
  for(member in series){
    if(!member=='-'&!is.na(member)&member>0){
      newseries=append(newseries,as.numeric(member))
    }
  }
  return(min(newseries))
}
findmaximum=function(series){
  newseries=c()
  for(member in series){
    if(is.numeric(member)){
      newseries=append(newseries,as.numeric(member))
    }
  }
  return(max(newseries))
}
coverages=c()
familycoverages=c()
for (member in 1:253){
  coverages=append(coverages,coverage(member))
  familycoverages=append(familycoverages,familycoverage(member))
  
}
coveragetable=data.frame(haplogroupages,coverages,familycoverages,stringsAsFactors=F)
findminimum(coveragetable[,1][familycoverages>2])
toneplotfamily1=c(4333.9,5381.6,7047.2,7109.9,10863.5,10863.5,17520.4,23912.2,24208.8)
toneplotlanguages1=c(4333.9,5381.6,5381.6,7109.9,7655.8,10863.5,10863.5,10863.5,23912.2,23912.2)
toneplotlanguages2=c(5381.6,5381.6,7655.8,7655.8,10863.5,10863.5,23912.2)
toneplotfamily2=c(5381.6,7109.9,7655.8,10863.5)
toneplotlanguages2=c(5381.6,5381.6,7655.8,7655.8,10863.5,10863.5)
codaplotfamily1=c(1268.2,1268.2,9739.5,9739.5,9739.5,12846,12846)
codaplotlanguages1=c(1268.2,1268.2,9739.5,9739.5,9739.5,9739.5,9739.5,9739.5,9739.5,9739.5,9739.5,9739.5,9739.5,9739.5,12846)
codaplotlanguages2=c(2699.5,8341,8341,8341,8341,8341,9739.5,9739.5,9739.5,12846,12846,12846,12846)
codaplotfamily2=c(2699.5,9739.5,12846,21905.8)
plot(toneplotfamily)
coda=temp[,5]
codathreshold=1
haplogroupages[14]
haplogroupages=haplogroupages$V1
ageandcoverage=c()
for (member in 1:)
match('F1a1a',haps)

pairs of haplogroups
paircoverage=function(x,y){
  languages=c()
  score=0
  for (member in 1:length(temp[,6])){
    if (temp[member,x+10]>0){
      if (coda[member]>codathreshold){
        if (!temp[member,1] %in% languages){
          score=score+1
          languages=append(languages,temp[member,1])
        }
        
      }
    }

  }
  for (member in 1:length(temp[,6])){
    if (temp[member,y+10]>0){
      if (coda[member]>codathreshold){
        if (!temp[member,1] %in% languages){
          score=score+1
          languages=append(languages,temp[member,1])
        }
        
      }
    }
    
  }
  return(score)
}
pairfamilycoverage=function(x,y){
  families=c()
  score=0
  for (member in 1:length(temp[,6])){
    if (temp[member,x+10]>0){
      if (coda[member]>codathreshold){
        if(!temp[member,7] %in% families){
          score=score+1
          families=append(families,temp[member,7])
        }
      }
    }
  }
  for (member in 1:length(temp[,6])){
    if (temp[member,y+10]>0){
      if (coda[member]>codathreshold){
        if(!temp[member,7] %in% families){
          score=score+1
          families=append(families,temp[member,7])
        }
      }
    }
  }
  return(score)
}
paircoverages=c()
pairfamilycoverages=c()
for (member in 1:252){
  for (member2 in 1:252){
    paircoverages=append(paircoverages,paircoverage(member,member2))
  }
}
for (member in 1:252){
  for (member2 in 1:252){
    pairfamilycoverages=append(pairfamilycoverages,pairfamilycoverage(member,member2))
  }
}
mapping('F1a1')
pairages=c()
for (member in 1:252){
  for (member2 in 1:252){
    pairages=append(pairages,findmaximum(c(haplogroupages$V1[member],haplogroupages$V1[member2])))
  }
}  
paircoveragetable=data.frame(pairages,paircoverages,pairfamilycoverages)
findminimum(paircoveragetable[,1][paircoveragetable[,2]>17])
coda=temp[,6]
codathreshold=1


findorigin=function(b){
  origin=b
  fromorigin=function(a){
    return(genedistance(a,origin))
  }
  x=sapply(c(1:74),fromorigin)
  return(summary(lm(x~coda))$adj.r.squared)
}
findorigin2=function(b){
  origin=b
  fromorigin=function(a){
    return(genedistance(a,origin))
  }
  x=sapply(c(1:74),fromorigin)
  summary(lm(x~coda))
}
findoriginmixed=function(b){
  origin=b
  x=sapply(c(1:74),fromorigin)
  s=lmer(x~coda+(1|phonotactics[,7]))
  t=lmer(x~(1|phonotactics[,7]))
  return(anova(s,t)[2,8])
}
origins=sapply(1:74,findorigin)
origin=35
x=sapply(c(1:74),fromorigin)
s=lmer(x~coda+(1|phonotactics[,7]))
t=lmer(x~(1|phonotactics[,7]))
anova(s,t)[2,8]
findoriginmixed(10)
origins=sapply(1:74,findorigin)
names(origins)=phonotactics[,1]


write(rep(2,252),'/Users/jercol/Documents/Gene data/gendisttest.txt',sep=" ",ncolumns=252)
write.table(phonotacticsgenedata[,c(3:254)],'/Users/jercol/Documents/Gene data/gendisttest.txt',sep=" ",append=T)
