install.packages('ape')
library(ape)
install.packages("ade4")
library(ade4)
install.packages("normalp")
library("normalp")
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
Phonotactics:
data=read.csv("~/Documents/Gene data/haplogroups11.txt",header=T,stringsAsFactors=F)
data2=data.frame(data[,1:2])
data2[,3]-data[,3]
for (member in names(data)[!names(data) %in% c('Population','Size')]){
  number=match(member,names(data))
  descendants=c()
  for (member2 in names(data)[!names(data) %in% c('Population','Size')]){
    if (isancestor(newtree,member,member2)==1){
      descendants=append(descendants,member2)
    }
  }
  a=data[,match(member,names(data))]
  for (member3 in 1:length(a)){
    for (member4 in descendants){
      a[member3]=a[member3]+data[member3,match(member4,names(data))]                 
    }      
  }
  data2[,number]=a
}
} 
}
data=data2
phonotacticsgenedata=data
data=phonotacticsgenedata
phonotactics=read.csv("~/Documents/Gene data/Linguistic data/phonotactics.txt",header=F,sep='\t',stringsAsFactors=F)
phonotactics[,7]
data[africa,83]
haps[81]
africa=c(25:28,56,57,73)
summary(lm(phonotactics[africa,4]~data[africa,83]))
extendedh1=data.frame(append(phonotactics[africa,1],c('Fula','Median in Senegal','Median in Chad','Median in Cameroon','Buduma','Median in Nigeria','Amharic','Oromo','Median in Sierra Leone')),append(phonotactics[africa,2],c()),append(phonotactics[africa,3],c()),append(phonotactics[africa,4],c(1,1,1,2,2,1,1,2,1,1.5)),append(phonotactics[africa,5],c(1,1,1,1,1,1,1,2,1,1)),append(phonotactics[africa,6],c(2,2,3,4,0,3,0,0,2,4)),append(data[africa,83],c(0,0,0,0,0,0,0,0,0,0)),append(data[africa,2],c(186,100,142,77,30,69,82,90,117,155)))
extendedh=data.frame(append(phonotactics[africa,1],c('Median of languages in Egypt','Median of languages in Ethiopia','Median of languages in Kenya','Median of languages in Burkina-Faso','Median of languages in Mali','Median of languages in Niger')),append(phonotactics[africa,2],c(30,9,1.3,12.2,12.4,13.5)),append(phonotactics[africa,3],c(30,38,37,1.32,8,2.1)),append(phonotactics[africa,4],c(1,1,2,1,2,1)),append(phonotactics[africa,5],c(2,1,1,1,1,1)),append(phonotactics[africa,6],c(0,0,2.5,3,3,6)),freq=append(data[africa,82],c(0,0.009,0,0.2,0.524,0.04)),append(data[africa,2],c(278,232,84,40,21,25)))
extendedhv=data.frame(append(phonotactics[africa,1],c('Median of languages in Egypt','Median of languages in Ethiopia','Median of languages in Kenya','Median of languages in Burkina-Faso','Median of languages in Mali','Median of languages in Niger')),append(phonotactics[africa,2],c(30,9,1.3,12.2,12.4,13.5)),append(phonotactics[africa,3],c(30,38,37,1.32,8,2.1)),append(phonotactics[africa,4],c(1,1,2,1,2,1)),append(phonotactics[africa,5],c(2,1,1,1,1,1)),append(phonotactics[africa,6],c(0,0,2.5,3,3,6)),freq=append(data[africa,83],c(0.061,0.026,0,0.2,0,0.12)),append(data[africa,2],c(278,232,84,40,21,25)))
extendedhv2=data.frame(append(phonotactics[africa,1],c('Median of languages in Egypt','Median of languages in Ethiopia','Median of languages in Kenya','Median of languages in Burkina-Faso','Median of languages in Mali','Median of languages in Niger')),append(phonotactics[africa,2],c(30,9,1.3,12.2,12.4,13.5)),append(phonotactics[africa,3],c(30,38,37,1.32,8,2.1)),append(phonotactics[africa,4],c(1,1,2,1,2,1)),append(phonotactics[africa,5],c(2,1,1,1,1,1)),append(phonotactics[africa,6],c(0,0,2.5,3,3,6)),extendedh[,7]+extendedhv[,7],freq=append(data[africa,2],c(278,232,84,40,21,25)))
extendedu=data.frame(append(phonotactics[africa,1],c('Median of languages in Egypt','Median of languages in Ethiopia','Median of languages in Kenya','Median of languages in Burkina-Faso','Median of languages in Mali','Median of languages in Niger')),append(phonotactics[africa,2],c(30,9,1.3,12.2,12.4,13.5)),append(phonotactics[africa,3],c(30,38,37,1.32,8,2.1)),append(phonotactics[africa,4],c(1,1,2,1,2,1)),append(phonotactics[africa,5],c(2,1,1,1,1,1)),append(phonotactics[africa,6],c(0,0,2.5,3,3,6)),freq=append(data[africa,53],c(0.137,0.065,0.0012,0,0,0.04)),append(data[africa,2],c(278,232,84,40,21,25)))

extendedhv[,c(1,7)]
summary(lm(extendedhv[,7]~extendedhv2[,5]))

a=c(2,3,3,2,1,1,1,2,1,1,1,1,1)
b=c()
coda=phonotactics[,5]
coda=sample(coda,73,replace=F)
pvalues=c()
for (member in 3:254){
  s=lm(data[,member]~coda)
  pvalues=append(pvalues,lmp(s))  
}
pvalues=pvalues[!is.na(pvalues)]
length(pvalues[pvalues<0.05])
length(pvalues[pvalues<0.35 & pvalues>0.3])
hist(pvalues,breaks=20,xlab='P-values',main='')

pvalues=c()
for (member in 3:254){
  for (member2 in 4:6){
    s=lm(data[,member]~phonotactics[,member2])
    pvalues=append(pvalues,lmp(s))  
  }
  
}
pvalues=pvalues[!is.na(pvalues)]
summary(lm(phonotactics[,5]~data[,53]))
plot(data[,82],phonotactics[,6])
coda=phonotactics[,5]
hist(pvalues,breaks=20)
pvalues[!is.na(pvalues)]
install.packages('lme4')
library(lme4)
s=lmer(coda~data[,85]+(1|phonotactics[,7]))
t=lmer(coda~(1|phonotactics[,7]))
anova(s,t)
anova(s,t)[2,8]
match('H',haps)
HV=83,H1=108,H3=109,H4=110,H5=111,H8=112,H1a=226,V=85,H=82
coda=phonotactics[,4]
pvalues=c()
for (member in 3:254){
  s=lmer(coda~data[,member]+(1|phonotactics[,7]))
  t=lmer(coda~(1|phonotactics[,7]))
  pvalues=append(pvalues,anova(s,t)[2,8])  
}
pvalues=pvalues[!is.na(pvalues)]
hist(pvalues,breaks=20,xlab='P-values',main='')
length(pvalues[pvalues<0.05])
length(pvalues[pvalues<0.8 & pvalues>0.75])
phonotactics[africa,1]
results=c()
for (pvalue in pvalues[!is.na(pvalues)]){
  if (pvalue<0.0000001){
    results=append(results,haps[match(pvalue,pvalues)])
  }
  
}
results
match('M13a',haps)
data[,18]
hist(pvalues,breaks=100)
control=runif(69,0,1)
pvalues=c()
for (member in 3:254){
  s=lm(data[,member]~control)
  pvalues=append(pvalues,lmp(s))  
}
pvalues
pvalues[!is.na(pvalues)]
hist(pvalues,breaks=20)

sampling
table=data.frame(syllable=phonotactics[,6],haplogroup=data[,match('B4c2',haps)+2],family=phonotactics[,7])
table$haplogroup=Vframe$freq
permuteresults=c()
for (n in 1:100)
{
  selectgenes=c()
  selectcoda=c()
  for (member in levels(table$family))
  {
    
    choice=sample(1:length(table$haplogroup[table$family==member]),1)
    selectgenes=append(selectgenes,table$haplogroup[table$family==member][choice])
    selectcoda=append(selectcoda,table$syllable[table$family==member][choice])
  }

  s=lm(selectcoda~selectgenes)
  if(sum(selectgenes)>0){
    permuteresults=append(permuteresults,lmp(s))
  }
    
}  
hist(permuteresults,breaks=20)
length(permuteresults[permuteresults<0.05])

mantel test
install.packages('geosphere')
library(geosphere)

distHaversine(c(0,5),c(5,5))
site.x=c()
site.y=c()
Distance=c()
for (member in 1:length(phonotactics[,1])){
  site.x=append(site.x,rep(phonotactics[member,1],length(phonotactics[,1])-member))
  site.y=append(site.y,phonotactics[member+1:length(phonotactics[,1]),1])
}
site.y=site.y[!is.na(site.y)]
Distance=c()
for (member in 1:length(site.x)){
  Distance=append(Distance,distHaversine(phonotactics[match(site.x[member],phonotactics[,1]),c(3,2)],phonotactics[match(site.y[member],phonotactics[,1]),c(3,2)]))
}
df=data.frame(site.x,site.y,Distance)
nams <- with(df, unique(c(as.character(site.x), as.character(site.y))))
distances <- with(df, Distance)
attributes(distances) <- with(df, list(Size = length(nams),
                                  Labels = nams,
                                  Diag = FALSE,
                                  Upper = FALSE,
                                  method = "user"))
class(distances) <- "dist"
site.y=site.y[!is.na(site.y)]
codaDistance=c()
number=6
for (member in 1:length(site.x)){
  codaDistance=append(codaDistance,phonotactics[match(site.x[member],phonotactics[,1]),number]-phonotactics[match(site.y[member],phonotactics[,1]),number])
}
codaDistance=abs(codaDistance)
df=data.frame(site.x,site.y,codaDistance)
codadistances <- with(df, codaDistance)
nams <- with(df, unique(c(as.character(site.x), as.character(site.y))))
attributes(codadistances) <- with(df, list(Size = length(nams),
                                       Labels = nams,
                                       Diag = FALSE,
                                       Upper = FALSE,
                                       method = "user"))
class(codadistances) <- "dist"
geneDistance=c()
number2=match('HV',haps)+2
data=phonotacticsgenedata
for (member in 1:length(site.x)){
  geneDistance=append(geneDistance,abs(data[match(site.x[member],phonotactics[,1]),number2]-data[match(site.y[member],phonotactics[,1]),number2]))
}
delete the next bit if you do not want to use all haplogroups
geneDistance=c()
number2=83
data=phonotacticsgenedata

geneticdistance(57,56)
geneticdistance=function(a,b){
  return(sum(abs(data[a,3:254]-data[b,3:254]))) 
}
geneticdistance2=function(a,b){
  return(sum(abs(data[match(a,data[,1]),3:254]-data[match(b,data[,1]),3:254]))) 
}

for (member in 1:length(site.x)){
  geneDistance=append(geneDistance,sum(abs(data[match(site.x[member],phonotactics[,1]),3:254]-data[match(site.y[member],phonotactics[,1]),3:254])))
}
df=data.frame(site.x,site.y,geneDistance)
genedistances <- with(df, geneDistance)
nams <- with(df, unique(c(as.character(site.x), as.character(site.y))))
attributes(genedistances) <- with(df, list(Size = length(nams),
                                           Labels = nams,
                                           Diag = FALSE,
                                           Upper = FALSE,
                                           method = "user"))
class(genedistances) <- "dist"
familyDistance=c()
for (member in 1:length(site.x)){
  if(phonotactics[match(site.x[member],phonotactics[,1]),7]==phonotactics[match(site.y[member],phonotactics[,1]),7]){
    familyDistance=append(familyDistance,0)
  }
  else if(!phonotactics[match(site.x[member],phonotactics[,1]),7]==phonotactics[match(site.y[member],phonotactics[,1]),7]){
    familyDistance=append(familyDistance,1)
  }
}
familyDistance=abs(familyDistance)
df=data.frame(site.x,site.y,familyDistance)
familydistances <- with(df, familyDistance)
nams <- with(df, unique(c(as.character(site.x), as.character(site.y))))
attributes(familydistances) <- with(df, list(Size = length(nams),
                                           Labels = nams,
                                           Diag = FALSE,
                                           Upper = FALSE,
                                           method = "user"))
class(familydistances) <- "dist"





computedistance=function(x){
  for (member in 1:length(x)){
    site.x=append(site.x,rep(x[member],length(x)-member))
    site.y=append(site.y,x[member+1:length(x)])
    site.y=site.y[!is.na(site.y)]
  }  
  distances=c()
  for (member in 1:length(site.x)){
    if(x[match(site.x[member],x)]==x[match(site.y[member],x)]){
      familyDistance=append(familyDistance,1)
    }
    else if(!x[match(site.x[member],x)]==x[match(site.y[member],x[,1])]){
      familyDistance=append(familyDistance,0)
    }
  }
  distances=abs(distances)
  df=data.frame(x,y,distances)
  distances2=with(df,distances)
  nams <- with(df, unique(c(as.character(x), as.character(y))))
  attributes(distances2) <- with(df, list(Size = length(nams),
                                               Labels = nams,
                                               Diag = FALSE,
                                               Upper = FALSE,
                                               method = "user"))
  class(distances2) <- "dist"
  return(distances2)
}
vector=c('dog','cat','dog')
computedistance(vector)

mantel.rtest(genedistances,codadistances)
mantel(codadistances~distance)
mantel.rtest(codadistances,jez2)
install.packages('ecodist')
library(ecodist)

Phonemes:
phonemes=read.csv("~/Documents/Gene data/Linguistic data/phonemedata.txt",sep='\t',header=F,stringsAsFactors=F)
phonemeswithfamilies=read.csv("~/Documents/Gene data/Linguistic data/phonemedatawithfamilies.txt",sep='\t',header=T,stringsAsFactors=F,row.names=NULL)
data=read.csv("~/Documents/Gene data/haplogroups12.txt",header=T,stringsAsFactors=F)
data2=data.frame(data[,1:2])
data2[,3]-data[,3]
for (member in names(data)[!names(data) %in% c('Population','Size')]){
  number=match(member,names(data))
  descendants=c()
  for (member2 in names(data)[!names(data) %in% c('Population','Size')]){
    if (isancestor(newtree,member,member2)==1){
      descendants=append(descendants,member2)
    }
  }
  a=data[,match(member,names(data))]
  for (member3 in 1:length(a)){
    for (member4 in descendants){
      a[member3]=a[member3]+data[member3,match(member4,names(data))]                 
    }      
  }
  data2[,number]=a
}
} 
}
data=data2
phonemesgenedata=data
data=phonemesgenedata
table=phonemes
significantresults=c()
table[1,]
phonotactics[,1][phonotactics[,7]=='Indo-European']
pvalues=c()
for (member in 3:254){
  for (member2 in 2:1638){
    s=glm(table[,member2]~data[,member])
    if (length(!data[,member][!data[,member]==0])>7){
      if (length(table[,member2][table[,member2]==1])>7){
        if (length(table[,member2][table[,member2]==1])<71){
          pvalues=append(pvalues,summary(s)$coefficients[8])
          significantresults=append(significantresults,c(haps[member],member2-2))
        }         
      }      
    }    
  }  
}
hist(pvalues[!is.na(pvalues)],breaks=20,xlab='P-values',main='')
length(pvalues[pvalues<0.05])
length(pvalues[pvalues<0.35 & pvalues>0.3])


significantresults=c()
pvalues=c()
for (member2 in 2:1638){
  s=glm(table[,member2]~data[,83])
  if (length(!data[,member][!data[,member]==0])>7){
    if (length(table[,member2][table[,member2]==1])>7){
      if (length(table[,member2][table[,member2]==1])<71){
        pvalues=append(pvalues,summary(s)$coefficients[8])
        if (summary(s)$coefficients[8]<0.0025){
          significantresults=append(significantresults,member2-1)
        }
      }   
    }   
  }    
}
table[,c(1,2)]
hist(pvalues[!is.na(pvalues)],breaks=20)
significantresults
table[,c(1,27)]
table[,2]
---
  
word order 
data=read.csv("~/Documents/Gene data/haplogroups13.txt",header=T,stringsAsFactors=F)
data2=data.frame(data[,1:2])
data2[,3]-data[,3]
for (member in names(data)[!names(data) %in% c('Population','Size')]){
  number=match(member,names(data))
  descendants=c()
  for (member2 in names(data)[!names(data) %in% c('Population','Size')]){
    if (isancestor(newtree,member,member2)==1){
      descendants=append(descendants,member2)
    }
  }
  a=data[,match(member,names(data))]
  for (member3 in 1:length(a)){
    for (member4 in descendants){
      a[member3]=a[member3]+data[member3,match(member4,names(data))]                 
    }      
  }
  data2[,number]=a
}
} 
}
data=data2
wordordergenedata=data
data=wordordergenedata
table=read.csv("~/Documents/Word order/data2.txt",sep='\t',header=F,stringsAsFactors=F)
family=table[,12]
table2=table[,1:4]
values=c(0,1,2)
n=5
for (value in values){
  for (member in 5:11){
    a=c(rep(0,59))
    for (member2 in 1:59){
      if (table[member2,member]==value){
        a[member2]=1
      }
    }
    table2[,n]=a
    n=n+1
  } 
}
table=table2
wordorderdata=table
wordorderdata$family=family
table=wordorderdata
s=glm(wordorderdata[,17]~wordordergenedata[,83])
summary(s)$coefficients[8]
s=glm(extendedwordorder[,6]~extendedwordorder[,8])
summary(s)$coefficients[8]
extendedwordorder[,7]
extendedwordorder=data.frame(append(wordorderdata[wordorderafrica,1],c('Median of languages in Egypt','Median of languages in Ethiopia','Median of languages in Kenya','Median of languages in Burkina-Faso','Median of languages in Mali','Median of languages in Niger')),append(wordorderdata[wordorderafrica,3],c(30,9,1.3,12.2,12.4,13.5)),append(wordorderdata[wordorderafrica,4],c(30,38,37,1.32,8,2.1)),append(wordorderdata[wordorderafrica,10],c(0,0,0,0,0,0)),append(wordorderdata[wordorderafrica,82],c(0,0.009,0,0.2,0.524,0.04)),append(wordorderdata[wordorderafrica,83],c(0.061,0.026,0,0.2,0,0.12)),append(wordordergenedata[wordorderafrica,83],extendedhv2$freq[8:13]),append(wordorderdata[wordorderafrica,53],c(0,0.009,0,0.2,0.524,0.04)),append(wordordergenedata[wordorderafrica,2],c(278,232,84,40,21,25)))
africanwordorder=data.frame(extendedwordorder[,c(1,2,3,4)],c(0,0,1,1,1,1,0,1,1,1,1,1),extendedwordorder[,c(7,9)])
s=glm(africanwordorder[,4]~africanwordorder[,6])
summary(s)$coefficients[8]
extendedh=data.frame(append(phonotactics[africa,1],c('Median of languages in Egypt','Median of languages in Ethiopia','Median of languages in Kenya','Median of languages in Burkina-Faso','Median of languages in Mali','Median of languages in Niger')),append(phonotactics[africa,2],c(30,9,1.3,12.2,12.4,13.5)),append(phonotactics[africa,3],c(30,38,37,1.32,8,2.1)),append(phonotactics[africa,4],c(1,1,2,1,2,1)),append(phonotactics[africa,5],c(2,1,1,1,1,1)),append(phonotactics[africa,6],c(0,0,2.5,3,3,6)),freq=append(data[africa,82],c(0,0.009,0,0.2,0.524,0.04)),append(data[africa,2],c(278,232,84,40,21,25)))
extendedhv=data.frame(append(phonotactics[africa,1],c('Median of languages in Egypt','Median of languages in Ethiopia','Median of languages in Kenya','Median of languages in Burkina-Faso','Median of languages in Mali','Median of languages in Niger')),append(phonotactics[africa,2],c(30,9,1.3,12.2,12.4,13.5)),append(phonotactics[africa,3],c(30,38,37,1.32,8,2.1)),append(phonotactics[africa,4],c(1,1,2,1,2,1)),append(phonotactics[africa,5],c(2,1,1,1,1,1)),append(phonotactics[africa,6],c(0,0,2.5,3,3,6)),freq=append(data[africa,83],c(0.061,0.026,0,0.2,0,0.12)),append(data[africa,2],c(278,232,84,40,21,25)))
extendedhv2=data.frame(append(phonotactics[africa,1],c('Median of languages in Egypt','Median of languages in Ethiopia','Median of languages in Kenya','Median of languages in Burkina-Faso','Median of languages in Mali','Median of languages in Niger')),append(phonotactics[africa,2],c(30,9,1.3,12.2,12.4,13.5)),append(phonotactics[africa,3],c(30,38,37,1.32,8,2.1)),append(phonotactics[africa,4],c(1,1,2,1,2,1)),append(phonotactics[africa,5],c(2,1,1,1,1,1)),append(phonotactics[africa,6],c(0,0,2.5,3,3,6)),freq=extendedh[,7]+extendedhv[,7],append(data[africa,2],c(278,232,84,40,21,25)))
extendedu=data.frame(append(phonotactics[africa,1],c('Median of languages in Egypt','Median of languages in Ethiopia','Median of languages in Kenya','Median of languages in Burkina-Faso','Median of languages in Mali','Median of languages in Niger')),append(phonotactics[africa,2],c(30,9,1.3,12.2,12.4,13.5)),append(phonotactics[africa,3],c(30,38,37,1.32,8,2.1)),append(phonotactics[africa,4],c(1,1,2,1,2,1)),append(phonotactics[africa,5],c(2,1,1,1,1,1)),append(phonotactics[africa,6],c(0,0,2.5,3,3,6)),freq=append(data[africa,53],c(0.137,0.065,0.0012,0,0,0.04)),append(data[africa,2],c(278,232,84,40,21,25)))
extendedhv2$freq
s=glmer(wordorderdata[,17]~wordordergenedata[,83]+(1|wordorderdata[,26]))
t=glmer(wordorderdata[,17]~(1|wordorderdata[,26]))
anova(s,t)
pvalues=c()
for (member in 3:254){
  for (member2 in 5:25){
    s=glm(table[,member2]~data[,member])
    pvalues=append(pvalues,summary(s)$coefficients[8])
    
  }
  
}
pvalues=pvalues[!is.na(pvalues)]
hist(pvalues[!is.na(pvalues)],breaks=20,xlab='P-values',main='')
length(pvalues[pvalues<0.05])
length(pvalues[pvalues<0.7 & pvalues>0.65])

wordordergenedata[,c(1,83)]
wordorderafrica=c(25,26,45,46,47,54)
table[54,17]
wordordergenedata[wordorderafrica,83]
wordordergenedata[wordorderafrica,1]
table[wordorderafrica,17]
summary(lm(table[africa,17]~wordordergenedata[wordorderafrica,83]))
pvalues=c()
for (member in 5:18){
  s=lm(table[,member]~data[,183])
  pvalues=append(pvalues,summary(s)$coefficients[8])  
}

hist(pvalues,breaks=20)
pvalues

numbers=c(17)
pvalues=c()
for (member in 3:254){
  for (member2 in numbers){
    s=glm(table[,member2]~data[,member])
    pvalues=append(pvalues,summary(s)$coefficients[8])
    
  }
  
}

data=read.csv("~/Documents/Gene data/haplogroups12.txt",header=T,stringsAsFactors=F)
phonemedata=read.csv("~/Documents/Gene data/phonemedata.txt",sep='\t',header=F)
s=glm(table[,2]~data$H)
summary(s)$coefficients[8]
length(table[1,])

install.packages("mapdata")
library("mapdata")
map("worldHires",xlim=c(-30,180), ylim=c(-40,80))
points(phonotactics[,3],phonotactics[,2],pch=19,col='cyan')
points(phonotactics[,3][phonotactics[,5]>1],phonotactics[,2][phonotactics[,5]>1],pch=19,col='blue')
points(phonotactics[,3][phonotactics[,7]=="Indo-European"],phonotactics[,2][phonotactics[,7]=="Indo-European"],pch=19,col='red')

map("worldHires",xlim=c(50,180), ylim=c(-40,80))
points(phonotactics[,3],phonotactics[,2],pch=19,col='cyan')
points(phonotactics[,3][phonotactics[,6]>0],phonotactics[,2][phonotactics[,6]>0],pch=19,col='blue')
points(phonotactics[,3][phonotactics[,6]>4],phonotactics[,2][phonotactics[,6]>4],pch=19,col='red')

summary(lm(phonotacticsphonotactics[genedata[,c(1,83)]
phonotactics[,4]


table[,c(1,10)]
for (pvalue in pvalues[!is.na(pvalues)]){
  if (pvalue<(0.05/100)){
    results=append(results,match(pvalue,pvalues))
  }
  
}
results
mantel.rtest(jez3,dist(data$H))
LangDistance

------------
  
phonotactics=read.csv('~/Documents/Phonotactics/forgenepaper.txt',sep='\t',header=F)  
data=read.csv('~/Documents/Gene data/haplogroups11.txt',sep=',',header=T)
data$H
s=lm(phonotactics[,3]~data$H)
summary(s)

pvalues=c()
for (member in 3:254){
  s=lm(phonotactics[,3]~data[,member])
  if (length(summary(s)$coefficients[,4])==2){
    pvalues=append(pvalues,summary(s)$coefficients[2,4]) 
    
  }
  else{
    pvalues=append(pvalues,1.5)
  }
}
hist(pvalues[!pvalues==1.5],breaks=20)
results=c()
for (pvalue in pvalues){
  if (pvalue<(0.00001)){
    results=append(results,haps[match(pvalue,pvalues)])
  }
  
}
results
match("L0",c(newtree$tip.label,newtree$node.label))

newtree$edge[1,]
data$F3
phonotactics[21,1]
newtree$edge
newtree$edge[which(newtree$edge[,2]==3589),1]

isancestor=function(tree,node1,node2){
  a=match(node1,c(tree$tip.label,tree$node.label))
  b=match(node2,c(tree$tip.label,tree$node.label))
  d=c()
  mother=b
  while (!mother==3588){
    d=append(d,tree$edge[which(tree$edge[,2]==mother),1])
    mother=tree$edge[which(tree$edge[,2]==mother),1]
  }
  answer=ifelse(a %in% d,1,0)
  return(answer)
}
isancestor(newtree,'H1',"H1")
names(data)
data[,3]
data2=data.frame(data[,1:2])
data2[,3]-data[,3]
for (member in names(data)[!names(data) %in% c('Population','Size')]){
  number=match(member,names(data))
  descendants=c()
  for (member2 in names(data)[!names(data) %in% c('Population','Size')]){
    if (isancestor(newtree,member,member2)==1){
      descendants=append(descendants,member2)
    }
  }
  a=data[,match(member,names(data))]
  for (member3 in 1:length(a)){
      for (member4 in descendants){
        a[member3]=a[member3]+data[member3,match(member4,names(data))]                 
        }      
      }
  data2[,number]=a
    }
  } 
}
data2[,3]-data[,3]

install.packages("mapdata")
library("mapdata")

Phonotactics map:
map("worldHires",xlim=c(-30,180), ylim=c(-40,80))
points(phonotactics[,3],phonotactics[,2],pch=19,col='cyan')
points(phonotactics[,3][phonotactics[,5]>1],phonotactics[,2][phonotactics[,5]>1],pch=19,col='blue')
points(phonotactics[,3][phonotactics[,5]>2],phonotactics[,2][phonotactics[,5]>2],pch=19,col='blue')
points(phonotactics[,3][phonotactics[,7]=='Indo-European'],phonotactics[,2][phonotactics[,7]=='Indo-European'],pch=19,col='red')
phonotactics
Haplogroup HV map:
happ=match('',haps)
temp[,11]
temp=data.frame(phonotactics,phonotacticsgenedata)  
map("worldHires",xlim=c(-30,180), ylim=c(-40,80))
happ=3
haps[happ]
points(temp[,3],temp[,2],pch=19,col='cyan')
points(temp[,3][temp[,happ+10]>0],temp[,2][temp[,happ+10]>0],pch=19,col='blue')
points(temp[,3][temp[,happ+10]>0.3],temp[,2][temp[,happ+10]>0.3],pch=19,col='red')

points(temp[,3][temp[,7]=='Indo-European'],temp[,2][temp[,7]=='Indo-European'],pch=19,col='red')
HV=83,H1=108,H3=109,H4=110,H5=111,H8=112,H1a=226,V=85,H=82,HV0=174,HV0a=84
phonotacticsgenedata[,3]
Haplogroups in Africa
thing=extendedhv
map("worldHires",xlim=c(-30,180), ylim=c(-40,80))
points(thing[,3],thing[,2],pch=19,col='cyan')
points(thing[,3][thing[,7]>0],thing[,2][thing[,7]>0],pch=19,col='blue')
points(thing[,3][thing[,7]>0.3],thing[,2][thing[,7]>0.3],pch=19,col='red')

Syllable structures in Africa
extendedu
map("worldHires",xlim=c(-30,180), ylim=c(-40,80))
points(thing[,3],thing[,2],pch=19,col='cyan')
points(thing[,3][thing[,5]>1],thing[,2][thing[,5]>1],pch=19,col='blue')
points(thing[,3][thing[,5]>2],thing[,2][thing[,5]>2],pch=19,col=pcolors)
thing=extendedhv
tables I need
extendedhv[,9]
phonotactics and phonotacticsgenedata
extended african data
wordorderdata and wordordergenedata
phonemes and phonemesgenedata
towrite=data.frame(extendedwordorder[,c(1,2,3,4)],append(wordorderdata[wordorderafrica,9],c(0,1,1,1,1,1)),extendedwordorder[,c(7,9)])
towrite[,4]
africanwordorder$tom=tom
write.table(africanwordorder[,c(1:6,8,7)],'~/Documents/Gene data/Complete/bigtable.txt',sep='\t',append=T,row.names=F,col.names=F,quote=F)
towrite[1,26]
wordorderdata[wordorderafrica,27]
numeral-noun order:
map("worldHires",xlim=c(-30,180), ylim=c(-40,80))
points(wordorderdata[,4],wordorderdata[,3],pch=19,col='cyan')
points(wordorderdata[,4][wordorderdata[,10]==1],wordorderdata[,3][wordorderdata[,10]==1],pch=19,col='red')
points(wordorderdata[,4][wordorderdata[,24]==1],wordorderdata[,3][wordorderdata[,24]==1],pch=19,col='blue')
points(africanwordorder[,3],africanwordorder[,2],pch=19,col='cyan')
points(africanwordorder[,3][africanwordorder[,5]==0],africanwordorder[,2][africanwordorder[,5]==0],pch=19,col='blue')
points(africanwordorder[,3][africanwordorder[,4]==1],africanwordorder[,2][africanwordorder[,4]==1],pch=19,col='red')
africanwordorder[7,c(1,5)]
unique(phonotactics[,7])
pvalues=c()
for (member in 3:254){
  s=glm(wordordergenedata[,member]~wordorderdata[,17])
  pvalues=append(pvalues,summary(s)$coefficients[8])  
}
haps[match(min(pvalues[!is.na(pvalues)]),pvalues)-2]
wordorderdata[,5]

match('U6',haps)
summary(glm(wordorderdata[,17]~wordordergenedata[,match('U',haps)+2]))


lastone=data.frame(append(wordorderdata[,1],africanwordorder[7:12,1]),append(wordorderdata[,17],africanwordorder[7:12,5]),append(wordordergenedata[,53],extendedu[8:13,7]),append(wordorderdata[,26],c('Afro-Asiatic','Afro-Asiatic','Niger-Congo','Niger-Congo','Mande','Niger-Congo')))
summary(glm(lastone[,2]~lastone[,3]))
s=glmer(lastone[,2]~lastone[,3]+(1|lastone[,4]))
t=glmer(lastone[,2]~(1|lastone[,4]))
anova(s,t)
wordorderdata[,26]

write.table(lastone,'~/Documents/Gene data/Complete/bigtable.txt',sep='\t',append=T,row.names=F,col.names=F,quote=F)
africanwordorder
tom=append(wordordergenedata[wordorderafrica,53],extendedu[8:13,7])
tim=append(wordorderdata[wordorderafrica,17],africanwordorder[7:12,5])
summary(glm(tom~tim))

wordorderdata[,3]
wordordergenedata[,254]


word order distances
linguistic=wordorderdata
genetic=wordordergenedata
site.x=c()
site.y=c()
Distance=c()
for (member in 1:length(linguistic[,1])){
  site.x=append(site.x,rep(linguistic[member,1],length(linguistic[,1])-member))
  site.y=append(site.y,linguistic[member+1:length(linguistic[,1]),1])
}
site.y=site.y[!is.na(site.y)]
for (member in 1:length(site.x)){
  Distance=append(Distance,distHaversine(linguistic[match(site.x[member],linguistic[,1]),c(4,3)],linguistic[match(site.y[member],linguistic[,1]),c(4,3)]))
}
df=data.frame(site.x,site.y,Distance)
nams <- with(df, unique(c(as.character(site.x), as.character(site.y))))
distances <- with(df, Distance)
attributes(distances) <- with(df, list(Size = length(nams),
                                       Labels = nams,
                                       Diag = FALSE,
                                       Upper = FALSE,
                                       method = "user"))
class(distances) <- "dist"
site.y=site.y[!is.na(site.y)]
codaDistance=c()
for (member in 1:length(site.x)){
  codaDistance=append(codaDistance,sum(abs(linguistic[match(site.x[member],linguistic[,1]),5:11]-linguistic[match(site.y[member],linguistic[,1]),5:11])))
}
df=data.frame(site.x,site.y,codaDistance)
codadistances <- with(df, codaDistance)
nams <- with(df, unique(c(as.character(site.x), as.character(site.y))))
attributes(codadistances) <- with(df, list(Size = length(nams),
                                           Labels = nams,
                                           Diag = FALSE,
                                           Upper = FALSE,
                                           method = "user"))
class(codadistances) <- "dist"
geneDistance=c()
data=genetic
for (member in 1:length(site.x)){
  geneDistance=append(geneDistance,sum(abs(data[match(site.x[member],linguistic[,1]),3:254]-data[match(site.y[member],linguistic[,1]),3:254])))
}
df=data.frame(site.x,site.y,geneDistance)
genedistances <- with(df, geneDistance)
nams <- with(df, unique(c(as.character(site.x), as.character(site.y))))
attributes(genedistances) <- with(df, list(Size = length(nams),
                                           Labels = nams,
                                           Diag = FALSE,
                                           Upper = FALSE,
                                           method = "user"))
class(genedistances) <- "dist"
familyDistance=c()
for (member in 1:length(site.x)){
  if(linguistic[match(site.x[member],linguistic[,1]),26]==linguistic[match(site.y[member],linguistic[,1]),26]){
    familyDistance=append(familyDistance,0)
  }
  else if(!linguistic[match(site.x[member],linguistic[,1]),26]==linguistic[match(site.y[member],linguistic[,1]),26]){
    familyDistance=append(familyDistance,1)
  }
}
familyDistance=abs(familyDistance)
df=data.frame(site.x,site.y,familyDistance)
familydistances <- with(df, familyDistance)
nams <- with(df, unique(c(as.character(site.x), as.character(site.y))))
attributes(familydistances) <- with(df, list(Size = length(nams),
                                             Labels = nams,
                                             Diag = FALSE,
                                             Upper = FALSE,
                                             method = "user"))
class(familydistances) <- "dist"

phonemes
linguistic=phonemeswithfamilies
genetic=phonemesgenedata2
site.x=c()
site.y=c()
Distance=c()
for (member in 1:length(linguistic[,1])){
  site.x=append(site.x,rep(linguistic[member,4],length(linguistic[,4])-member))
  site.y=append(site.y,linguistic[member+1:length(linguistic[,4]),4])
}
site.y=site.y[!is.na(site.y)]
for (member in 1:length(site.x)){
  Distance=append(Distance,distHaversine(linguistic[match(site.x[member],linguistic[,4]),c(2,1)],linguistic[match(site.y[member],linguistic[,4]),c(2,1)]))
}
df=data.frame(site.x,site.y,Distance)
nams <- with(df, unique(c(as.character(site.x), as.character(site.y))))
distances <- with(df, Distance)
attributes(distances) <- with(df, list(Size = length(nams),
                                       Labels = nams,
                                       Diag = FALSE,
                                       Upper = FALSE,
                                       method = "user"))
class(distances) <- "dist"
site.y=site.y[!is.na(site.y)]
codaDistance=c()
for (member in 1:length(site.x)){
  codaDistance=append(codaDistance,sum(abs(linguistic[match(site.x[member],linguistic[,4]),5:1641]-linguistic[match(site.y[member],linguistic[,4]),5:1641])))
}
df=data.frame(site.x,site.y,codaDistance)
codadistances <- with(df, codaDistance)
nams <- with(df, unique(c(as.character(site.x), as.character(site.y))))
attributes(codadistances) <- with(df, list(Size = length(nams),
                                           Labels = nams,
                                           Diag = FALSE,
                                           Upper = FALSE,
                                           method = "user"))
class(codadistances) <- "dist"
geneDistance=c()
data=genetic
for (member in 1:length(site.x)){
  geneDistance=append(geneDistance,sum(abs(data[match(site.x[member],linguistic[,4]),3:254]-data[match(site.y[member],linguistic[,4]),3:254])))
}
df=data.frame(site.x,site.y,geneDistance)
genedistances <- with(df, geneDistance)
nams <- with(df, unique(c(as.character(site.x), as.character(site.y))))
attributes(genedistances) <- with(df, list(Size = length(nams),
                                           Labels = nams,
                                           Diag = FALSE,
                                           Upper = FALSE,
                                           method = "user"))
class(genedistances) <- "dist"
familyDistance=c()
for (member in 1:length(site.x)){
  if(phonemeswithfamilies[match(site.x[member],phonemeswithfamilies[,4]),3]==phonemeswithfamilies[match(site.y[member],phonemeswithfamilies[,4]),3]){
    familyDistance=append(familyDistance,0)
  }
  else if(!linguistic[match(site.x[member],phonemeswithfamilies[,4]),3]==phonemeswithfamilies[match(site.y[member],phonemeswithfamilies[,4]),3]){
    familyDistance=append(familyDistance,1)
  }
}
familyDistance=abs(familyDistance)
df=data.frame(site.x,site.y,familyDistance)
familydistances <- with(df, familyDistance)
nams <- with(df, unique(c(as.character(site.x), as.character(site.y))))
attributes(familydistances) <- with(df, list(Size = length(nams),
                                             Labels = nams,
                                             Diag = FALSE,
                                             Upper = FALSE,
                                             method = "user"))
class(familydistances) <- "dist"
phonemesgenedata2=phonemesgenedata[!phonemesgenedata[,1]=='Scotland',]
phonemesgenedata2=phonemesgenedata2[!phonemesgenedata2[,1]=='Mozabites',]
phonemesgenedata2=phonemesgenedata2[!phonemesgenedata2[,1]=='Mansi',]
phonemesgenedata2=phonemesgenedata2[!phonemesgenedata2[,1]=='Gujarati',]

individual phonemes
fcodaDistance=c()
for (member in 1:length(site.x)){
  fcodaDistance=append(fcodaDistance,sum(abs(linguistic[match(site.x[member],linguistic[,4]),31]-linguistic[match(site.y[member],linguistic[,4]),31])))
}
df=data.frame(site.x,site.y,fcodaDistance)
fcodadistances <- with(df, fcodaDistance)
nams <- with(df, unique(c(as.character(site.x), as.character(site.y))))
attributes(fcodadistances) <- with(df, list(Size = length(nams),
                                           Labels = nams,
                                           Diag = FALSE,
                                           Upper = FALSE,
                                           method = "user"))
class(fcodadistances) <- "dist"