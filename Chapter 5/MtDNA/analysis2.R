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
randomcontrol=runif(69,0,1)

Phonotactics:
data=read.csv("~/Documents/Gene data/haplogroups11.txt",header=T,stringsAsFactors=F)
data=data2
phonotactics=read.csv("~/Documents/Gene data/Linguistic data/phonotactics.txt",header=F,sep='\t',stringsAsFactors=F)
coda=phonotactics[,3]
phonotactics[,1]
africa=c(25:28,56,57)
phonotactics[africa,3]
summary(lm(phonotactics[africa,4]~data[africa,83]))
pvalues=c()
for (member in 3:254){
  s=lm(data[,member]~coda)
  pvalues=append(pvalues,lmp(s))  
}
pvalues
summary(lm(phonotactics[,4]~data[,147]))
data[,147]
data[,c(1,147)]
hist(pvalues,breaks=20)
pvalues[!is.na(pvalues)]
install.packages('lme4')
library(lme4)
s=lmer(coda~data[,83]+(1|phonotactics[,5]))
t=lmer(coda~(1|phonotactics[,5]))
anova(s,t)[2,8]
pvalues=c()
for (member in 3:254){
  s=lmer(coda~data[,member]+(1|phonotactics[,5]))
  t=lmer(coda~(1|phonotactics[,5]))
  pvalues=append(pvalues,anova(s,t)[2,8])  
}
pvalues
hist(pvalues,breaks=20)
results=c()
for (pvalue in pvalues[!is.na(pvalues)]){
  if (pvalue<0.0001){
    results=append(results,match(pvalue,pvalues))
  }
  
}
results
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
results=c()
for (pvalue in pvalues[!is.na(pvalues)]){
  if (pvalue<0.001){
    results=append(results,haps[match(pvalue,pvalues)])
  }
  
}
results
hist(pvalues,breaks=20)

---
doing mantel tests with each haplogroup

triangle=function(x){
  return (((x*x)+x)/2)
}
triangle(77)
jez3
dist(data$H)
a=mantel.rtest(jez3,dist(data$H))
length(dist(data$H))
pvalues=c()
for (member in 3:254){
  s=mantel.rtest(jez3,dist(data[,member]))
  pvalues=append(pvalues,s$pvalue[1])  
}
hist(pvalues[!is.na(pvalues)],breaks=100)
results=c()
for (pvalue in pvalues[!is.na(pvalues)]){
  if (pvalue<(1/254)){
    results=append(results,haps[match(pvalue,pvalues)])
  }
  
}

Phonemes:
phonemes=read.csv("~/Documents/Gene data/Linguistic data/phonemedata.txt",sep='\t',header=F)
data=read.csv("~/Documents/Gene data/haplogroups12.txt",header=T,stringsAsFactors=F)
data=data2
table=phonemes
pvalues=c()
for (member in 3:254){
  for (member2 in 2:500){
    s=glm(table[,member2]~data[,member])
    if (length(!data[,member][!data[,member]==0])>7){
      if (length(table[,member2][table[,member2]==1])>7){
        if (length(table[,member2][table[,member2]==1])<71){
          pvalues=append(pvalues,summary(s)$coefficients[8])
        }
         
      }
      
    }
    
    
  }
  
}
hist(pvalues[!is.na(pvalues)],breaks=20)


results=c()
for (pvalue in pvalues[!is.na(pvalues)]){
  if (pvalue<0.001){
    results=append(results,haps[match(pvalue,pvalues)])
  }
  
}
results


s=glm(table[,2]~data$H)
summary(s)$coefficients[8]
length(table[1,])

pvalues=c()
for (member in 2:60){
  s=glm(table[,member]~data$H)
  pvalues=append(pvalues,summary(s)$coefficients[8])  
}

hist(pvalues[!is.na(pvalues)],breaks=20)

pvalues=c()
for (member in 3:254){
  for (member2 in 2:301){
    s=glm(table[,member2]~data[,member])
    pvalues=append(pvalues,summary(s)$coefficients[8])
    
  }
  
}
hist(pvalues[!is.na(pvalues)],breaks=20)


---
  
word order mantel tests
data=read.csv("~/Documents/Gene data/haplogroups13.txt",header=T,stringsAsFactors=F)
data=data2
a=mantel.rtest(jez3,dist(data$H))
length(dist(data$H))
pvalues=c()
for (member in 3:254){
  s=mantel.rtest(jez3,dist(data[,member]))
  pvalues=append(pvalues,s$pvalue[1])  
}
hist(pvalues[!is.na(pvalues)],breaks=20)
results=c()
for (pvalue in pvalues[!is.na(pvalues)]){
  if (pvalue<(1/254)){
    results=append(results,haps[match(pvalue,pvalues)])
  }
  
}
results

pvalues=c()
for (member in 3:254){
  for (member2 in 5:11){
    s=glm(table[,member2]~data[,member])
    pvalues=append(pvalues,summary(s)$coefficients[8])
    
  }
  
}
hist(pvalues[!is.na(pvalues)],breaks=20)


data=read.csv("~/Documents/Gene data/haplogroups12.txt",header=T,stringsAsFactors=F)
phonemedata=read.csv("~/Documents/Gene data/phonemedata.txt",sep='\t',header=F)
s=glm(table[,2]~data$H)
summary(s)$coefficients[8]
length(table[1,])

table=read.csv("~/Documents/Word order/data2.txt",sep='\t',header=F)
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
s=glm(table[,5]~data$H)
summary(s)$coefficients[8]


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
hist(pvalues[!is.na(pvalues)],breaks=20)

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

permuting results

permuteresults=c()
for (n in 1:100)
{
  selectgenes=c()
  selectcoda=c()
  for (member in levels(table$Family))
  {
    
    choice=sample(1:length(table$H[table$Family==member]),1)
    selectgenes=append(selectgenes,table$H[table$Family==member][choice])
    selectcoda=append(selectcoda,table$Coda[table$Family==member][choice])
  }
  selectgenes
  selectcoda
  s=lm(selectcoda~selectgenes)
  permuteresults=append(permuteresults,lmp(s))	
}
phonotactics
