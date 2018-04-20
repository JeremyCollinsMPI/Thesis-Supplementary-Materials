
plot(table[,2])
table[match(2,table[,3]),1]
install.packages('ggplot2')
library(ggplot2)
ggplot(aes(x=table[,3],y=table[,2]))
qplot(x=table$chars,y=table[,3])

table=read.csv("~/Documents/Parallel Bibles/results2.txt",sep='\t',header=F, stringsAsFactors=FALSE)
findmax=function(number){
  a=table[,1][table[,number]==max(table[,number][!is.na(table[,2])])]
  return(a[!is.na(a)])
}
findmax(3)
table[,1]
write(table[,1],file="~/Documents/Parallel Bibles/results3.txt")
monkeys=function(member){
  tryCatch(return(substr(table[member,1],1,1)),error=function(e){return('')}) 
}
chars=c()
for (member in 1:length(table[,1])){
  x=monkeys(member)
  chars=append(chars,x)
}

