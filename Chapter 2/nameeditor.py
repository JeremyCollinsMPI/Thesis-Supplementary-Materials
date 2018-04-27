# name of input tree here
file=open('1.tree','r').readlines()
file2=open('data.csv','r').readlines()
# name of output tree here
file3=open('1b.tree','w')
line=''
for m in xrange(len(file)):
	if 'tree_1' in file[m]:
		line=file[m]
		break
names=[]
families=[]
line2=line.split('[&height=')
for m in xrange(len(line2)-1):
	x=line2[m]
	y=x[len(x)-3:len(x)]
	y=''.join(y)
	
	if y.isalpha():
# 		print y
		for n in range(1,len(file2)):
			if file2[n].split(',')[4]==y:
				name=file2[n].split(',')[0]
				family=file2[n].split(',')[26*6+12]
				names.append(name)
				families.append(family)
	else:
		names.append('NA')
		families.append('NA')
	print y
	print names[m]
	
for m in range(1,len(line2)):
	if not names[m-1]=='NA':
		line2[m]=line2[m].replace("]:",',name2="MONK",family="GOOSE"]:')
		line2[m]=line2[m].replace("MONK",names[m-1])
		line2[m]=line2[m].replace("GOOSE",families[m-1])
line3='[&height='.join(line2)
file4=open('2test.tree','r').read()
file3.write(file4.replace(line,line3))