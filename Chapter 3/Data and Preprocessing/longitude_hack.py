from __future__ import division
import random
file1=open('output3.nex','r').readlines()
file2=open('output4.nex','w')
min=0
max=6028
total=0
for m in range(0,len(file1)):
	print m
	print file1[m]
	if m<6 or m>6034:
		if m==3:
			file2.write(file1[m].replace('6028','JEREMYCOLLINS'))
		else:
			file2.write(file1[m])
	elif m<max+6 and m>=min+6:	
		x=random.random()
		if x<(7000/6028):
			total=total+1
			print m
			print file1[m]
			list1=(file1[m].split('\t')[1]).split('_')
			print list1
			if float(list1[2])<-30:
				list1[2]=float(list1[2])+360
			list1[2]=str(float(list1[2])-150)
			print list1
			file2.write(file1[m].replace(file1[m].split('\t')[1],'_'.join(list1)))	
file2.close()
file1=open('output4.nex','r').readlines()
file2=open('output4.nex','w')
for m in range(0,len(file1)):
	if m==3:
		file2.write(file1[m].replace('JEREMYCOLLINS',str(total)))
	else:
		file2.write(file1[m])	