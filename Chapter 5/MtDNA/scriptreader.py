# -*- coding: utf-8 -*-
from __future__ import division
"""
Created on Tue Oct 29 16:24:54 2013

@author: jercol
"""

# -*- coding: utf-8 -*-
"""
Spyder Editor

This temporary script file is located here:
/Network/Servers/osx01.mpi.nl/Volumes/data2/home/jercol/.spyder2/.temp.py
"""
import random
from random import randint

import urllib2

def is_number(x):
	try:
		 float(x)
		 return True
	except ValueError:
		return False
		
# data=open('/Users/jercol/Documents/Gene data/derenko.txt')
# data=data.readlines()
# haps=[]
# for n in range(1,19):
# 	list=[]
# 	for item in data:
# 		item=item.split('\t')
# 		haps.append(item[0])
# 		if ' ' in item[n]:
# 			item[n]=item[n].replace(' ','')
# 		if '\n' in item[n]:
# 			item[n]=item[n].replace('\n','')		
# 		if not item[n]=='…':
# 			list.append(item[n])
# 		elif item[n]=='…':
# 			list.append('0')	
# 		elif item[n]=='…\n':
# 			list.append('0')
# 
# 	x=''
# 	for item3 in list:
# 		item4=item3
# 		if is_number(item3):
# 			item4=float(item3)/100		
# 			x=x+str(item4)+','
# 		else:
# 			if '(n=' in item4:
# 				item4=item4.replace('(n=',',')
# 				item4=item4.replace(')','')
# 			x=x+str(item4)+','
# 			
# 	print(x)		
# print(','.join(haps)) 	

# data=open('/Users/jercol/Documents/Gene data/fedorova.txt')
# data=data.readlines()
# 
# for item in range(2,20):	
# 	item2=data[item].split(' ')
# 	list=[]
# 	for item3 in item2:
# 		list.append(item3)
# 
# 	names=data[0].split(',')
# 	for item4 in range(0,len(list)):
# 		list[item4]=float(list[item4])
# 	for item4 in range(0,len(list)):
# 		if item4>0:
# 			list[item4]=list[item4]/100
# 	y=names[item-2]
# 	for member in list:
# 		y=y+','+str(member)
# 	print(y)				
		
# data=open('/Users/jercol/Documents/Gene data/helgason.txt')
# data=data.readlines()
# haps=[]
# for item in range(1,16):
# 	list=[]
# 	for line in data:
# 		line=line.split('\t')
# 		haps.append(line[0])
# 		if is_number(line[item]):
# 			y=str(float(line[item])/100)
# 		else:
# 			y=line[item]
# 		list.append(y)		
# 	x=','.join(list)
# 	print(x)	
# print(','.join(haps)) 	

# data=open('/Users/jercol/Documents/Gene data/peng.txt')
# data=data.readlines()
# haplogroups=[]
# for item in range(2,953):
# 	line=data[item].split('\t')
# 	if line[0] not in haplogroups:
# 		haplogroups.append(line[0])
# 
# x='Japanese,952'		
# for haplogroup in haplogroups:	
# 	n=0
# 	for item in range(2,953):
# 		line=data[item].split('\t')
# 		if haplogroup==line[0]:
# 			n=n+1
# 
# 	x=x+','+str(n/952)
# print(x)			
# print(','.join(haplogroups))

# data=open('/Users/jercol/Documents/Gene data/li.txt')
# data=data.readlines()
# haplogroups=[]
# populations=[]
# families=[]
# for item in range(3,902):
# 	line=data[item].split('\t')
# 	if line[1] not in haplogroups:
# 		haplogroups.append(line[1])
# 	if line[-3] not in populations:
# 		populations.append(line[-3])
# 	if line[-2] not in families:
# 		families.append(line[-2])	
# for member in families:
# 
# 	c=''
# 	for haplogroup in haplogroups:
# 		a=0
# 		b=0
# 		for item in range(3,902):
# 			line=data[item].split('\t')				
# 			if member in line:
# 				b=b+1
# 				if haplogroup in line:
# 					a=a+1
# 		c=c+','+str(a/b)			
# 	d=member+','+str(b)+c
# 	print(d)
# print(','.join(haplogroups))

# data=open('/Users/jercol/Documents/Gene data/tanaka.txt')
# data=data.readlines()
# haplogroups=[]
# populations=[]
# families=[]
# for item in range(3,86):
# 	line=data[item].split('\t')
# 	if line[0] not in haplogroups:
# 		haplogroups.append(line[0])
# line=data[0].split('\t')
# for n in range(1,len(line)):
# 	population=line[n]
# 	if '\n' in population:
# 		population=population.replace('\n','')
# 	haps=[]
# 	size2=data[2].split('\t')
# 	size=size2[n]
# 	if '\n' in size:
# 		size=size.replace('\n','')
# 	for line2 in range(3,86):
# 		x=data[line2].split('\t')
# 		a=x[n]
# 		b=[a]
# 		if '\n' in a:
# 			a=a.replace('\n','')
# 		if is_number(a):
# 			a=float(a)/100	
# 		haps.append(str(a))
# 	haps=",".join(haps)
# 	print(population+","+size+","+haps)	
# 	
# print(','.join(haplogroups))
# print(len(haplogroups))

# data=open('/Users/jercol/Documents/Gene data/wen.txt')
# data=data.readlines()
# haplogroups=[]
# populations=[['Hmong','103'],['Miao',str((35+10+24+19+32+32+41+11+27+40+26+42+31))]]
# for n in range(1,44):
# 	line=data[n].split('\t')
# 	for member in range(0,len(line)):
# 		if '\n' in line[member]:
# 			line[member]=line[member].replace('\n','')
# 	if not line[0] in haplogroups:
# 		haplogroups.append(line[0])
# 	hmong=line[1]
# 	if line[1]=='':
# 		hmong=0	
# 	populations[0].append(str(hmong))
# 	miao=0
# 	for o in range(5,18):
# 		if line[o]=='':
# 			line[o]=0
# 		miao=miao+float(line[o])
# 	populations[1].append(str(miao))
# print(','.join(populations[0])+'\n'+','.join(populations[1])+'\n'+','.join(haplogroups))

# data=open('/Users/jercol/Documents/Gene data/pengcham.txt')
# data=data.readlines()
# haplogroups=[]
# populations=[]
# 
# line=data[0].split('\t')
# line2=data[77].split('\t')
# for member in range(1,len(line)):
# 	a=line[member].replace(' ','')
# 	b=line2[member].replace(' ','')
# 	
# 	populations.append([a,b])
# for n in range(1,77):
# 	line=data[n].split('\t')
# 	haplogroups.append(line[0])
# 	for member in range(1,len(line)):
# 		if '\n' in line[member]:	
# 			line[member]=line[member].replace('\n','')
# 		if ' ' in line[member]:	
# 			line[member]=line[member].replace(' ','')	
# 		if line[member]=='':
# 			line[member]='0'
# 		populations[member-1].append(line[member])					
# for member in populations:
# 	print(','.join(member))	
# print(','.join(haplogroups))	
	
	
# data=open('/Users/jercol/Documents/Gene data/duggan.txt')
# data=data.readlines()
# haplogroups=[]
# populations=[]
# line=data[0].split('\t')
# haplogroups=[hap for hap in line if not hap=='Population']
# haplogroups[-1]=haplogroups[-1].replace('\n','')
# for n in range(1,40):
# 	line=data[n].split('\t')
# 	populations.append([line[0]])
# for population in populations:
# 	name=population[0]
# 	count=0
# 	for n in range(40,len(data)):
# 		name2=data[n].replace('\n','')
# 		if name==name2:
# 			count=count+1
# 	population.append(str(count))
# 	
# for n in range(1,40):
# 	line=data[n].split('\t')	
# 	for m in range(1,len(line)):
# 		if '\n' in line[m]:
# 			line[m]=line[m].replace('\n','')
# 		if line[m]=='' or line[m]==' ':
# 			line[m]='0'
# 		if ' ' in line[m]:
# 			line[m]=line[m].replace(' ','')
# 		populations[n-1].append(line[m])				
# print('Population,Size,'+','.join(haplogroups))
# for pop in populations:
# 	print(','.join(pop))


# data=open('/Users/jercol/Documents/Gene data/barbierimandenka.txt')
# data=data.readlines()
# haplogroups=[]
# populations=[]
# line=data[0].split('\t')
# haplogroups=[hap for hap in line if not hap==' ' and not hap=='n ' and not hap=='']
# haplogroups[-1]=haplogroups[-1].replace('\n','')
# print(haplogroups)
# for n in range(2,12):
# 	line=data[n].split('\t')	
# 	populations.append([line[0],line[1]])
# 	for m in range(1,len(line)):
# 		if '\n' in line[m]:
# 			line[m]=line[m].replace('\n','')
# 		if line[m]=='' or line[m]==' ':
# 			line[m]='0'
# 		if ' ' in line[m]:
# 			line[m]=line[m].replace(' ','')
# 		populations[-1].append(line[m])
# 						
# print('Population,Size,'+','.join(haplogroups))
# for pop in populations:
# 	print(','.join(pop))

# data=open('/Users/jercol/Documents/Gene data/quintana-murci.txt')
# data=data.readlines()
# haplogroups=[]
# populations=[]
# line=data[1].split('\t')
# line2=data[2].split('\t')
# for member in range(0,len(line)):
# 	populations.append([line[member],line2[member+1]])
# for n in range(3,len(data)):
# 	line=data[n].split('\t')	
# 	haplogroups.append(line[0])
# 	for m in range(1,len(line)):
# 		populations[m-1].append(line[m])
# 	
# 						
# print('Population,Size,'+','.join(haplogroups))
# for pop in populations:
# 	print(','.join(pop))

	
# data=open('/Users/jercol/Documents/Gene data/haplogroups.txt')
# data=data.readlines()
# haps=["L1b","L1c","L2a","L2b","L2c","L3b","L3d","L3e","C","D","E","G","Z","A","X","Y","W","I","B","J1","J2","T1","T2","T5","HV","V","H1","H2","U6","U5a","U5b","U2","U3","U4","U7","K"]
# haplist=[]
# for line in data:	
# 	line=line.split(',')
# 	if line[0]=='New\n':
# 		continue
# 	if line[0]=='Population':
# 		haplist.append(line)
# for hap in haps:
# 	for 		
# 			
# 	if line[0]='New\n':	



# data=open('/Users/jercol/Documents/Gene data/phylotreel*.txt')
# data=data.readlines()		
# 
# stack=[]
# current=stack
# metastack=[stack]
# dummynumber=0
# def nest():
# 	global current
# 	current.append([])
# 	metastack.append(current)
# 	print(current)
# 	print(metastack)
# 
# 	current=current[-1]
# 	print(current)
# 	print(metastack)
# 	print(current)
# def unnest():
# 	global current
# 	global dummynumber
# 	print(current)
# 	print(metastack)
# 	current2=[thing for thing in current if isinstance(thing,str)]
# 	if len(current2)==1:
# 		current.append("dummy"+str(dummynumber)+":"+"1")
# 		dummynumber=dummynumber+1
# 	current=metastack[-1]
# 	metastack.pop()
# 	print(current)
# 	print(metastack)




# previouscounter=0
# counter=0
# stack=[]
# current=stack
# for n in range(0,len(data)):
# 	counter=0
# 	global stack
# 	line=data[n].split('\t')
# 	haplogroup=''
# 	snpnumber=0
# # 	print(line)
# # 	print(counter)
# # 	m=[x for x in line if not x==' ' and not x=='\n' and not x=='' and not x==' \n']
# 	m=[]
# 	fl=0
# 	for x in line:
# 		if not x==' ' and not x=='\n' and not x=='' and not x==' \n' and not fl==2:
# 			m.append(x)
# 			if fl==0:
# 				haplogroup=x
# 			fl=1
# 		if (x==' ' or x=='\n' or x=='' or x==' \n') and fl==1:
# 			fl=2
# 			
# # 	print(m)		
# 	if m==[]:
# 		continue	
# 	for member in line:
# 		if not member== ' ':
# 			break
# 		if member==' ':
# 			counter=counter+1	
# # 	print(line)
# 	if not m==[]:
# 		if "'" in haplogroup:
# 			haplogroup=haplogroup.replace("'","_")
# 		else:
# 			haplogroup=m[0]	
# # 		print(haplogroup)
# # 		print(counter)
# # 		print(haplogroup)
# 		snps=[]
# 		a=[b for b in m if not b==m[0]]
# 		for member in a:
# 			c=member.split(' ')
# # 			print(c)
# 			for member2 in c:
# 				snps.append(member2)
# 		snps=[snp for snp in snps if not snp=='']
# 		branchlength=len(snps)
# 		if branchlength==0:
# 			branchlength=1
# 			counter=counter-1
# 			if ' ' in haplogroup:
# 				haplogroup=haplogroup.replace(' ','')
# 			if '!' in haplogroup:
# 				haplogroup=haplogroup.replace('!','')
# 			if '(' in haplogroup:
# 				haplogroup=haplogroup.replace('(','')
# 			if ')' in haplogroup:	
# 				haplogroup=haplogroup.replace(')','')			
# # 		print(snps)	
# 	global current
# # 	print(current)
# 	if counter==previouscounter:		
# 		current.append(haplogroup+":"+str(branchlength))			
# 	if counter>previouscounter:
# 		nest()
# 		current.append(haplogroup+":"+str(branchlength))			
# 		previouscounter=counter	
# 	if counter<previouscounter:
# # 		print('yes')
# # 		print(current)
# 
# 		iter=0
# 		while iter<(previouscounter-counter):
# 			unnest()
# 			iter=iter+1
# 		current.append(haplogroup+":"+str(branchlength))			
# 		previouscounter=counter	
# # 		print(current)	
# 
# # 		branchlength=len([snp for snp in snps if not snp==''])	
# 	if n==len(data)-1:
# 		
# 		iter=0
# 		while iter<(previouscounter):
# 			unnest()
# 			iter=iter+1
# 		
# print(stack)
# 
# 
# 
# 
# 	
# 
# 
# listo=['a','b',['b1',['f','k'],'b2'],'c','d',['f','y']]
# def writetree(x):
# 	x2=[thing for thing in x if isinstance(thing,str)]
# 	y='('
# 	for item in range(0,len(x)):
# 		try:
# 			if isinstance(x[item+1],list):
# 				z=writetree(x[item+1])
# 				y=y+z
# 		except:
# 			pass
# 		if isinstance(x[item],str) and not x[item]==x2[len(x2)-1]:
# 			y=y+x[item]+','	
# 		if isinstance(x[item],str) and x[item]==x2[len(x2)-1]:
# 			y=y+x[item]
# 	y=y+')'
# 	return(y)	
# print(writetree(stack))			

# import urllib2
# 
# response=urllib2.urlopen("http://www.etymonline.com/index.php?term=infant&allowed_in_frame=0")
# f=response.readlines()		
# for member in f:
# 	print(member)




# data=open('/Users/jercol/Documents/Gene data/haplogroups5.txt','r')
# data2=open('/Users/jercol/Documents/Gene data/haplogroups6.txt','w')
# data=data.read()
# data=data.replace(',A4,',',A,')
# data=data.replace(',C*,',',C,')
# data=data.replace('G*','G')
# data=data.replace('M*','M')
# data=data.replace('T*','T')
# data=data.replace('R*','R')
# data=data.replace(',J1a,',',J1,')
# data=data.replace(',M/N,',',L3,')
# data=data.replace('I/W/N','N')
# data=data.replace(',A1b,',',A1,')
# data=data.replace(',A1b1,',',A1,')
# data=data.replace(',UK,',',U,')
# data=data.replace(',R9a,',',R9,')
# data=data.replace(',F2a1,',',F2a,')
# data=data.replace(',M5/D4a/G1,',',M,')
# data=data.replace(',G1a1/D,','G1a')
# data=data.replace(',G5,',',G,')
# data=data.replace('J/T','JT')
# data=data.replace(',HV*,',',HV,')
# data=data.replace(',L,',',L0,')
# data=data.replace(',N1c,',',N1,')
# data=data.replace(',N1d,',',N1,')
# data=data.replace(',T*(xT1),',',T,')
# data=data.replace(',L1-3,',',L1,')
# data=data.replace(',M9bp,',',M9b,')
# data=data.replace(',U2i/U7,',',U2,')
# data=data.replace('U1/U3-6/U*','U1')
# data=data.replace('HV/TJ/N1/X','N')
# data=data.replace('B/F','R9')
# data=data.replace(',B1,',',B,')
# data=data.replace(',B3,',',B,')
# data=data.replace(',C2,',',C,')
# data=data.replace(',C3,',',C,')
# data=data.replace(',Q1+,',',Q1,')
# data=data.replace('B4a1a1a+16247!','B4a1a1a')
# data=data.replace('B4a1a1a1+16247!','B4a1a1a1')
# data=data.replace('E1b+16261','E1b')
# data=data.replace('M7c3c','M7c3')
# data=data.replace('B*','B')
# data=data.replace('B4*','B4')
# data=data.replace('B4a*','B4a')
# data=data.replace('D*','D')
# data=data.replace('F1*','F1')
# data=data.replace('F1a*','F1a')
# data=data.replace('F1a1*','F1a1')
# data=data.replace('M21c','M21')
# data=data.replace('M21d','M21')
# data=data.replace('M7*','M7')
# data=data.replace('M7b*','M7b')
# data=data.replace('M7b3','M7b')
# data=data.replace('M7c*','M7c')
# data=data.replace('M7c1*','M7c1')
# data=data.replace('N*','N')
# data=data.replace('N23','N')
# data=data.replace('N9a*','N9a')
# data=data.replace('R9d','R9')
# data=data.replace('N*','N')
# 
# data2.write(data)
# 
# 
# data=open('/Users/jercol/Documents/Gene data/haplogroups5.txt')
# data=data.readlines()
# haplogroups=[]
# 
# print data[0]
# line=data[0].split(',')
# print(line)
# for member in line:
# 	if not member in haplogroups:
# 		haplogroups.append(member)

# data=open('/Users/jercol/Documents/Gene data/haplogroups4.txt','r')
# data=data.readlines()
# data2=open('/Users/jercol/Documents/Gene data/haplogroups7.txt','w')
# counter=''
# for line in data:
# 	line2=line
# 	countline=line.split('\t')
# 	line=line.split(',')
# 	if line[0]=='Population':
# 		continue
# 	if countline[0]=='New':
# 		counter=countline[1].replace('\n','')
# 		continue
# 	if counter=='tanaka':
# 		line2=line2.replace(' -','0')
# 		print(line2)		
# 	if counter=='plaza':
# # 		print('yes')
# # 		print(line)
# 		for m in range(2,len(line)):
# 			if line[m]=='-' or line[m]=='-\n' or line[m]=='\n' or line[m]=='' or line[m]==' \n':
# # 				print 'monkey'
# 				line[m]='0'
# 			try:
# 				line[m]=str(float(line[m])/100)
# 			except:	
# # 				print('what')
# 				print(line[m])	
# 		line2=','.join(line)
# 		print(line2)
# 	if counter=='yunusbayev' or counter=='india' or counter=='wen':
# 		x=[]
# 		for member in range(0,2):
# 			x.append(line[member])
# 		for member in range(2,len(line)):
# 			x.append(str(float(line[member])/float(line[1])))
# 		line2=','.join(x)
# 		print(line2)	
# 	if counter=='pengcham' or counter=='barbieripdf' or counter=='starikovskaya' or counter=='quintana-murci':
# 		x=[]
# 		for member in range(0,2):
# 			x.append(line[member])
# 		for member in range(2,len(line)):
# 			x.append(str(float(line[member])/100))
# 		line2=','.join(x)
# 		print(line2)			

# haplogroups=[]							
# data=open('/Users/jercol/Documents/Gene data/haplogroups8.txt')
# data=data.readlines()
# for line in data:
# 	line=line.split(",")
# 	line[1]=line[1].replace('"','')
# 	line[1]=line[1].replace('\n','')
# 	haplogroups.append(line[1])
# 			
# file=open('/Users/jercol/Documents/Gene data/haplogroups9.txt','w')
# file.write('Population,'+'Size,'+','.join(haplogroups)+'\n')	
# collection=[]		
# data=open('/Users/jercol/Documents/Gene data/haplogroups7.txt')
# data=data.readlines()
# counter=''
# columns=[]	
# for n in range(1,len(data)):
# 	line=data[n].split('\t')
# 	if line[0]=='New':
# 		counter=line[1]
# 		continue
# 	line=data[n].split(',')
# 	if line[0]=='Population':
# 		columns=line		
# 		continue
# 	for member in range(0,len(columns)):
# 		if is_number(columns[member]):
# 			columns[member]=float(columns[member])
# 	for member in range(0,len(line)):
# 		if is_number(line[member]):
# 			line[member]=float(line[member])
# 	collection.append([])
# 	collection[-1].append(line[0])
# 	collection[-1].append(line[1])
# 	for number in range(0,len(haplogroups)):
# 		collection[-1].append(0)
# ##	for m in range(2,len(line)-1):
# # -1 in order to avoid the last element 'Others'
# 	for m in range(2,len(line)):
# # not -1 because 'Others' should not be recognised
# 			try:
# 			 if columns[m] in haplogroups:
# 				x=haplogroups.index(columns[m])			
# 				collection[-1][x+2]=collection[-1][x+2]+line[m]
# 			except:
# 				print columns	
# for member in collection:
# 	for member2 in range(0,len(member)):
# 		member[member2]=str(member[member2])
# for member in range(0,len(collection)):
# 	collection[member]=','.join(collection[member])
# collection='\n'.join(collection)
# file.write(collection)	
# print(haplogroups)
# print(','.join(haplogroups))
# 
# print len(data[66].split(','))
# for n in range(66,93):
# 	print(data[n])
# 	
# 	print len(data[n].split(','))
# print data[66].split(',')


# file=open('/Users/jercol/Documents/Gene data/haplogroups9.txt','w')
# file.write('Population,'+'Size,'+','.join(haplogroups))	
# collection=[]		
# data=open('/Users/jercol/Documents/Gene data/haplogroups7.txt')
# data=data.readlines()
# counter=''
# columns=[]	
# for n in range(1,len(data)):
# 	line=data[n].split('\t')
# 	if line[0]=='New':
# 		counter=line[1]
# 		continue
# 	line=data[n].split(',')
# 	if line[0]=='Population':
# 		columns=line		
# 	columns=data[0].split(',')
# 	for member in range(0,len(columns)):
# 		if is_number(columns[member]):
# 			columns[member]=float(columns[member])
# 	for member in range(0,len(line)):
# 		if is_number(line[member]):
# 			line[member]=float(line[member])
# 	collection.append([])
# 	collection[-1].append(line[0])
# 	collection[-1].append(line[1])
# 	for number in range(0,len(haplogroups)):
# 		collection[-1].append(0)
# ##	for m in range(2,len(line)-1):
# # -1 in order to avoid the last element 'Others'
# 	for m in range(2,len(line)):
# # not -1 because 'Others' should not be recognised
# 		 if columns[m] in haplogroups:
# 		 	x=haplogroups.index(columns[m])			
# 			collection[-1][x+2]=collection[-1][x+2]+line[m]
# for member in collection:
# 	for member2 in range(0,len(member)):
# 		member[member2]=str(member[member2])
# for member in range(0,len(collection)):
# 	collection[member]=','.join(collection[member])
# collection='\n'.join(collection)
# file.write(collection)	
# print(haplogroups)
# print(','.join(haplogroups))


data=open('/Users/jercol/Documents/Gene data/haplogroups9.txt')
data=data.readlines()
data2=open('/Users/jercol/Documents/Gene data/languages3.txt')
data2=data2.readlines()
file=open('/Users/jercol/Documents/Gene data/haplogroups13.txt','w')
languages=[]
file.write(data[0])
for n in range(0,len(data)):
	if len(data2[n].split(','))==1:
		file.write(data[n])		
		languages.append(data2[n].replace('\n',''))
print languages		
