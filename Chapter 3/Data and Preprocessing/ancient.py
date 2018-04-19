from __future__ import division
import geocoder

import numpy
import random
import math
import matplotlib.pyplot as plt
def log(x):
	return math.log(x)

# g = geocoder.google("England")
# print g.latlng

file=open('31485 documents.gb','r').readlines()
file2=open('ancient2.txt','w')
country=open('countries.txt','r').readlines()
print country
country2=[x.replace('\n','') for x in country if not x=='\n']
print country2
country=country2

		
stop=0
stop2=0
z=''
results=[]
print len(country)
for member in country:
	if not member=='':
		print member
		x=geocoder.google(member)
		print x.latlng
		counter=1
		ancient='date'
		
		for member2 in file:
			
			if 'Llamas,B., Fehren-Schmitz,L.,' in member2:
				ancient='ancient'
# 			if 'ancient' in member2 or 'Ancient' in member2 or 'date' in member2:
# 				ancient='ancient'
			if 'ACCESSION' in member2:
				ancient='date'		
			if ancient=='ancient' and 'note' in member2:
				ancient='ancient ' + member2 + '\n'
				
			if 'country' in member2:
				try:
					zn= (member2.split('"')[1]).replace('\n','')	
					if zn==member:
						stop=1
						print 'moose'
				except:
					pass			
# 			if member in member2:
# 	# 			print 'moos'
# 				stop=1
			
				
			if 'ORIGIN' in member2 and stop==1:
				stop2=1	
	# 			print 'mate'
				continue
			if stop2==1:
	# 			print 'harry'
				memberlist=list(member2)
				for member3 in memberlist:
					if member3 in ['a','c','t','g','A','C','T','G','-']:
						z=z+member3
			if '//' in member2:
				stop=0
				stop2=0
				if not z=='':
					y=member.replace(' ','')
					y=y.replace(':','')
					results.append(['>'+y+str(counter)+'_'+str(x.lat)+'_'+str(x.lng),ancient,z])
					z=''		
					ancient='date'
					counter=counter+1	
					print counter


for member in results:
	if 'ancient' in member[1]:
		for member2 in member:
			file2.write(member2)
			file2.write('\n')

# sorting ancient.


			
# ------

# file=open('results.txt','r').readlines()
# for member in file:
# 	count=0
# 	if '_None_None' in member:
# 		print member
# 	if '>' in member:
# 		count=count+1
# 	print count		


# -------
# output=open('results2b.txt','w')
# file=open('results.txt','r').readlines()
# countries=open('countries.txt','r').readlines()
# print countries
# count=0
# for member in countries:
# 	member=member.replace('\n','')
# 	member=member.replace(' ','')
# 	for x in member:
# 		if not x.isalpha():
# 			member=member.replace(x,'')
# 	print 'plush'+member
# 	cands=[]
# 	for c in range(0,len(file)-1):
# 		member2=file[c]
# 		if '>' in member2 and not '_None_None' in member2:
# 			print member2
# 			a=''
# 			member2=member2.replace('>','')
# 			for member3 in member2:
# 				
# 				if member3.isalpha():
# 					a=a+member3
# 				else:	
# 					continue
# 			print 'ho' +' '+member
# 			print a
# 			print member			
# 			if a==member and len(file[c+1])>16000 and len(file[c+1])<17000:
# 				member5=member2.replace(a,'')
# 				member5=member5.split('_')
# 				print member
# 				print member5
# 				lat=member5[1]
# 				long=member5[2].replace('\n','')
# 				print float(long)
# # 				if float(long)<-30:
# 				print 'mor'
# 				if float(long)<-30:
# 					if file[c].replace(',','') in open('native_names.txt','r').readlines():
# 						cands.append(member2)
# 				else:		
# 					cands.append(member2)
# 				print cands
# 	print 'zoo'+'\t'+member			
# 	print cands
# 	
# 		
# 	if len(cands)>0:		
# 		print 'max1'
# # 		chosen=random.sample(cands,len(cands))
# 
# # IF YOU WANT TO SAMPLE ONE:
# 		chosen=random.sample(cands,1)
# #  IF YOU WANT TO USE ALL:
# 		chosen=cands
# # IF YOU WANT TO CAP IT AT A CERTAIN NUMBER:
# 		cap=97
# 		if len(cands)>cap:
# 			chosen=random.sample(cands,cap)		
# 		print 'max2'		
# 		print chosen
# # 		if 'China' in member:
# # 		for member3 in cands:
# # 			for member2 in range(0,len(file)):
# # 				z=file[member2].replace('>','')		
# # 				
# # 				if 'Zhejiang' in z:
# # 					print 'jezza'
# # 					print z
# # 					print member3
# # 				if z==member3:
# # 					count=count+1
# # 					output.write('>'+z)
# # 					print count
# # 					print 'max3'
# # 					print z
# # 					n=1
# # 					try:
# # 						while not '>' in file[member2+n]:
# # # 							print file[member2+n]
# # 							output.write(file[member2+n])
# # 							n=n+1
# # 					except:
# # 						pass
# # 		else:
# 		for member2 in range(0,len(file)):
# 			z=file[member2].replace('>','')		
# 			if z in chosen:				
# # 			if z==chosen and count<50:	
# 				output.write('>'+z)
# 				count=count+1
# 				print count
# 				print 'max3'
# 				print z
# 				n=1
# 				try:
# 					while not '>' in file[member2+n]:
# 	# 						print file[member2+n]
# 						output.write(file[member2+n])
# 						n=n+1
# 				except:
# 					pass
			 			
	
