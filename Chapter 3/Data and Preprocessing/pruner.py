import random
import numpy as np
outputstring='output6.nex'


file=open('output4.nex','r').readlines()
countries=open('countries.txt','r').readlines()

# output1=open(outputstring,'w')
# 
# ancient_taxa=open('ancient_taxa.txt','r').readlines()
# 
# ancient_taxa=[(x.split('\t')[0]).split('_')[0] for x in ancient_taxa] 
# 
# output=''
# for m in range(0,7):
# 	output=output+file[m]
# limit=1
# locations=[]
# for m in range(7,len(file)-1):
# 	member=file[m].split('_')
# 
# 	string=''
# 	for a in member[0]:
# 		if a.isalpha():
# 			string=string+a
# 			
# 	
# 	if not string in locations:
# 		print string
# 		locations.append(string)
# 
# maximum=900
# total=0
# remainder=0
# for member in locations:
# 	if total==maximum or total>maximum:
# 		break
# 	else:
# 		cands=[]
# 		for m in range(7,len(file)):
# 			string=''
# 			member2=file[m].split('-')
# 			for a in member2[0]:
# 				if a.isalpha():
# 					string=string+a
# 			if member==string:
# 				x=file[m].split('_')[0].replace('\t','')
# 				x=x.replace('\n','')
# # 				print x
# # 				print ancient_taxa
# 				if x in ancient_taxa:
# # 					print 'yes'
# 					output=output+file[m]
# 					remainder=remainder+1
# 				else:				
# 					cands.append(file[m])
# 		
# 
# 		number=min(len(cands),limit)
# 		cands2 = random.sample(cands,number)		
# 		for member2 in cands2:
# 			output=output+member2	
# 			total=total+1
# 		
# tom='NTAX ='+str(total+remainder)
# print tom	
# if 'NTAX =6028' in output:
# 	print 'jez'	
# output=output.replace('NTAX =6028',tom)		
# if 'NTAX =6028' in output:
# 	print 'jez'	
# output1.write(output)		
# output1.write('End;')	



# ------------
#  A PART TO CREATE TWO PARTITIONS

# file=open('output4.nex','r').readlines()
# output1=open('output4_partition1.nex','w')
# output2=open('output4_partition2.nex','w')
# 	
# for m in range(0,6):
# 	if m==3:
# 		output1.write(file[m].replace('16786','15507'))
# 		output2.write(file[m].replace('16786','1279'))
# 	else:
# 		output1.write(file[m])
# 		output2.write(file[m])
# 	
# for m in range(6,len(file)-1):
# 	try:
# 		if m==6:
# 			member=list(file[m].split('\t')[3])
# 			name=file[m].split('\t')[1]
# 		else:
# 			member=list(file[m].split('\t')[3])
# 			name=file[m].split('\t')[1]
# 		print len(member)
# 		string1=''
# 		string2=''
# 		for n in range(637,16144):
# 			string1=string1+member[n]
# 		for n in range(0,637):
# 			string2=string2+member[n]
# 		for n in range(16144,len(member)):
# 			string2=string2+member[n]
# 		output1.write(name+'\t'+string1+'\n')
# 		output2.write(name+'\t'+string2+'\n')		
# 	except:
# 		print file[m]	
# output1.write('End;')
# output2.write('End;')


# ------------
#  A PART TO CREATE PARTITIONS FOR CODON POSITIONS AND D-LOOP


def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False
results=open('results2.txt','r').readlines()
file=open('all.txt','r').readlines()
string=open('string.txt','r').read()
string=string.replace('\n','')
string=string.replace('\t','')
string2=''
for m in string:
	if m in ['a','c','t','g','-']:
		string2=string2+m
# print string2
string=string2		
cambridge=open('cambridge.txt').readlines()
genes= [x for x in cambridge if 'CDS   ' in x]
bounds=[]
for m in xrange(len(genes)):
	temp=[]
	number1=''
	for n in genes[m]:
		if is_number(n):
			number1=number1+n
		else:
			if not number1=='':
				temp.append(int(number1))
				number1=''
	bounds.append(temp)			
print bounds
print string
string=list(string)
string2=[x for x in string if not x=='-']
n=1
aligned_bounds=[]
temp=[]
on=0
for m in xrange(len(string2)):
	
	if n in [x[0] for x in bounds] and not on==1:
		print on
		print m
		print n
		on=1
		temp.append(m)
	if n in [x[1] for x in bounds] and on==1:
		print on
		print m
		print n
		on=0	
		temp.append(m)
		aligned_bounds.append(temp[:])
		temp=[]	
	if not string[m]=='-':
		n=n+1
print bounds		
print aligned_bounds
string=''.join(string)
print ''.join(string[(aligned_bounds[0][0]-1):(aligned_bounds[0][1])])
partition1=[]
partition2=[]
partition3=[]
partition_junk=[]
gene=''.join(string[(aligned_bounds[0][0]-1):(aligned_bounds[0][1])])

gene_start=aligned_bounds[0][0]-1
print gene_start
# n=0
# for m in xrange(len(gene)):
# 	if not gene[m]=='-':
# 		n=n+1
# 	else:
# 		partition_junk.append(m+gene_start)
# 		continue	
# 	if n%3==1:
# 		partition1.append(m+gene_start)
# 	if n%3==2:
# 		partition2.append(m+gene_start)			
# 	if n%3==0:
# 		partition3.append(m+gene_start)

print aligned_bounds

for i in xrange(len(aligned_bounds)):
	gene=''.join(string[(aligned_bounds[i][0]-1):(aligned_bounds[i][1])])
	gene_start=aligned_bounds[i][0]-1
	print gene_start
	n=0
	for m in xrange(len(gene)):
		if not gene[m]=='-':
			n=n+1
		else:
			partition_junk.append(m+gene_start)
			continue	
		if n%3==1:
			partition1.append(m+gene_start)
		if n%3==2:
			partition2.append(m+gene_start)			
		if n%3==0:
			partition3.append(m+gene_start)	
print len(partition1)
print len(partition2)
print len(partition3)

def unique(x):
	new=[]
	for member in x:
		if not member in new:
			new.append(member)
	return new

# print partition1
# for member in partition1:
# 	if partition1.count(member)==2:
# 		print member	



file=open(outputstring,'r').readlines()

shortoutputstring=outputstring.replace('.nex','')
position1=open(shortoutputstring+'_position1.nex','w')
position2=open(shortoutputstring+'_position2.nex','w')
position3=open(shortoutputstring+'_position3.nex','w')
positionjunk=open(shortoutputstring+'_positionjunk.nex','w')
othergenes=open(shortoutputstring+'_othergenes.nex','w')
dloop=open(shortoutputstring+'_dloop.nex','w')

	
for m in range(0,6):
	if m==3:
		othergenes.write(file[m].replace('16786','4792'))
		dloop.write(file[m].replace('16786','1279'))
		position1.write(file[m].replace('16786','3573'))
		position2.write(file[m].replace('16786','3568'))
		position3.write(file[m].replace('16786','3574'))
	else:
		othergenes.write(file[m])
		dloop.write(file[m])
		position1.write(file[m])
		position2.write(file[m])
		position3.write(file[m])
for m in range(6,len(file)):
	try:
		member=list(file[m].split('\t')[3])
		name=file[m].split('\t')[1]
		print len(member)
		stringposition1=''
		stringposition2=''
		stringposition3=''
		stringpositionjunk=''
		stringothergenes=''
		stringdloop=''
		for n in range(637,16144):
			if n in partition1:
				stringposition1=stringposition1+member[n]
			elif n in partition2:
				stringposition2=stringposition2+member[n]
			elif n in partition3:	
				stringposition3=stringposition3+member[n]
			elif n in partition_junk:	
				stringposition3=stringposition3+member[n]				
			else:	
				stringothergenes=stringothergenes+member[n]
		for n in range(0,637):
			stringdloop=stringdloop+member[n]
		for n in range(16144,len(member)):
			stringdloop=stringdloop+member[n]
		print 'mole'
		print len(stringothergenes)
		print len(stringdloop)
		print len(stringposition1)
		print len(stringposition2)
		print len(stringposition3)
		othergenes.write(name+'\t'+stringothergenes+'\r')
		dloop.write(name+'\t'+stringdloop+'\r')	
		position1.write(name+'\t'+stringposition1+'\r')	
		position2.write(name+'\t'+stringposition2+'\r')
		position3.write(name+'\t'+stringposition3+'\r')
	except:
		print 'yikes'
		print file[m]	
othergenes.write('End;')
dloop.write('End;')	
position1.write('End;')	
position2.write('End;')	
position3.write('End;')	


# A PART TO WRITE AGES AT THE END OF THE DLOOP FILE


file1=open('results.txt','r').readlines()
file2=open('results_new.txt','r').readlines()
ancient1=open('ancient_check_results.txt','r').readlines()
ancient2=open('ancient.txt','r').readlines()

def find_age(taxon):
	coefficient=1000000
	taxon=taxon.split('_')[0]
	result='0'
	id=''
	for m in xrange(len(file1)):
# 		print m
		try:
			a=file[m].split('_')[0]
			if a==taxon:
				id=file2[m]
		except:
			pass		
	if not id=='':		
		for m in xrange(len(ancient1)):
			if id in ancient1[m]:
				result=ancient1[m].split('\t')[1]
				if '+/-' in result:
					result=result.split(' +/- ')
					result=str(float(result[0])+float(result[1]))+'-'+str(float(result[0])-float(result[1]))
	else:				
		for m in xrange(len(ancient2)-1):
			if taxon+'_' in ancient2[m]:
# 				print 'yes'
				x=ancient2[m+1].split(' ')
				x[-1]=x[-1].replace('/note=','')
				x[-1]=x[-1].replace('"','')
				x[-1]=x[-1].replace('\n','')
				if 'AD' in x[-1]:
					result=x[-1].replace('AD','')
					result=result.split('-')
# 					print result
					for n in xrange(len(result)):
						result[n]=str(2000-float(result[n]))
					result='-'.join(result)
				if 'BP' in x[-1]:
					result=x[-1].replace('cal','')
					result=result.replace('BP','')
					result=result.split('-')
# 					print result
					for n in xrange(2):
						result[n]=str(float(result[n]))
					result='-'.join(result)
	result=result.split('-')

# 	if len(result)==2:
# 		result2=[np.mean([result[0],result[1]]),np.mean([result[0],result[1]])-result[1]/1.95]
# 		result=result2
	if len(result)==1:
		result2=[float(result[0])+10,float(result[0])]
		result=result2	
	for m in xrange(len(result)):
		result[m]=float(result[m])/coefficient
			
	return result	
	

file=open('output6.nex','r').readlines()



taxa=[]
for m in range(6,len(file)):
	try:
		print m
		print file[m].split('\t')[1]
		taxa.append(file[m].split('\t')[1])
	except:
		pass
taxa=sorted(taxa)
print taxa

things=[dloop]
for member in things:

	member.write('\r')
	member.write('begin assumptions;\r')

	for m in xrange(len(taxa)):
		if not find_age(taxa[m])[0]==0:
			age=find_age(taxa[m])
	# 		dloop.write('calibrate '+taxa[m]+' = normal('+str(age[0])+','+str(age[1])+')\r')
			member.write('calibrate '+taxa[m]+' = uniform('+str(age[1])+','+str(age[0])+')\r')

		if m==len(taxa)-1:
			member.write('end;')
	
