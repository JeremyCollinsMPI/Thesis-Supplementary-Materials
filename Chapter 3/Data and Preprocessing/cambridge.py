x=open('31485 documents.gb','r').readlines()
string='ancient'
string='Improved calibration of the human mitochondrial clock using ancient'
string='KC521454'
string='Llamas,B., Fehren-Schmitz,L.,'
string='Cambridge reference sequence'
string='NC_012920 AC_000021'


# m=0
# text_length=500
# number=0
# while m<len(x)-text_length:
# # for m in xrange(len(x)-30):
# 	if string in x[m]:
# 		print 'BEGIN'
# 		number=number+1
# 		print number
# 		for n in xrange(0,text_length):
# 			print x[m+n]
# 		m=m+text_length	
# 	else:
# 		m=m+1	




# titles=[]
# m=0
# text_length=500
# number=0
# while m<len(x)-text_length:
# # for m in xrange(len(x)-30):
# 	if string in x[m] and not x[m] in titles:
# 		print x[m]
# 		titles.append(x[m])
# # 		print 'BEGIN'
# # 		number=number+1
# # 		print number
# # 		for n in xrange(0,text_length):
# # 			print x[m+n]
# # 		m=m+text_length	
# # 	else:
# # 		m=m+1
# for m in titles:
# 	print m


# results=open('results2.txt','r').readlines()
# string=open('string.txt','r').read()
# string=string.replace('\n','')
# string=string.replace('\t','')
# 
# # string='gttt atgtagctta cctcctcaaa601 gcaatacact gaaaatgttt agacgggctc acatcacccc ataaacaaat aggtttggtc661 ctagcctttc tattagctct tagtaagatt acacatgcaa gcatccccgt tccagtgagt721 tcaccctcta aatcaccacg atcaaaagga acaagcatca agcacgcagc aatgcagctc'
# 
# string2=''
# for m in string:
# 	if m in ['a','c','t','g']:
# 		string2=string2+m
# print string2		
# for m in xrange(len(results)):	
# 	if string2 in results[m]:
# 		print results[m-1]

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
# 
# 
# 
# input=[]
# result=[]
# taxa=[]
# z=''
# for m in range(0,30000):
# 
# 	member=file[m]
# 	if '>' in member:
# 		if len(result)>0:
# 			result.append(z)
# 			input.append(result)
# 			strlen=len(z)
# 			z=''
# 			taxa.append(member)
# # 		member=member.replace('\n','')
# 		member=member.replace('>','')
# 		member=member.replace(',','')
# 		result=[member]
# 	else:
# 		seq=list(member)
# 		for member3 in seq:
# 			if member3 in ['a','c','t','g','-']:
# 				z=z+member3
# 	if m==len(file)-1:
# 		if len(result)>0:
# 			result.append(z)
# 			input.append(result)
# print input
# 
# for m in xrange(len(input)):
# 	print 'BEGIN'
# 	print input[m][0]
# 	print m
# 	sequence=input[m][1]
# 	print ''.join(sequence[(aligned_bounds[0][0]-1):(aligned_bounds[0][1])])
# 	print [sequence[x] for x in partition_junk]

	
	
# genes=[]
# for m in xrange(len(bounds)):
# 	genes.append(''.join(string[(int(bounds[m][0])-1):(int(bounds[m][1]))]))
# 
# input=[]
# result=[]
# taxa=[]
# z=''
# # for m in range(0,1694149):
# for m in range(0,30000):
# 
# 	member=file[m]
# 	if '>' in member:
# 		if len(result)>0:
# 			result.append(z)
# 			input.append(result)
# 			strlen=len(z)
# 			z=''
# 			taxa.append(member)
# # 		member=member.replace('\n','')
# 		member=member.replace('>','')
# 		member=member.replace(',','')
# 		result=[member]
# 	else:
# 		seq=list(member)
# 		for member3 in seq:
# 			if member3 in ['a','c','t','g','-']:
# 				z=z+member3
# 	if m==len(file)-1:
# 		if len(result)>0:
# 			result.append(z)
# 			input.append(result)
# 
# print input
# print len(input)
# 
# y=input[0][1]
# print [f for f in y if not f=='-']
# 
# gene='gatcacaggtc'
# for m in xrange(len(input)):
# 	if gene in input[m][1].replace('-',''):
# 		

		  