from __future__ import division
import sys

def is_a_number(x):
	try:
		y=float(x)
		return 1
	except:
		return 0
			
file1=open(sys.argv[1],'r').readlines()
file2=open(sys.argv[2],'w')
name=sys.argv[3]
# file1=open('/Users/jeremycollins/Documents/Phylogeography/MTDNA/Analyses To Publish/World/Run 6/output4b.tree','r').readlines()
# file2=open('/Users/jeremycollins/Documents/Phylogeography/MTDNA/Analyses To Publish/World/Run 6/output4bconverted.tree','w')

for memberz in file1:
	if not 'tree TREE1' in memberz:
		file2.write(memberz)
	else:
		member=memberz.split('locations'+name+'2=')
		string=member[0]
		for m in range(1,len(member)):
			string=string+'locations'+name+'2='
			member2=list(member[m])
			string2=''
			done=0
			for n in range(0,len(member2)):
				
				if done==1:
					string=string+member2[n]
				if done==0:
					if is_a_number(string2+member2[n])==0 and not string2+member2[n]=='-':
						print string2
						print member[m]
						long=float(string2)
						print long	
						long2=long+150
						if long2>180:
							long2=long2-360
						print long2	
						done=1
						string=string+str(long2)+member2[n]
					else:
						string2=string2+member2[n]	
		print string
		print 'jeznoe'
		print memberz
		file2.write(string)
					
print string					
			