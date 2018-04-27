import sys
thing=sys.argv[1]

colour='blue'
possible_colours=['blue','cyan','purple','red','yellow','orange','green']
colours=[]
possible_families=[]
file=open('subtree'+str(thing)+'.tree','r').readlines()
file2=open('data.csv','r').readlines()
output=open('locations'+str(thing)+'.txt','w')
locations=[]
families=[]
line=''
for m in xrange(len(file)):
	if 'tree_1' in file[m]:
		line=file[m]
		break
line2=line.split('[&height=')
for m in xrange(len(line2)-1):
	x=line2[m]
	y=x[len(x)-3:len(x)]
	y=''.join(y)
	
	if y.isalpha():
# 		print y
		for n in range(1,len(file2)):
			if file2[n].split(',')[4]==y:
				location=str(file2[n].split(',')[2])+','+str(file2[n].split(',')[1])
				locations.append(location)
				family=file2[n].split(',')[26*6+12]
				if not family in possible_families:
					possible_families.append(family)
				colours.append(possible_colours[min(len(possible_colours)-1,possible_families.index(family))])
print colours				
output.write('\n'.join(locations))
output.close()
input=open('locations'+str(thing)+'.txt','r').readlines()
output=open('subtree'+str(thing)+'.kml','w')
template=open('template_beginning.xml','r').readlines()
for m in xrange(len(template)):
	output.write(template[m])
for m in range(len(input)):
	print m
	output.write('<Placemark>\n\t<styleUrl>'+colours[m]+'</styleUrl>\n\t<Point>\n\t<coordinates>'+input[m].replace('\n','')+',0\t</coordinates>\n\t</Point>\n</Placemark>')
output.write('</Document> </kml>')