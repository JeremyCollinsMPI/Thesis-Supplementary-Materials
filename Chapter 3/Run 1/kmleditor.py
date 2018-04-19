from __future__ import division
import sys
import math
import numpy as np
from math import radians, cos, sin, asin, sqrt
def unique(x):
	new=[]
	for member in x:
		if not member in new:
			new.append(member)
	return new
def haversine(lon1, lat1, lon2, lat2):
    """
    Calculate the great circle distance between two points 
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians 
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])

    # haversine formula 
    dlon = lon2 - lon1 
    dlat = lat2 - lat1 
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a)) 
    r = 6371 # Radius of earth in kilometers. Use 3956 for miles
    return c * r



def sort_dates(list):
	positive_dates=[]
	negative_dates=[]	
	for x in list:
		if not x[0]=='-':
			positive_dates.append(x)
		if x[0]=='-':
			negative_dates.append(x)		
	return sorted(negative_dates,reverse=True)+sorted(positive_dates)

# def find_dates(file):
# 	dates=[]
# 
# 	for m in xrange(len(file)):
# 		if '<TimeSpan>' in file[m-1]:
# 			x=file[m].split('<begin>')[1]
# 			x=x.split('<')[0]
# 			dates.append(x)
# 	dates=sort_dates(dates)
# 	return dates

# def reverse_date(line):
# 	if '<begin>' in line:
# 		x=line.split('<begin>')[1]
# 		x=x.split('<')[0]
# 		index1=dates.index(x)
# 		date2=dates[(len(dates)-1)-index1]
# 		return line.replace(x,date2)
# 	else:
# 		return line
# 
# def reverse_coordinates(line):
# 	if '<coordinates>' in line:
# 		x=line.split('<coordinates>')[1]
# 		x=x.split(' ')
# 		x[1]=x[1].split('\t')[0]
# 		new=[x[0].split(',')[0:3],x[1].split(',')[0:3]]
# 		return '\t\t\t\t\t\t<coordinates>'+','.join(new[1])+' '+','.join(new[0])+'\t\t\t\t\t\t</coordinates>\n'
# 	else:
# 		return line

threshold='-28000'
upper_threshold='-18000'

def check_date(line):
	
	if '<begin>' in line:
		x=line.split('<begin>')[1]
		x=x.split('<')[0]
		x=x.split('-')
		if x[0]=='':
			x='-'+x[1]
		else:
			x=x[0]

		print float(x)
		if float(x)<float(threshold) or float(x)>float(upper_threshold):
			return False
		else:
			return True	
	else:
		return True


# def collect_paths(mafia):
# 	mafia=open(mafia,'r').readlines()
# 	new=[]
# 	for m in xrange(len(mafia)):
# 		if '<coordinates>' in mafia[m]:
# # 			print mafia[m]
# 			coordinates=mafia[m].replace('\t','')
# 			coordinates=coordinates.replace('\n','')
# 			coordinates=coordinates.replace('<coordinates>','')
# 			coordinates=coordinates.replace('</coordinates>','')
# 			coordinates=coordinates.split(' ')
# 			coordinates=[x.split(',') for x in coordinates]
# 			new.append(coordinates)
# 	return new


def collect_paths(mafia):
	mafia=open(mafia,'r').readlines()
	new=[]
	for m in xrange(len(mafia)):
		if '<Folder>' in mafia[m] and not 'Branches' in mafia[m+1]:
			coordinates=mafia[m+1].replace('\t','')
			coordinates=coordinates.replace('\n','')
			coordinates=coordinates.replace('<name>','')
			coordinates=coordinates.replace('</name>','')
			coordinates=coordinates.split(':')
			coordinates=[x.split(',') for x in coordinates]
			new.append(coordinates)
	return new



def find_daughters(paths,m):
	result=[]
	for n in xrange(len(paths)):
		if paths[m][1]==paths[n][0]:
			result.append(n)
	return result

def find_descendants(paths,m):
	result=[]
	daughters=find_daughters(paths,m)
	result=result+daughters
# 	print 'daughters'
# 	print daughters
	for n in xrange(len(daughters)):
		result=result+find_descendants(paths,daughters[n])
	return result
			
def find_maximal_descendants(paths,m):
	result=[]
	daughters=find_daughters(paths,m)
	if len(daughters)==0:
		result=[m]
		return result
	if len(daughters)==2:
		result=result+[m]
# 	print m
# 	print 'daughters'
# 	print daughters
	
	for n in xrange(len(daughters)):
		
		result=result+find_maximal_descendants(paths,daughters[n])
	result=unique(result)
	return result


# def find_maximal_descendants(paths,m):
# 	result=[]
# 	daughters=find_daughters(paths,m)
# 	print m
# 	print 'daughters'
# 	print daughters
# 	if len(daughters)==0:
# 		return result
# 	
# 	else:
# 		for m in xrange(len(daughters)):
# 			if find_daughters(paths,daughters[m])==[]:
# 				result.append(daughters[m])
# 			else:
# 				result.append(find_maximal_descendants(paths,daughters[m]))
# 	return result
		
# 	if len(daughters)==2:
# 		result=result+[m]
# 	print 'daughters'
# 	print daughters
# 	
# 	for n in xrange(len(daughters)):
# 		
# 		result=result+find_maximal_descendants(paths,daughters[n])
# 	return result



# def write_file():
# 	file=open(sys.argv[1],'r').readlines()
# 	file2=open(sys.argv[2],'w')	
# 	
# 	print len(file)
# 	m=0
# 
# 	while m<len(file):
# 		try:
# 			print m
# # 			print file[m]
# 			if '<Folder>' in file[m] and '<name>Polygons</name>' in file[m+1]:
# 				print 'mate'
# 				file2.write('\t</Document>\r</kml>')
# 				break
# 			if '<Placemark>' in file[m]:
# 				print 'jez'
# 				if check_date(file[m+2]):
# 					file2.write(''.join(file[m:m+12]))
# 					m=m+12
# 				else:
# 					print file[m+2]
# 					m=m+12
# 			else:
# 				file2.write(file[m])	
# 				m=m+1
# 		except:
# 			m=m+1
# 			print 'mole'
# 
# 			pass
# 	file2.close()

def write_file():
	file=open(sys.argv[1],'r').readlines()
	file2=open(sys.argv[2],'w')	
	
	print len(file)
	m=0

	while m<len(file):
		try:
			print m
# 			print file[m]
			if '<Folder>' in file[m] and '<name>Polygons</name>' in file[m+1]:
				print 'mate'
				file2.write('\t</Document>\r</kml>')
				break
			elif '<Folder>' in file[m]:
				print 'jez'
				if check_date(file[m+4]):
					file2.write(file[m])
					m=m+1
				else:
					m=m+1
					while not '</Folder>' in file[m-1]:
						m=m+1
			else:
				file2.write(file[m])	
				m=m+1
		except:
			m=m+1
			print 'mole'

			pass
	file2.close()



	

def write_paths(filename,output_filename):
	paths=collect_paths(filename)
	output_file=open(output_filename,'w')
	done=[]
	maximal_paths=[]
	for m in xrange(len(paths)):
		print m
# 		print done
		if not m in done:
# 			print 'no'
			x=find_descendants(paths,m)
			done=done+x
			done=sorted(done)
			y=find_maximal_descendants(paths,m)
# 			print y
			maximal_paths.append([m]+y)
# 	print paths[47399]
# 	print paths[47400]
# 	print paths[47500]
	
	output_file=open(output_filename,'w')
	for m in xrange(len(maximal_paths)):
		current=maximal_paths[m]
		temp=','.join(paths[maximal_paths[m][0]][0])
		for n in range(1,len(current)):
# 			print temp
			temp=temp+'\t'+','.join(paths[maximal_paths[m][n]][1])
			output_file.write(temp)
			if not n==len(current)-1:
				output_file.write('\t')
		if not m==len(maximal_paths)-1:
			output_file.write('\n')	


def analyse_distances(filename):
	input=open(filename,'r').readlines()
	results=[]
	for m in xrange(len(input)):
		line=input[m].split('\t')
		origin=(line[0]).split(',')
		origin=[float(x) for x in origin]
		for n in range(1,len(line)):
			point=line[n].split(',')
			point=[float(x) for x in point]
			distance=haversine(origin[0],origin[1],point[0],point[1])
			results.append(distance)
	return results		


def read_paths(filename):
	results=[]
	input=open(filename,'r').readlines()
	for m in xrange(len(input)):
		x=input[m].split('\t')
		for n in range(1,len(x)):
			results=results+[[x[0],x[n]]]
	return results


def reduced_paths(filename,distance_threshold):
	paths=read_paths(filename)
	distances=analyse_distances(filename)
	results=[]
	for m in xrange(len(paths)):
		if distances[m]>distance_threshold:
			results.append(paths[m])
	return results

# def check_path(line,paths_file):
# 	paths_file=open(paths_file,'r').readlines()
# 	try:
# 		line=line.replace('<name>','')
# 		line=line.replace('</name>','')
# 		line=line.replace('\t','')
# 		line=line.replace('\n','')
# 		coordinates=line.split('\
				

def write_filtered_file(file,file2,paths_file):
	file=open(file,'r').readlines()
	file2=open(file2,'w')	
	
	print len(file)
	m=0

	while m<len(file):
		try:
			print m
# 			print file[m]
			if '<Folder>' in file[m] and '<name>Polygons</name>' in file[m+1]:
				print 'mate'
				file2.write('\t</Document>\r</kml>')
				break
			elif '<Folder>' in file[m]:
				print 'jez'
				if check_path(file[m+1],paths_file):
					file2.write(file[m])
					m=m+1
				else:
					m=m+1
					while not '</Folder>' in file[m-1]:
						m=m+1
			else:
				file2.write(file[m])	
				m=m+1
		except:
			m=m+1
			print 'mole'

			pass
	file2.close()



def main():
# 	write_file()
# 	write_paths('output3.kml','maximal_paths.txt')
	paths=read_paths('maximal_paths.txt')
	print paths[0]
# 	print reduced_paths('maximal_paths.txt',5000)
# 	print read_paths('maximal_paths.txt')
# 	write_filtered_fi/le(
	
	
if __name__=="__main__":
	main()

		