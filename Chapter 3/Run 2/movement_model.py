from __future__ import division
import numpy as np
import random
import math
import matplotlib.pyplot as plt
import time
import datetime as dt
import geocoder
from math import radians, cos, sin, asin, sqrt, degrees, acos
import scipy.stats
def log(x):
	return math.log(x)
def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False
def rep(x,y):
	new=[]
	for m in xrange(y):
		new.append(x)
	return new	        
def copy(x):
	new=[]
	for member in x:
		new.append(member)
	return new	
def copy_list(x):
	new=[]
	for member in x:
		new.append(x[:])
	return new		
def strlist(x):
	new=[]
	for member in x:
		new.append(str(member))
	return new
def numlist(x):
	new=[]
	for member in x:
		new.append(float(member))
	return new
def sigmoid(x):
	y=1/(1+np.exp(-x))
	return y
def time_between(a,b):
	exp=[int(x) for x in (a.split(',')[0]).split('/')]
# 	print exp
	a=dt.datetime(exp[0],exp[1],exp[2],exp[3],exp[4],exp[5])
	exp=[int(x) for x in (b.split(',')[0]).split('/')]
# 	print exp
	b=dt.datetime(exp[0],exp[1],exp[2],exp[3],exp[4],exp[5])
	return (b-a).total_seconds()	

def unique(seq, idfun=None): 
   # order preserving
   if idfun is None:
       def idfun(x): return x
   seen = {}
   result = []
   for item in seq:
       marker = idfun(item)
       # in old Python versions:
       # if seen.has_key(marker)
       # but in new ones:
       if marker in seen: continue
       seen[marker] = 1
       result.append(item)
   return result	
def haversine(lat1, lon1, lat2, lon2):
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




file=open('movements2000.csv','r').readlines()
movement_list=[[float(x) for x in y.split('\t')] for y in file if not y==file[0]]


m=0


directions=[]
for m in xrange(len(movement_list)):
	if movement_list[m][0]<movement_list[m][2]:
		NS='N'
	else:
		NS='S'
	if movement_list[m][1]<movement_list[m][3]:
		EW='E'
	else:
		EW='W'
	adjacent=abs(movement_list[m][1]-movement_list[m][3])
	hypotenuse=((abs(movement_list[m][1]-movement_list[m][3])**2)+(abs(movement_list[m][0]-movement_list[m][2])**2))**0.5
# 	print adjacent/hypotenuse
	angle=math.degrees(math.acos(adjacent/hypotenuse))
	if NS == 'N':
		if EW == 'E':
			true_angle=angle
		if EW == 'W':
			true_angle=180-angle
	if NS == 'S':
		if EW == 'E':
			true_angle=360-angle
		if EW == 'W':
			true_angle=180+angle		
	movement_list[m]=movement_list[m]+[NS,EW,angle,true_angle]


def analyse_movements(lat_bounds,long_bounds,NSdirection=None,EWdirection=None,angle_bounds=[None,None],destination=False,distance_threshold=None,true_angle_bounds=[None,None],true_angle_adjustment=0):
	if destination==False:
		return len([x[8] for x in movement_list if x[0]>lat_bounds[0] and x[0]<lat_bounds[1] and x[1]>long_bounds[0] and x[1]<long_bounds[1] and (x[7]== NSdirection or NSdirection == None) and (x[9]<angle_bounds[1] and x[9]>angle_bounds[0] or angle_bounds==[None,None]) and (x[8]==EWdirection or EWdirection==None) and (x[6]>distance_threshold or distance_threshold==None) and ((x[10]-true_angle_adjustment)% 360 >true_angle_bounds[0] and (x[10]-true_angle_adjustment)% 360<true_angle_bounds[1] or true_angle_bounds==[None,None])])
	if destination==True:
		return len([x[8] for x in movement_list if x[2]>lat_bounds[0] and x[2]<lat_bounds[1] and x[3]>long_bounds[0] and x[3]<long_bounds[1] and (x[7]== NSdirection or NSdirection == None) and (x[9]<angle_bounds[1] and x[9]>angle_bounds[0] or angle_bounds==[None,None]) and (x[8]==EWdirection or EWdirection==None) and (x[6]>distance_threshold or distance_threshold==None) and ((x[10]-true_angle_adjustment)% 360 >true_angle_bounds[0] and (x[10]-true_angle_adjustment)% 360<true_angle_bounds[1] or true_angle_bounds==[None,None])])


def produce_kml(long_size_limit,destination_value,NSdirection_value=None,EWdirection_value=None,angle_bounds_value=[None,None],colour='green',filename='squares.kml',distance_threshold=None,threshold=0.67,true_angle_bounds_value=[None,None],true_angle_adjustment_value=0):
	test_square=open('test_square.kml','r').read()
	squares=open(filename,'w')
	squares.write('<kml xmlns="http://www.opengis.net/kml/2.2">\n<Document>\n')
	for y in range(-30,60):
		for x in range(0,160):
			lat_size=5
			long_size=5
			if analyse_movements([y,y+lat_size],[x,x+long_size])<30:
				while analyse_movements([y,y+lat_size],[x,x+long_size],destination=destination_value)<30 and long_size<long_size_limit:
					lat_size=lat_size+1
					long_size=long_size+1
			if not analyse_movements([y,y+lat_size],[x,x+long_size],destination=destination_value)<30:		
				moo=analyse_movements([y,y+lat_size],[x,x+long_size],angle_bounds=angle_bounds_value,NSdirection=NSdirection_value,EWdirection=EWdirection_value,destination=destination_value,distance_threshold=distance_threshold,true_angle_bounds=true_angle_bounds_value,true_angle_adjustment=true_angle_adjustment_value)/analyse_movements([y,y+lat_size],[x,x+long_size],destination=destination_value)
				if moo>threshold:
					print y,x,lat_size,long_size
					print moo
					coordinates=str(x)+','+str(y)+'\t'+str(x)+','+str(y+lat_size)+'\t'+str(x+long_size)+','+str(y+lat_size)+'\t'+str(x+long_size)+','+str(y)+'\t'+str(x)+','+str(y)
					if colour=='red':
						test_square=test_square.replace('<color>5014F032</color>','<color>641400FA</color>')
					squares.write(test_square.replace('[coordinates]',coordinates))
	squares.write('</Document>\n</kml>')

produce_kml(20,False,None,'W',[0,90])	
produce_kml(20,False,None,'E',[0,90],'red','squares2.kml')	
produce_kml(20,False,'N',None,[0,90],'green','squares3.kml')
produce_kml(20,False,'S',None,[0,90],'red','squares4.kml')
produce_kml(20,False,None,None,[0,45],'green','squares5.kml')
produce_kml(20,False,None,None,[46,90],'green','squares6.kml')
produce_kml(20,False,None,None,[0,25],'green','squares7.kml',0.67)
produce_kml(20,False,None,None,[26,90],'green','squares8.kml',0.67)
produce_kml(20,False,None,None,[0,90],'green','squares9.kml',0.5)
produce_kml(20,False,None,None,[None,None],'green','squares10.kml',true_angle_bounds_value=[0,50],threshold=0.33,true_angle_adjustment_value=-25)
produce_kml(20,False,None,None,[None,None],'green','squares11.kml',true_angle_bounds_value=[90,140],threshold=0.33,true_angle_adjustment_value=-25)


