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

dataFile  = open('dataframe.txt','r').readlines()
demographyFile = open('demography.txt','r').readlines()
data = {}
demography = {}
columns = dataFile[0].split('\t')
columns[-1] = columns[-1].replace('\n','')

demography = {}
for m in range(1,len(demographyFile)):
	line = demographyFile[m].split('\t')
	name = line[0]
	demography[name] = {}
	demography[name]['lon'] = float(line[2])
	demography[name]['lat'] = float(line[3])
	demography[name]['ratio'] = float(line[4])

def findDemographicRatio(lon, lat, number = 1):
	lon = float(lon)
	lat = float(lat)
	distances = []
	ratios = []
	for member in demography:
		distance = haversine(lat, lon, demography[member]['lat'], demography[member]['lon'])
		distances.append(distance)
		ratios.append(demography[member]['ratio'])
	sorted_distances = sorted(distances)
	total = 0
	for i in xrange(number):
		total = total + ratios[distances.index(sorted_distances[i])]
	return total/number

for m in range(1,len(dataFile)):
	line = dataFile[m].split('\t')
	name = line[0]
	dict = {}
	for c in range(1,len(columns)):
		print line[c]
		dict[columns[c]] = line[c].replace('\n','')
	data[name] = dict
	data[name]['ratio'] = findDemographicRatio(data[name]['Longitude'], data[name]['Latitude'],1)

class model1:
	def findValues(valueName):
		values = []
		for member in data:
			print member
			values.append(data[member][valueName])
		return values
	eye_of_the_day_values = findValues('Eye of the day')
	eye_of_the_day_prob1 = len([x for x in eye_of_the_day_values if x == '1'])/len(eye_of_the_day_values)
	eye_of_the_day_likelihood = (eye_of_the_day_prob1 ** len([x for x in eye_of_the_day_values if x == '1'])) * ((1-eye_of_the_day_prob1)** len([x for x in eye_of_the_day_values if x == '1']))

class model2:
	def findValues(valueName):
		values = []
		for member in data:
			print member
			values.append([data[member][valueName], data[member]['ratio']])
		return values
	def find(values, greaterThan = True, threshold = 1):
		if greaterThan:
			return [x for x in values if x[1] > threshold]
		else:
			return [x for x in values if x[1] < threshold]
	
	columns = ['Eye of the day', 'Classifiers', 'SV/VS', 'Demonstratives', 'Number of eat verbs', 'Presence of bull imagery', 'Numerals','Containing Tai numerals under 10', 'Glottolog name']
	s1 = []
	s2 = []
	column = columns[0]
	s1 = find(findValues(column), True, 1)
	s2 = find(findValues(column), False, 1)
	def findRatios(value1, value2, values):
		number1 = sum([x[1] for x in values if x[0] == value1])
		number2 = sum([x[1] for x in values if x[0] == value2])
		print number1
		print number2

	def permutationTest(value1, value2, values, samples = 10000):
		total = 0
		number1 = sum([x[1] for x in values if x[0] == value1 or x[0] == 'SVS'])
		number2 = sum([x[1] for x in values if x[0] == value2])
		allValues = [x[1] for x in values]
		length1 = len([x for x in values if x[0] == value1])
		length2 = len([x for x in values if x[0] == value2])
		print allValues
		for i in xrange(samples):
			sample = random.sample(allValues, len(values))
			
			sample1 = sample[0:length1]
			sample2 = sample[length1:len(sample)]
			sampleNumber1 = sum(sample1)
			sampleNumber2 = sum(sample2)
			if sampleNumber1 > number1:
				total = total + 1
				print sampleNumber1
				print number1
		return number1/length1, number2/length2, total/samples

	def bullPermutationTest(value1, value2, values, samples = 10000):
		total = 0
		number1 = sum([x[1] for x in values if x[0] == value1])
		number2 = sum([x[1] for x in values if x[0] == value2])
		allValues = [x[1] for x in values]
		length1 = len([x for x in values if x[0] == value1])
		length2 = len([x for x in values if x[0] == value2])
		print allValues
		for i in xrange(samples):
			sample = random.sample(allValues, len(values))
			
			sample1 = sample[0:length1]
			sample2 = sample[length1:len(sample)]
			sampleNumber1 = sum(sample1)
			sampleNumber2 = sum(sample2)
			if sampleNumber1 <= number1:
				total = total + 1
		return number1/length1, number2/length2, total/samples			
	eyeOfTheDayPermutation = permutationTest('2','1',findValues('Eye of the day'))
	SVPermutation = permutationTest('SV','VS',findValues('SV/VS'))
	DemonstrativesPermutation = permutationTest('2','1',findValues('Demonstratives'))
	bullImageryPermutation = bullPermutationTest('1','0',findValues('Presence of bull imagery'))
	numeralsPermutation = permutationTest('1','0',findValues("Containing Tai numerals under 10"))

print data
print model2.bullImageryPermutation
print model2.DemonstrativesPermutation
print model2.SVPermutation
print model2.eyeOfTheDayPermutation
print model2.numeralsPermutation

total = 0
number = 0
for member in data.keys():
	if data[member]['Glottolog name'] == 'Awa':
		total = total + data[member]['ratio']
		number = number + 1
print total/number



