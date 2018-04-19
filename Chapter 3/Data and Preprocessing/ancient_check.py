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
file2=open('ancient_check.txt','w')
for member2 in file:

	if 'ancient' in member2 or 'Ancient' in member2 or ' date' in member2 or ' Date' in member2 or 'remains' in member2 or ' old ' in member2 or 'KT760574' in member2 or 'JX893364' in member2 or 'JX893365' in member2 or 'JX893366' in member2 or 'JX893367' in member2:

		file2.write(member2)
		file2.write('\n')
