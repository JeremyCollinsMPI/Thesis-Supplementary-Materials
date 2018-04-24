import subprocess
from selenium import webdriver
import time
from selenium.webdriver.common.by import By
import dateutil.parser
import ast
import unicodedata
from selenium.webdriver.common.action_chains import ActionChains

file = open('raw.txt','r').readlines()
driver=webdriver.Chrome("./chromedriver")





def lookUp(string):
	searchString = string.replace(' ','+')
	driver.get("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q="+
	
for m in xrange(len(file)):
	
	
	
	