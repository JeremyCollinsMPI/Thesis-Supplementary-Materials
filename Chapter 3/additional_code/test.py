# import reverse_geocoder 
import json
# import country_converter as coco
from collections import Counter
import os
import logging
from time import sleep

from pyvirtualdisplay import Display
from selenium import webdriver

logging.getLogger().setLevel(logging.INFO)

def test1():
  coordinates = (-37.81, 144.96), (31.76, 35.21)
  print(reverse_geocoder.search(coordinates))
  
def test2():
  file = open('1.xml', 'r').readlines()[0:2168]
  output_file = open('just_sequences.txt', 'w')
  for line in file:
    output_file.write(line)

def test3():
  file = open('just_sequences.txt', 'r').readlines()
  print(len(file))

def test4():
  file = open('just_sequences.txt', 'r').readlines()
  for line in file:
    if 'sequence' in line:
      pass
    else:
      print('no')

def test5():
  file = open('just_sequences.txt', 'r').readlines()
  location_file = open('location_file.json', 'w')
  result = []
  i = 0
  for line in file:
    thing = line.split('taxon')[0]  
    location = {}
    thing_split = thing.split('_')
    location['lat'] = float(thing_split[2])
    location['long'] = float(thing_split[3].split('"')[0])
    coordinates = (location['lat'], location['long'])
    temp = reverse_geocoder.search(coordinates)
    result = result + temp
    print(i)
    i = i + 1
  json.dump(result, location_file)

def test6():

  some_names = ['United Rep. of Tanzania', 'DE', 'Cape Verde', '788', 'Burma', 'COG',
              'Iran (Islamic Republic of)', 'Korea, Republic of',
              "Dem. People's Rep. of Korea"]
  standard_names = coco.convert(names=some_names, to='name_short')
  print(standard_names)    

def test7():
  '''
  get the country name for each i
  have a list with each country name
  then just turn it into list of country names with number of occurrences
  '''
  locations = json.load(open('location_file.json', 'r'))
  country_codes = [x['cc'] for x in locations]
  print(country_codes)
  standard_names = coco.convert(names=country_codes, to='name_short')
  print(standard_names)
  c = Counter(standard_names)
  print(c)
  print(len(country_codes))

def test8():
  file = open('just_sequences.txt', 'r').readlines()
  result = []
  i = 0
  for line in file:
    thing = line.split('taxon')[0]  
    location = {}
    thing_split = thing.split('_')
    print(thing)
    location['lat'] = float(thing_split[2])
    location['long'] = float(thing_split[3].split('"')[0])
    coordinates = (location['lat'], location['long'])
    temp = reverse_geocoder.search(coordinates)
    print(temp)
#     result = result + temp
    print(i)
    i = i + 1
#   json.dump(result, location_file)
    '''
    longitudes are wrong
    '''

def transform_long(long):
  if long < -30:
    long = long + 360
  long = long - 150
  return long

def reverse_transform_long(long):
  long = long + 150
  if long > 180:
    long = long - 360
  return long

def test9():
  file = open('just_sequences.txt', 'r').readlines()
  location_file = open('location_file.json', 'w')
  result = []
  i = 0
  for line in file:
    thing = line.split('taxon')[0]  
    location = {}
    thing_split = thing.split('_')
    print(thing)
    location['lat'] = float(thing_split[2])
    location['long'] = float(thing_split[3].split('"')[0])
    location['long'] = location['long'] + 180 
    #they were transformed for the phylogeographic analysis
    #the transformation is still wrong
    '''
    the original transformation was:
    if long < -30:
      long = long + 360
    long = long - 150
    
    if float(list1[2])<-30:
				list1[2]=float(list1[2])+360
    
    '''
    coordinates = (location['lat'], location['long'])
    temp = reverse_geocoder.search(coordinates)
    print(temp)
    result = result + temp
    print(i)
    i = i + 1
  json.dump(result, location_file)

def test10():
  long = 121
  print(reverse_transform_long(long))
  long = -117
  print(reverse_transform_long(long))
  long = -23
  print(reverse_transform_long(long))
  
  '''
  result so far
  121 - > -89
  -117 -> 33
  
  
  
  '''
  
  
  '''
  examples
  35.0077519_112.907123 in the US, oklahoma
  1.373333_-117.709725 uganda
  37.566535_-23.0220308 korea
  
  korea: seems to be long - 150
  uganda: seems to be long - 150
  in the us: about -97.  so seems to be long + 210
  
  
  '''

def test11():
  file = open('just_sequences.txt', 'r').readlines()
  location_file = open('location_file.json', 'w')
  result = []
  i = 0
  for line in file:
    thing = line.split('taxon')[0]  
    location = {}
    thing_split = thing.split('_')
    print(thing)
    location['lat'] = float(thing_split[2])
    location['long'] = float(thing_split[3].split('"')[0])
    location['long'] = reverse_transform_long(location['long'])
    coordinates = (location['lat'], location['long'])
    temp = reverse_geocoder.search(coordinates)
    print(temp)
    result = result + temp
    print(i)
    i = i + 1
  json.dump(result, location_file)


def test12():
  '''
  get the country name for each i
  have a list with each country name
  then just turn it into list of country names with number of occurrences
  '''
  locations = json.load(open('location_file.json', 'r'))
  country_codes = [x['cc'] for x in locations]
  print(country_codes)
  standard_names = coco.convert(names=country_codes, to='name_short')
  print(standard_names)
  c = Counter(standard_names)
  print(c)
  print(len(country_codes))
  output_file = open('table.txt', 'w')
  for key in c:
    output_file.write(key + '\t' + str(c[key]) + '\n')
    
  '''
  then want to write lines to make into a table
  just make tsv, then can copy into pages
  '''
 
def test13():
  file = open('just_sequences.txt', 'r').readlines()
  output_file = open('sequences.fasta', 'w')
  for line in file[50:100]:
    name = line.split('taxon="')[1].split('"')[0]
    sequence = line.split('value="')[1].split('"')[0]
    output_file.write('>' + name + '\n')
    output_file.write(sequence + '\n')

def test14():
  file = open('output5b.tree', 'r').readlines()
  tree = file[-2]
#   print(tree)
  taxa_names = file[2169:4329]
  taxa = {}
  for taxa_name in taxa_names:
    taxa[taxa_name.split(' ')[0].replace('\t', '')] = taxa_name.split(' ')[1].replace('\n', '')
  print(tree)

def submit_sequence(sequence, context):
  return {'result': 'success'}

def get_result_from_page_source(context):
  page_source = context['page_source']
  result = page_source.split('<p><b><big>Best mtDNA Haplogroup Matches:</big></b></p>')[1]
  result = result.split('<p><b>Defining Markers for haplogroup ')[1]
  result = result.split(':</b>')[0]
  return {'result': result}

def test15():
  BASE_URL = 'https://dna.jameslick.com/mthap/'
  display = Display(visible=0, size=(800, 600))
  display.start()
  chrome_options = webdriver.ChromeOptions()
  chrome_options.add_argument('--no-sandbox')

  chrome_options.add_experimental_option('prefs', {
        'download.default_directory': os.getcwd(),
        'download.prompt_for_download': False,
  })

  browser = webdriver.Chrome(chrome_options=chrome_options)

  browser.get(BASE_URL)
  print(browser.page_source)
  file_button = browser.find_element_by_name("upload_file")
  print(file_button)
  file_button.send_keys(os.getcwd()+"/example.fasta")
  submit_button = browser.find_element_by_name("upload")
  submit_button.click()
  sleep(120)
  result = get_result_from_page_source({'page_source': browser.page_source})
  print(result)
  browser.quit()
  display.stop()  


def get_sequences(context):
  file = open('sequences.fasta', 'r').readlines()
  result = []
  for i in range(0, len(file), 2):
    result.append([file[i].replace('\n', ''), file[i+1].replace('\n', '')])
  return {'result': result}

def save_sequence_to_example_fasta(context):
  file = open('example.fasta', 'w')
  file.write(context['label'] + '\n')
  file.write(context['sequence'])
  return {'result': 'success'}

def test16():
  BASE_URL = 'https://dna.jameslick.com/mthap/'
  display = Display(visible=0, size=(800, 600))
  display.start()
  chrome_options = webdriver.ChromeOptions()
  chrome_options.add_argument('--no-sandbox')

  chrome_options.add_experimental_option('prefs', {
        'download.default_directory': os.getcwd(),
        'download.prompt_for_download': False,
  })

  browser = webdriver.Chrome(chrome_options=chrome_options)
  get_sequences_context = {}
  sequences = get_sequences(get_sequences_context)['result']
  sequences = sequences[0:3]
#   print('here1')
  for sequence in sequences:
    save_sequence_to_example_fasta({'sequence': sequence[1], 'label': sequence[0]})
    browser.get(BASE_URL)
#     print('here2')
#     print(browser.page_source)
    file_button = browser.find_element_by_name("upload_file")
#     print(file_button)
    file_button.send_keys(os.getcwd()+"/example.fasta")
    submit_button = browser.find_element_by_name("upload")
    submit_button.click()
    sleep(120)
    result = get_result_from_page_source({'page_source': browser.page_source})
    print(result)

def test17():
  '''
  go through just_sequences.txt
  you extract the sequences one at a time and submit them
  you will have a haplogroup
  you then want to make a dictionary of the sequence names and their haplogroups
  
  '''
  file = open('just_sequences.txt', 'r').readlines()
  output_file = open('sequences.fasta', 'w')
  for line in file:
    name = line.split('taxon="')[1].split('"')[0]
    sequence = line.split('value="')[1].split('"')[0]
    output_file.write('>' + name + '\n')
    output_file.write(sequence + '\n')
  BASE_URL = 'https://dna.jameslick.com/mthap/'
  display = Display(visible=0, size=(800, 600))
  display.start()
  chrome_options = webdriver.ChromeOptions()
  chrome_options.add_argument('--no-sandbox')

  chrome_options.add_experimental_option('prefs', {
        'download.default_directory': os.getcwd(),
        'download.prompt_for_download': False,
  })
  browser = webdriver.Chrome(chrome_options=chrome_options)
  get_sequences_context = {}
  sequences = get_sequences(get_sequences_context)['result']
  result_dict = {}
  for i in range(len(sequences)):
    sequence = sequences[i]
    label = sequence[0]
    print(label)
    save_sequence_to_example_fasta({'sequence': sequence[1], 'label': sequence[0]})
    browser.get(BASE_URL)
    file_button = browser.find_element_by_name("upload_file")
    file_button.send_keys(os.getcwd()+"/example.fasta")
    submit_button = browser.find_element_by_name("upload")
    submit_button.click()
    sleep(120)
    result = get_result_from_page_source({'page_source': browser.page_source})
    hap = result['result']
    result_dict[label] = hap
    json.dump(result_dict, open('result_dict.json', 'w'))


def test18():
  '''
  go through just_sequences.txt
  you extract the sequences one at a time and submit them
  you will have a haplogroup
  you then want to make a dictionary of the sequence names and their haplogroups
  you then go through the consensus tree and replace each sequence name with the sequence name plus _ plus the haplogroup
  '''
  tree_file = open('output5b.tree', 'r').read()
  output = tree_file
  result_dict = json.load(open('result_dict.json', 'r'))
  for label in result_dict.keys():
    to_replace_with = result_dict[label]
    to_replace_with = to_replace_with.replace('(', '_')
    to_replace_with = to_replace_with.replace(')', '_')
    to_replace_with = to_replace_with.replace(' ', '_')
    output = output.replace(label.replace('>', ''), label.replace('>', '') + '_' + to_replace_with)
  output_file = open('output_tree_file.tree', 'w')
  output_file.write(output)    
  
test18()




