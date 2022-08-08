import pandas as pd
from math import radians
from math import sin, cos, asin, sqrt
import statistics

def load_coordinates_df(context):
    df = pd.read_csv('coordinates.txt', sep='\t', header=0)
    return {'result': df}

def load_demography_df(context):
    df = pd.read_csv('demography.txt', sep='\t')
    return {'result': df}

def haversine(lon1, lat1, lon2, lat2):
    """
    http://evoling.net/code/haversine/
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

def find_distance(context):
    return {'result': haversine(context['long1'], context['lat1'],
        context['long2'], context['lat2'])}

def find_nearest_town(context):
    distances = []
    towns = []
    for index, row in context['demography_df'].iterrows():
        distances.append(find_distance({'lat1': context['lat'], 'long1': context['long'],
            'lat2': row['Latitude'], 'long2': row['Longitude']})['result'])
        towns.append(row['Name'])
    minimum = min(distances)
    town = towns[distances.index(minimum)]
    return {'result': town, 'distance': minimum}
    
def find_distances_to_nearest_towns(context):
    result = []
    towns = []
    locations = []
    for index, row in context['coordinates_df'].iterrows():
        find_nearest_town_result = find_nearest_town(context | {'lat': row['Latitude'],
            'long': row['Longitude']})
        result.append(find_nearest_town_result['distance'])
        towns.append(find_nearest_town_result['result'])
        locations.append(row['Name'])
    return {'result': result, 'towns': towns, 'locations': locations}

def main(context):
    coordinates_df = load_coordinates_df(context)['result']
    demography_df = load_demography_df(context)['result']
    find_distances_to_nearest_towns_result = find_distances_to_nearest_towns({'coordinates_df': coordinates_df,
        'demography_df': demography_df})
    distances = find_distances_to_nearest_towns_result['result']
    print(sorted(distances))
    print(statistics.mean(distances))
    print('---')
    for i in range(len(find_distances_to_nearest_towns_result['locations'])):
        print(find_distances_to_nearest_towns_result['locations'][i] + '\t' +
            find_distances_to_nearest_towns_result['towns'][i] + '\t' +
            str(find_distances_to_nearest_towns_result['result'][i]))
    print(find_distances_to_nearest_towns_result['locations'])
    print(find_distances_to_nearest_towns_result['towns'])
    print(find_distances_to_nearest_towns_result['result'])
    return {'result': distances}
    

if __name__ == "__main__":
    main({})