import xml.etree.ElementTree as ET
from scipy.cluster.hierarchy import dendrogram, linkage, to_tree
from matplotlib import pyplot as plt
from scipy.spatial.distance import squareform
import pandas as pd

import numpy as np

X = np.array([[5,3],
    [10,15],
    [15,12],
    [24,10],
    [30,30],
    [85,70],
    [71,80],
    [60,78],
    [70,55],
    [80,91],])

def get_miss_dist(x,y):
    return np.nanmean(abs(x - y))

def get_miss_dist_mat(dat):
    Npat = dat.shape[0]
    dist = np.ndarray(shape=(Npat,Npat))
    dist.fill(0)
    for ix in range(0,Npat):
        x = dat[ix,]
        if ix >0:
            for iy in range(0,ix):
                y = dat[iy,]
                dist[ix,iy] = get_miss_dist(x,y)
                dist[iy,ix] = dist[ix,iy]
    return dist


def get_values(context):
    tree = ET.parse(context['filename'])
    root = tree.getroot()
    root[14][0].get('value') # multistate values
    root[12][0].get('value') # binary values
    result = []
    taxa = []
    for i in range(len(root[14])):
        multistate_value_string = root[14][i].get('value')
        binary_state_value_string = root[12][i].get('value')
        x = multistate_value_string.split(',') + list(binary_state_value_string)
        x = [np.nan if y == '?' else int(y) for y in x]
        result.append(x)
        taxa.append(root[14][i].get('taxon'))
    return {'result': result, 'taxa': taxa}
    
def normalise(context):
    x = context['array']
    y = x / np.sqrt(np.nansum(np.square(x), axis=0))
    return {'result': y}

def load_data(context):
    get_values_result = get_values({'filename': context['filename']})
    unnormalised_array = get_values_result['result']
    labels = get_values_result['taxa']
    normalised_array = normalise({'array': unnormalised_array})['result']
    return {'result': normalised_array, 'labels': labels}

def getNewick(node, newick, parentdist, leaf_names):
    '''
    From https://stackoverflow.com/questions/28222179/save-dendrogram-to-newick-format
    Example, if you have linked and the list of leaves leaf_names: 
    tree = to_tree(linked, False)
    newick_string = getNewick(tree, "", tree.dist, leaf_names)
    '''
    if node.is_leaf():
        return "%s:%.5f%s" % (leaf_names[node.id], parentdist - node.dist, newick)
    else:
        if len(newick) > 0:
            newick = "):%.5f%s" % (parentdist - node.dist, newick)
        else:
            newick = ");"
        newick = getNewick(node.get_left(), newick, node.dist, leaf_names)
        newick = getNewick(node.get_right(), ",%s" % (newick), node.dist, leaf_names)
        newick = "(%s" % (newick)
        return newick

def make_newick_string(context):
    linked = context['linked']
    leaf_names = context['leaf_names']
    tree = to_tree(linked, False)
    newick_string = getNewick(tree, "", tree.dist, leaf_names)
    return {'result': newick_string}

def make_dendrogram(context):
    if context.get('use_distances'):
        dat = context['array']
        dist_mat = get_miss_dist_mat(dat)
        condens_dist = squareform(dist_mat)
        linked = linkage(condens_dist, method='average')
    else:
        linked = linkage(context['array'], context['method'])
    plt.figure(figsize=(300, 300))
    if context['plot_using_dendrogram']:
        dendrogram(linked,
            orientation='top',
            labels=context['labels'],
            distance_sort='descending',
            show_leaf_counts=True)
    if context['save_plot']:
        plt.savefig(context['save_plot_filename'])
    if context.get('export_to_newick'):
        newick_string = make_newick_string({'linked': linked, 'leaf_names': context['labels']})['result']
        with open(context['newick_string_file'], 'w') as file:
            file.write(newick_string)

def find_language_name_and_family(context):
    iso = context['iso']
    if not 'df' in context.keys():
        if not context.get('df_filename'):
            df = pd.read_csv('phonotactics.csv', index_col='ISO code', na_values=['?'], keep_default_na=False)
        else:
            df = pd.read_csv(context['df_filename'], index_col='ISO code', na_values=['?'], keep_default_na=False)
    else:
        df = context['df']
    return {'language': df['Language'][iso], 'family': df['Language family'][iso], 'iso': iso}

def make_labels_have_language_and_family_name(context):
    labels = context['labels']
    df = pd.read_csv('phonotactics.csv', index_col='ISO code', na_values=['?'], keep_default_na=False)
    label_dicts = [find_language_name_and_family({'iso': x,
        'df': df}) for x in labels]
    result = [x['language'] + '_' + x['iso'] + '_' + x['family'] for x in label_dicts]
    return {'result': result}

def convert_names_in_bayesian_phylogeny(context):
    file = open('1.tree', 'r').readlines()
    labels = file[4:696]
    labels = [x.replace(' ', '').replace('\n', '').replace('\t', '') for x in labels]
    labels = make_labels_have_language_and_family_name({'labels': labels})['result']
    output_filename = '1_with_names.tree'
    for i in range(4, 696):
        new_name = labels[i-4]
        file[i] = file[i] + '[&!name="' + new_name + '"]'
    output = '\n'.join(file)
    with open(output_filename, 'w') as output_file:
        output_file.write(output)
    
def main(context):
    load_data_result = load_data({'filename': 'data.xml'})
    array = load_data_result['result']
    labels = load_data_result['labels']
    labels = make_labels_have_language_and_family_name({'labels': labels})['result']
    make_dendrogram(context | {'array': array, 'labels': labels})    

if __name__ == "__main__":
    context = {'save_plot': False,'method': 'average', 'use_distances': True, 
        'save_plot_filename': '3.png', 'export_to_newick': True, 'newick_string_file': 'tree.tree',
        'plot_using_dendrogram': False}
    main(context)

    labelList = range(1, 11)
    make_dendrogram({'save_plot': True, 'array': X, 'method': 'average', 'labels': labelList,
        'use_distances': True, 'save_plot_filename': '2.png'})

    convert_names_in_bayesian_phylogeny({})
    
    
    