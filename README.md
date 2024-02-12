Data and code used in Jeremy Collins's PhD thesis _Linguistic Areas and Human Prehistory_ (2024). 
The following section provides directories for individual chapters, and a summary of the files. 

**Chapter 2**  
1.R	: R script for analysing the relationship between tone and humidity, and phylogeographic analyses.  
africatree.txt, asiatree.txt, glottologtree.txt: tree files for use in the phylogeographic analysis.  
epicentersafrica.txt, epicentersasia.txt, epicentersmexico.txt, epicenterspapua.txt, epicentersworld.txt: data on number of tones in different regions (Africa, Asia, Mexico, Papua New Guinea, and globally) from the World Phonotactics Database (see note on World Phonotactics Database below).  

**Chapter 3**  
1.log, 1.tree, 1.trees, 1.xml, 1.xml.state: files relating to the run of the phylogenetic analysis of languages without clade constraints.  These are the log file of states, the consensus tree, the trees, the xml file, and the xml.state files respectively.   
2.log, 2.tree, 2.trees, 2.xml, 2.xml.state: files relating to the clade-constrained phylogenetic analysis.   
data.csv: data from the World Phontactics Database (Donohue et al. 2013) in csv format (see note on World Phonotactics Database below).   
nameeditor.py: a python script for editing names in tree files.   
point_generator.py: a python script for generating maps from a set of coordinates.   
template_beginning.xml: a xml script for use in Google Earth for generating maps.   
additional_code/main.py: script for producing a dendrogram using the UPGMA algorithm    

**Chapter 4**  
analyses.txt: a transcription of data used in the chapter.  
model.py: main script for running permutation tests  
dataframe.txt: dataframe of features  
demography.txt: demographic data for towns  
additional_code/main.py: script for finding distance of languages to nearest towns   
An additional repository https://github.com/JeremyCollinsMPI/palaungic-data-cldf contains files in cross-linguistic data format (CLDF 1.2, Forkel et al. 2018): see the README in this repository for an updated description of the files.  

**Chapter 5**  
/Data and Preprocessing:   
31485 documents.gb: mtDNA sequences for 31,845 individuals downloaded from GenBank (Benson et al. 2013).   
all-aligned.tar.gz: a file showing aligned mtDNA sequences used in this chapter.   
all.txt: a file of unaligned mtDNA sequences used in this chapter.   
ancient.py, ancient.txt, ancient_check.py, ancient_check.txt, ancient_check_results.txt, ancient_taxa.txt: files relating to giving time calibrations to ancient DNA samples. 
cambridge.py, cambridge.txt: files relating to partitioning sequences.   
countries_new.txt: a file containing names of locations of sequences.   
longconverter.py, longitude_hack.py: a file for preprocessing longitudes before the phylogeographic analysis.   
mtdnaanalysis.py, pruner.py: a script for sampling sequences for the phylogenetic analysis and preparing the nexus file. 
native_names.txt: names of Native American mtDNA sequences included.   

/Run 1:   
1.xml, 1.xml.state, output.tree, output.trees, output.kml: files relating to the time-calibrated analysis of mtDNA sequences.   
ages.txt: time calibrations for sequences.   
eurasianlanguages.kml: a kml file for languages in Eurasia.   
kmleditor.py: a script for producing kml files used in the chapter.   
southeastasianlanguages.kml: a kml file for languages in Southeast Asia.   

/Run 2:   
1.xml, 1.xml.state, beast.log, output.kml, output.tree, output.trees: files relating to the phylogeographic analysis of 2000 sequences.   
migrations.r: R script for analysing migration routes.   
migrations7.r: R script for predicting language similarity from number of migrations between languages.   
movement_model.py: python script for processing migration route data.   
movements2000.csv: csv file of reconstructed migration routes.   
squares.kml, squares10.kml, squares11.kml, squares2.kml, squares3.kml, squares4.kml, squares5.kml	, squares6.kml, squares7.kml, squares8.kml, squares9.kml: kml files showing tendencies in direction of migration in Eurasia and Africa.  
additional_code/test.py: script for submitting fasta sequences to MtHap to find the haplogroups  

**Note on the World Phonotactics Database**

Data from the World Phonotactics Database (Donohue, M., R.Hetherington, J.McElvenny & V.Dawson.  2013. World phonotactics database.  Department of Linguistics, The Australian National University, accessed 15/6/2015.) was used in Chapters 2 and 3.  The database was originally online at http://phonotactics.anu.edu.au, but has since been taken offline.  The data is still publicly available on the site as archived by Web Archive (e.g. https://web.archive.org/web/20190608215845/http://phonotactics.anu.edu.au/features.php), and a csv of the same data used is provided in this repository (Chapter 3/data.csv).
