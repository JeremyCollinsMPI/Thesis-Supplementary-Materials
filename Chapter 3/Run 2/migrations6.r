install.packages('ape')
library(ape)
install.packages('maps')
library(maps)
install.packages('OutbreakTools')
library(OutbreakTools)
install.packages('SDMTools')
library(SDMTools)
install.packages('geosphere')
library(geosphere)
install.packages('binr')
library(binr)
install.packages('adephylo')
library(adephylo)
install.packages('ade4')
library(ade4)
install.packages('fitdistrplus')
library(fitdistrplus)

movements = read.csv('/Users/jeremycollins/Documents/Thesis/Code/Chapter 3/Run 2/movements2000.csv', header=TRUE, sep = '\t')
file=read.csv('/Users/jeremycollins/Documents/Phonotactics/New Workflow/Beastling/data.csv',header=T,sep=',',stringsAsFactors = F)
#this file does not load:
languagetree=read.annotated.nexus('/Users/jeremycollins/Documents/Phonotactics/New Workflow/Run 53/2.tree')
constrained_language_tree = read.annotated.nexus('/Users/jeremycollins/Documents/Phonotactics/New Workflow/Run 15/outputc.tree')
language_tree = languagetree

#geographical analysis with constrained eurasian language tree
constrained_nearestlanguage=function(lat,long, in_eurasian_tree = FALSE){
  file2=file[!file$Ancient.language==1,]
  if(in_eurasian_tree == TRUE){
    file2 = file2[file2$ISO %in% constrained_language_tree$tip.label, ]
  }
  cands=c()
  threshold=0
  while(length(cands)<5){
    threshold=threshold+5
    for(m in 1:length(file2[,1])){
      if(abs(file2[m,2]-lat)<threshold & abs(file2[m,3]-long)<threshold){
        cands=append(cands,m)
      }
    }
    cands=unique(cands)
  }
  dists=c()
  for(m in 1:length(cands)){
    dists=append(dists,distHaversine(c(long,lat),c(file2[cands[m],3],file2[cands[m],2])))
    
  }
  return(file2[cands[which(dists==min(dists))[1]],1])
}
constrained_movements_eurasia = c()
languages1 = c()
languages2 = c()
for (m in 1:length(movements[,1])){
  language1 = constrained_nearestlanguage(movements[m,1], movements[m, 2], in_eurasian_tree = TRUE)
  language2 = constrained_nearestlanguage(movements[m,3], movements[m, 4], in_eurasian_tree = TRUE)
  if (file$World.macro.region[which(file$Language == language1)[1]] == 'Eurasia' & file$World.macro.region[which(file$Language == language2)[1]] == 'Eurasia'){
    constrained_movements_eurasia = append(constrained_movements_eurasia, m)
    languages1 = append(languages1, language1)
    languages2 = append(languages2, language2)
  }
}
findISO = function(language_name){
  return (file$ISO[which(file$Language == language_name)[1]])
}
constrained_eurasia_movements = movements[constrained_movements_eurasia,]
constrained_eurasia_movements$languages1 = languages1
constrained_eurasia_movements$languages2 = languages2
constrained_eurasia_movements$languages1ISO = lapply(constrained_eurasia_movements$languages1, findISO)
constrained_eurasia_movements$languages2ISO = lapply(constrained_eurasia_movements$languages2, findISO)
geographical_distance_between_languages = function(language1, language2) {
  row1 = which(file$Language == language1)[1]
  row2 = which(file$Language == language2)[1]
  return (distHaversine(c(file$Longitude[row1],file$Latitude[row1]),
                        c(file$Longitude[row2],file$Latitude[row2]))/1000)
}
constrained_languages_geographical_distance_matrix = c()
for(m in 1:length(constrained_language_tree$tip.label)){
  for(n in 1:length(constrained_language_tree$tip.label)){
    language1 = file$Language[which(file$ISO == constrained_language_tree$tip.label[m])[1]]
    language2 = file$Language[which(file$ISO == constrained_language_tree$tip.label[n])[1]]
    constrained_languages_geographical_distance_matrix = append(constrained_languages_geographical_distance_matrix, 
                                                                geographical_distance_between_languages(
                                                                  language1, language2))
  }
}
constrained_languages_geographical_distance_matrix = matrix(constrained_languages_geographical_distance_matrix, ncol = length(constrained_language_tree$tip.label))
constrained_languages_geographical_distance_matrix = t(constrained_languages_geographical_distance_matrix)
lookup_geographical_distance_between_languages = function(iso1, iso2){
  number1 = which(constrained_language_tree$tip.label == iso1)
  number2 = which(constrained_language_tree$tip.label == iso2)
  return(constrained_languages_geographical_distance_matrix[number1, number2])
}
language_geographical_distances = c()
for ( m in 1:length(constrained_eurasia_movements$languages1ISO)){
  language_geographical_distances = append(language_geographical_distances,
                                           lookup_geographical_distance_between_languages(constrained_eurasia_movements$languages1ISO[m], constrained_eurasia_movements$languages2ISO[m]))
}
constrained_eurasia_movements$language_geographical_distances = language_geographical_distances
count_of_distance = function(distance, distance_histogram) {
  interval = which(distance_histogram$breaks > distance)[1] - 1
  return ( distance_histogram$counts[interval]  )
}
produce_geographical_distances_within_eurasia = function() {
  matrix_vector = c()
  for (m in 1:length(constrained_eurasia_movements$languages1ISO)) {
    iso = constrained_eurasia_movements$languages1ISO[m]
    iso_number = which(constrained_language_tree$tip.label == iso)
    row = constrained_languages_geographical_distance_matrix[iso_number,]
    matrix_vector = append(matrix_vector, row)
  }
  matrix = t(matrix(matrix_vector, length(row), length(constrained_eurasia_movements$languages1ISO)))
  return(matrix)
}
geographical_distances_within_eurasia = produce_geographical_distances_within_eurasia()
calculate_geographic_distance_counts_all_dirty = function(histogram) {
  totals=c()
  count_matrix_vector = c()
  x = histogram
  for (m in 1:length(constrained_eurasia_movements$languages1ISO)) {
    counts = c()
    row = geographical_distances_within_eurasia[m,]
    for (n in 1:length(row)) {
      counts = append(counts,
                      count_of_distance(row[n], x))
    }
    counts[which(is.na(counts))] = 0
    count_matrix_vector = append(count_matrix_vector, counts)
  }
  count_matrix_vector[which(is.na(count_matrix_vector))] = 0
  count_matrix = t(matrix(count_matrix_vector, length(row), length(constrained_eurasia_movements$languages1ISO)))
  return(count_matrix)
}
calculate_geographic_distance_counts_one_dirty = function (previous_count_matrix, histogram, count_number1){
  x = histogram
  current_count_matrix = previous_count_matrix
  interval1 = c(x$breaks[count_number1], x$breaks[count_number1+1])
  if(is.na(interval1[2])){
    interval1[2] = 100000000000
  }
  for(m in 1:length(geographical_distances_within_eurasia[,1])) {
    for (n in 1:length(geographical_distances_within_eurasia[1,])) {
      y = geographical_distances_within_eurasia[m,n]
      if((y >= interval1[1] & y < interval1[2]) ) {
        replacement = count_of_distance(y, x)
        if(is.na(replacement)) {
          replacement = 0
        }
        current_count_matrix[m,n] = replacement
      }
    }
  }
  return(current_count_matrix)
}
calculate_normalised_probability_of_geographical_distance = function (m, count_matrix, histogram) {
  iso = constrained_eurasia_movements$languages1ISO[m]
  iso_number = which(constrained_language_tree$tip.label == iso)
  counts = count_matrix[iso_number,]
  return(count_of_distance(constrained_eurasia_movements$language_geographical_distances[m], histogram)/sum(counts))
}
calculate_likelihoods_of_geographical_distance_histogram = function(histogram, count_matrix) {
  probabilities = c()
  for(m in 1:length(constrained_eurasia_movements$languages1ISO)) {
    probability = calculate_normalised_probability_of_geographical_distance(m, count_matrix, histogram)
    probabilities = append(probabilities, probability)
    print(probabilities[1:10])
  }
  return(probabilities)
}
geographical_distance_hist = hist(constrained_eurasia_movements$language_geographical_distances, breaks = 1000, plot = TRUE)
current_geographical_hist = geographical_distance_hist
previous_geographical_count_matrix=calculate_geographic_distance_counts_all_dirty(current_geographical_hist)
geographical_likelihoods = calculate_likelihoods_of_geographical_distance_histogram(current, previous_geographical_count_matrix)
current_likelihood = sum(sapply(geographical_likelihoods, log))
toChooseFrom = which(!current_geographical_hist$counts == 0)
current = current_geographical_hist
for(number1 in toChooseFrom[1:423]){
  positiveDone = FALSE
  negativeDone = FALSE
  while(positiveDone == FALSE | negativeDone == FALSE) {
    new = current
    if(positiveDone == FALSE){
      number2 = 1
    }
    else{
      if(negativeDone == FALSE){
        number2 = -1
      }
    }
    new$counts[number1] = max(new$counts[number1] + number2,1)
    if(!(new$counts[number1]==1 & current$counts[number1]==1)){
      current_geographical_count_matrix=calculate_geographic_distance_counts_one_dirty(previous_geographical_count_matrix, new, number1)
      geographical_likelihoods = calculate_likelihoods_of_geographical_distance_histogram(new, current_geographical_count_matrix)
      new_likelihood = sum(sapply(geographical_likelihoods, log))
      if(new_likelihood > current_likelihood) {
        current_likelihood = new_likelihood
        current = new
        previous_geographical_count_matrix = current_geographical_count_matrix
      }
      else{
        if(number2 == 1){
          positiveDone = TRUE
        }
        if(number2 == -1){
          negativeDone = TRUE
        }
      }
    }
    print(number1)
    print(number2)
    print(positiveDone)
    print(negativeDone)
    print(current_likelihood)
    print(current$counts[number1])
    current_geographical_hist = current
  }
}


------
  # linguistic distances
constrained_languages_distance_matrix = as.matrix(distTips(constrained_language_tree))
language_distance = function(language1, language2, matrix, tree){
  tip1 = which(tree$tip.label == language1)[1]
  tip2 = which(tree$tip.label == language2)[1]
  distance = matrix[tip1,tip2]
  return(distance)
}
constrained_language_distances = c()
for (m in 1:length(constrained_eurasia_movements[,1])){
  constrained_language_distances = append(constrained_language_distances, language_distance(constrained_eurasia_movements$languages1ISO[m], constrained_eurasia_movements$languages2ISO[m], constrained_language_distances_matrix, constrained_language_tree))
}
constrained_eurasia_movements$constrained_language_distances = constrained_language_distances
produce_constrained_language_distances_within_eurasia = function() {
  matrix_vector = c()
  for (m in 1:length(constrained_eurasia_movements$languages1ISO)) {
    iso = constrained_eurasia_movements$languages1ISO[m]
    row = constrained_language_tree$tip.label
    for (n in 1:length(row)) {
      d = language_distance(iso, row[n], constrained_language_distances_matrix, constrained_language_tree)
      matrix_vector = append(matrix_vector, d)
    }
  }
  matrix = t(matrix(matrix_vector))
  return(matrix)
}
constrained_language_distances_within_eurasia = produce_constrained_language_distances_within_eurasia()
count_of_distance = function(distance, distance_histogram) {
  interval = which(distance_histogram$breaks > distance)[1] - 1
  return ( distance_histogram$counts[interval]  )
}
calculate_constrained_language_distance_counts_all_dirty = function(histogram) {
  totals=c()
  count_matrix_vector = c()
  x = histogram
  for (m in 1:length(constrained_eurasia_movements$languages1ISO)) {
    counts = c()
    row = constrained_language_distances_within_eurasia[m,]
    for (n in 1:length(row)) {
      counts = append(counts,
                      count_of_distance(row[n], x))
    }
    counts[which(is.na(counts))] = 0
    count_matrix_vector = append(count_matrix_vector, counts)
  }
  count_matrix_vector[which(is.na(count_matrix_vector))] = 0
  count_matrix = t(matrix(count_matrix_vector, length(row), length(constrained_eurasia_movements$languages1ISO)))
  return(count_matrix)
}
calculate_constrained_language_distance_counts_one_dirty = function (previous_count_matrix, histogram, count_number1){
  x = histogram
  current_count_matrix = previous_count_matrix
  interval1 = c(x$breaks[count_number1], x$breaks[count_number1+1])
  if(is.na(interval1[2])){
    interval1[2] = 100000000000
  }
  for(m in 1:length(constrained_language_distances_within_eurasia[,1])) {
    for (n in 1:length(constrained_language_distances_within_eurasia[1,])) {
      y = constrained_language_distances_within_eurasia[m,n]
      if((y >= interval1[1] & y < interval1[2]) ) {
        replacement = count_of_distance(y, x)
        if(is.na(replacement)) {
          replacement = 0
        }
        current_count_matrix[m,n] = replacement
      }
    }
  }
  return(current_count_matrix)
}
calculate_normalised_probability_of_constrained_language_distance = function (m, count_matrix, histogram) {
  iso = constrained_eurasia_movements$languages1ISO[m]
  iso_number = which(constrained_language_tree$tip.label == iso)
  counts = count_matrix[iso_number,]
  return(count_of_distance(constrained_eurasia_movements$language_geographical_distances[m], histogram)/sum(counts))
}
calculate_likelihoods_of_constrained_language_distance_histogram = function(histogram, count_matrix) {
  probabilities = c()
  for(m in 1:length(constrained_eurasia_movements$languages1ISO)) {
    probability = calculate_normalised_probability_of_constrained_language_distance(m, count_matrix, histogram)
    probabilities = append(probabilities, probability)
  }
  return(probabilities)
}
constrained_language_distance_hist = hist(constrained_eurasia_movements$constrained_language_distances, breaks = 1000, plot = TRUE)
current = constrained_language_distance_hist
previous_constrained_language_distance_count_matrix=calculate_constrained_language_distance_counts_all_dirty(current)
constrained_language_likelihoods = calculate_likelihoods_of_constrained_language_distance_histogram(current, previous_constrained_language_distance_count_matrix)
current_likelihood = sum(sapply(constrained_language_likelihoods, log))
toChooseFrom = which(!current$counts == 0)
for(number1 in toChooseFrom[1:423]){
  positiveDone = FALSE
  negativeDone = FALSE
  while(positiveDone == FALSE | negativeDone == FALSE) {
    new = current
    if(positiveDone == FALSE){
      number2 = 1
    }
    else{
      if(negativeDone == FALSE){
        number2 = -1
      }
    }
    new$counts[number1] = max(new$counts[number1] + number2,1)
    if(!(new$counts[number1]==1 & current$counts[number1]==1)){
      current_constrained_language_distance_count_matrix=calculate_constrained_language_distance_counts_one_dirty(previous_constrained_language_distance_count_matrix, new, number1)
      constrained_language_likelihoods = calculate_likelihoods_of_constrained_language_distance_histogram(new, current_constrained_language_distance_count_matrix)
      new_likelihood = sum(sapply(geographical_likelihoods, log))
      if(new_likelihood > current_likelihood) {
        current_likelihood = new_likelihood
        current = new
        previous_geographical_count_matrix = current_geographical_count_matrix
      }
      else{
        if(number2 == 1){
          positiveDone = TRUE
        }
        if(number2 == -1){
          negativeDone = TRUE
        }
      }
    }
    print(number1)
    print(number2)
    print(positiveDone)
    print(negativeDone)
    print(current_likelihood)
    print(current$counts[number1])
    current_constrained_language_distance_hist = current
  }
}

----

#analysis using constrained languages distances
  
matrix1 = as.matrix(distTips(constrained_language_tree))
matrix2 = constrained_languages_geographical_distance_matrix
matrix3 = matrix(0,length(constrained_language_tree$tip.label),
                        length(constrained_language_tree$tip.label))
for(m in 1:length(constrained_eurasia_movements$languages1)){
  index1 = which(constrained_language_tree$tip.label == constrained_eurasia_movements$languages1ISO[[m]])
  index2 = which(constrained_language_tree$tip.label == constrained_eurasia_movements$languages2ISO[[m]])
  matrix3[index1, index2] = matrix3[index1, index2] + 1
}

matrix4 = matrix(0,length(constrained_language_tree$tip.label),
                 length(constrained_language_tree$tip.label))
for(m in 1:length(constrained_language_tree$tip.label)){
  total = sum(matrix3[m,])
  for(n in 1:length(constrained_language_tree$tip.label)){
   matrix4[m,n] = matrix3[m,n]/total 
  }
}

matrix_vector1 = as.vector(matrix1)
matrix_vector2 = as.vector(matrix2)
matrix_vector3 = as.vector(t(matrix3))
matrix_vector4 = as.vector(t(matrix4))

#function for turning error into probability
error_to_probability = function(x, mean, sd){
  a = pnorm(x, mean, sd)
  b = pnorm(x+0.1, mean, sd)
  return(b-a)
}

# a model for geography
onlyUsingNonZeros=FALSE
if(onlyUsingNonZeros == TRUE){
  model1=lm(matrix_vector1[!matrix_vector3 == 0] ~ poly(matrix_vector2[!matrix_vector3 == 0], 13, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model1$coefficients)){
    quadratic = quadratic + (model1$coefficients[x] * (matrix_vector2[!matrix_vector3 == 0]**(x-1)))
  }
} else {
  model1=lm(matrix_vector1 ~ poly(matrix_vector2, 13, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model1$coefficients)){
    quadratic = quadratic + (model1$coefficients[x] * (matrix_vector2**(x-1)))
  }
}
#plot(matrix_vector2, quadratic, xlab = "Geographic distance (km)", ylab = "Predicted linguistic distance")
#a correction which makes all identical languages have predicted distance of 0
quadratic[matrix_vector2==0] = 0

#finding residuals from geographic model
residual_matrix_vector=matrix_vector1-quadratic
model1_sd = sd(residual_matrix_vector)
model2_sd = model1_sd
#a model for genetics:
onlyUsingNonZeros = FALSE
numberOfTerms = 4
if(onlyUsingNonZeros == TRUE){
  model2=lm(residual_matrix_vector[!matrix_vector3 == 0] ~ pmax(poly(matrix_vector3[!matrix_vector3 == 0], 
                                                             numberOfTerms, raw=TRUE), 
                                                        rep(0,length(matrix_vector3[!matrix_vector3 == 0]))))
  quadratic = 0
  for(x in 1:length(model2$coefficients)){
    quadratic = quadratic + (model2$coefficients[x] * ((matrix_vector3[!matrix_vector3 == 0]**(x-1))))
  }
#  plot(matrix_vector3[!matrix_vector3 == 0], quadratic, xlab = "Number of migrations", ylab = "Predicted ")
} else {
  model2=lm(residual_matrix_vector ~ pmax(poly(matrix_vector3, numberOfTerms, raw=TRUE), 
                                                        rep(0,length(matrix_vector3))))
  quadratic = 0
  for(x in 1:length(model2$coefficients)){
    quadratic = quadratic + (model2$coefficients[x] * (matrix_vector3**(x-1)))
  }
#  plot(matrix_vector3, quadratic, xlab = "Number of migrations", ylab = "Predicted change to linguistic distance")
}

#finding the likelihoods of the models
weighting =0.01
find_error=function(x) {
  return(error_to_probability(x,0,model1_sd))
}
likelihoods1 = sapply(residual_matrix_vector, find_error)
likelihoods2 = sapply(residual_matrix_vector - quadratic, find_error)
total_likelihoods = likelihoods1*weighting + likelihoods2* (1-weighting)
sum(sapply(total_likelihoods,log))

#finding particular languages that are better predicted after adding a residual by genetics
find_pair_of_languages = function(x) { 
  number = length(constrained_language_tree$tip.label)
  m = floor(x/number) + 1
  n = x %% number
  return(c(m, n))
}
significances = likelihoods2/likelihoods1
ordered_significances = order(significances, decreasing = T)
significances[ordered_significances][1:800]
result = data.frame()

for(i in 1:700) { 
  language_pair = find_pair_of_languages(ordered_significances[i])
  m = language_pair[1]
  n = language_pair[2]
  toAdd = data.frame(c(constrained_language_tree$tip.label[m]),
                     c(constrained_language_tree$tip.label[n]),
                     c(file$Longitude[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                     c(file$Latitude[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                     c(file$Longitude[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                     c(file$Latitude[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                     c(significances[ordered_significances[i]]),
                     c(file$Language[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                     c(file$Language[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                     c(file$Language.family[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                     c(file$Language.family[which(file$ISO.code == constrained_language_tree$tip.label[n])]))
  names(toAdd) = c("l1", "l2", "l1_long", "l1_lat", "l2_long", "l2_lat", 
                   "significance", "language_name1", 
                   "language_name2", "language_family1", "language_family2")
  result = rbind(result, toAdd)
}

#writing result to a file
threshold = 2
linesToWrite = c()
for(m in 1:length(result[,1])){
  if(result$significance[m] > threshold) {
    string = paste(sub("_"," ", result$language_name1[m]),' (',result$language_family1[m],')','\t',
                   sub("_"," ", result$language_name2[m]),' (',result$language_family2[m],')', '\t',
                   result$significance[m], sep="")
    linesToWrite = append(linesToWrite, string)
  }
}
outputFile = "/Users/jeremycollins/Documents/Thesis/Code/Chapter 3/Run 2/ToPlot/constrainedAbsoluteTable.txt"
writeLines(linesToWrite, outputFile)

# a permutation test
permutation_results=c()
for(i in 1:1000){
  random_matrix_vector = sample(matrix_vector3, length(matrix_vector3))
  numberOfTerms = 4
  model2=lm(residual_matrix_vector ~ pmax(poly(random_matrix_vector, numberOfTerms, raw=TRUE), 
                                          rep(0,length(matrix_vector3))))
  quadratic = 0
  for(x in 1:length(model2$coefficients)){
    quadratic = quadratic + (model2$coefficients[x] * (random_matrix_vector**(x-1)))
  }
  #  plot(matrix_vector3, quadratic)
  
  find_error=function(x) {
    return(error_to_probability(x,0,model1_sd))
  }
  likelihoods1 = sapply(residual_matrix_vector, find_error)
  likelihoods2 = sapply(residual_matrix_vector - quadratic, find_error)
  
  weighting = 0.01
  total_likelihoods = likelihoods1*weighting + likelihoods2* (1-weighting)
  permutation_result = sum(sapply(total_likelihoods,log))
  permutation_results = append(permutation_results, permutation_result)
  print(i)
}
sort(permutation_results)

#using mantel test
install.packages('ncf')
library(ncf)
partial.mantel.test(matrix1, matrix3, matrix2, method = 'spearman')

# using relative probabilities
numberOfTerms = 1
model2=lm(residual_matrix_vector[!is.na(matrix_vector4)] ~ poly(matrix_vector4[!is.na(matrix_vector4)], numberOfTerms, raw=TRUE))
quadratic = 0
for(x in 1:length(model2$coefficients)){
  quadratic = quadratic + (model2$coefficients[x] * (random_matrix_vector**(x-1)))
}
#  plot(matrix_vector3, quadratic)

find_error=function(x) {
  return(error_to_probability(x,0,model1_sd))
}
likelihoods1 = sapply(residual_matrix_vector[!is.na(matrix_vector4)], find_error)
likelihoods2 = sapply(residual_matrix_vector[!is.na(matrix_vector4)] - quadratic[!is.na(matrix_vector4)], find_error)

weighting = 0.5
total_likelihoods = likelihoods1*weighting + likelihoods2* (1-weighting)
sum(sapply(total_likelihoods,log))



#repeating above analysis but making migrations symmetrical, i.e. m to n and n to n
matrix1 = as.matrix(distTips(constrained_language_tree))
matrix2 = constrained_languages_geographical_distance_matrix
matrix3 = matrix(0,length(constrained_language_tree$tip.label),
                 length(constrained_language_tree$tip.label))
for(m in 1:length(constrained_eurasia_movements$languages1)){
  index1 = which(constrained_language_tree$tip.label == constrained_eurasia_movements$languages1ISO[[m]])
  index2 = which(constrained_language_tree$tip.label == constrained_eurasia_movements$languages2ISO[[m]])
  matrix3[index1, index2] = matrix3[index1, index2] + 1
  matrix3[index2, index1] = matrix3[index2, index1] + 1
}

matrix_vector1 = as.vector(matrix1)
matrix_vector2 = as.vector(matrix2)
matrix_vector3 = as.vector(t(matrix3))

#function for turning error into probability
error_to_probability = function(x, mean, sd){
  a = pnorm(x, mean, sd)
  b = pnorm(x+0.1, mean, sd)
  return(b-a)
}

# a model for geography
onlyUsingNonZeros=FALSE
if(onlyUsingNonZeros == TRUE){
  model1=lm(matrix_vector1[!matrix_vector3 == 0] ~ poly(matrix_vector2[!matrix_vector3 == 0], 13, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model1$coefficients)){
    quadratic = quadratic + (model1$coefficients[x] * (matrix_vector2[!matrix_vector3 == 0]**(x-1)))
  }
} else {
  model1=lm(matrix_vector1 ~ poly(matrix_vector2, 13, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model1$coefficients)){
    quadratic = quadratic + (model1$coefficients[x] * (matrix_vector2**(x-1)))
  }
}
#a correction which makes all identical languages have predicted distance of 0
quadratic[matrix_vector2==0] = 0

#finding residuals from geographic model
residual_matrix_vector=matrix_vector1-quadratic
model1_sd = sd(residual_matrix_vector)
model2_sd = model1_sd
#a model for genetics:
onlyUsingNonZeros = FALSE
numberOfTerms = 1
if(onlyUsingNonZeros == TRUE){
  model2=lm(residual_matrix_vector[!matrix_vector3 == 0] ~ pmax(poly(matrix_vector3[!matrix_vector3 == 0], 
                                                                     numberOfTerms, raw=TRUE), 
                                                                rep(0,length(matrix_vector3[!matrix_vector3 == 0]))))
  quadratic = 0
  for(x in 1:length(model2$coefficients)){
    quadratic = quadratic + (model2$coefficients[x] * ((matrix_vector3[!matrix_vector3 == 0]**(x-1))))
  }
  #  plot(matrix_vector3[!matrix_vector3 == 0], quadratic)
} else {
  model2=lm(residual_matrix_vector ~ pmax(poly(matrix_vector3, numberOfTerms, raw=TRUE), 
                                          rep(0,length(matrix_vector3))))
  quadratic = 0
  for(x in 1:length(model2$coefficients)){
    quadratic = quadratic + (model2$coefficients[x] * (matrix_vector3**(x-1)))
  }
  #  plot(matrix_vector3, quadratic)
}

#finding the likelihoods of the models
weighting =0.01
find_error=function(x) {
  return(error_to_probability(x,0,model1_sd))
}
likelihoods1 = sapply(residual_matrix_vector, find_error)
likelihoods2 = sapply(residual_matrix_vector - quadratic, find_error)
total_likelihoods = likelihoods1*weighting + likelihoods2* (1-weighting)
sum(sapply(total_likelihoods,log))

#finding particular languages that are better predicted after adding a residual by genetics
find_pair_of_languages = function(x) { 
  number = length(constrained_language_tree$tip.label)
  m = floor(x/number) + 1
  if(n == 0) {
    n = number
  }
  return(c(m, n))
}
significances = likelihoods2/likelihoods1
ordered_significances = order(significances, decreasing = T)
significances[ordered_significances][1:10]
result = data.frame()

for(i in 1:500) { 
  language_pair = find_pair_of_languages(ordered_significances[i])
  m = language_pair[1]
  n = language_pair[2]
  toAdd = data.frame(c(constrained_language_tree$tip.label[m]),
                     c(constrained_language_tree$tip.label[n]),
                     c(file$Longitude[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                     c(file$Latitude[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                     c(file$Longitude[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                     c(file$Latitude[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                     c(significances[ordered_significances[i]]),
                     c(file$Language[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                     c(file$Language[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                     c(file$Language.family[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                     c(file$Language.family[which(file$ISO.code == constrained_language_tree$tip.label[n])]))
  names(toAdd) = c("l1", "l2", "l1_long", "l1_lat", "l2_long", "l2_lat", 
                   "significance", "language_name1", 
                   "language_name2", "language_family1", "language_family2")
  result = rbind(result, toAdd)
}

#writing result to a file
linesToWrite = c()
for(m in 1:length(result[,1])){
  string = paste(sub("_"," ", result$language_name1[m]),' (',result$language_family1[m],')','\t',
                 sub("_"," ", result$language_name2[m]),' (',result$language_family2[m],')', sep="")
  linesToWrite = append(linesToWrite, string)
}
outputFile = "/Users/jeremycollins/Documents/Thesis/Code/Chapter 3/Run 2/ToPlot/constrainedAbsoluteTable.txt"
writeLines(linesToWrite, outputFile)

#analysis using unconstrained languages distances

matrix1 = as.matrix(distTips(language_tree))
matrix2 = languages_geographical_distance_matrix
matrix3 = matrix(0,length(language_tree$tip.label),
                 length(language_tree$tip.label))
for(m in 1:length(eurasia_movements$languages1)){
  index1 = which(language_tree$tip.label == eurasia_movements$languages1ISO[[m]])
  index2 = which(language_tree$tip.label == eurasia_movements$languages2ISO[[m]])
  matrix3[index1, index2] = matrix3[index1, index2] + 1
}
matrix4 = matrix(0,length(language_tree$tip.label),
                 length(language_tree$tip.label))
for(m in 1:length(language_tree$tip.label)){
  total = sum(matrix3[m,])
  for(n in 1:length(language_tree$tip.label)){
    matrix4[m,n] = matrix3[m,n]/total 
  }
}
matrix_vector1 = as.vector(matrix1)
matrix_vector2 = as.vector(matrix2)
matrix_vector3 = as.vector(t(matrix3))
matrix_vector4 = as.vector(t(matrix4))

#function for turning error into probability
error_to_probability = function(x, mean, sd){
  a = pnorm(x, mean, sd)
  b = pnorm(x+0.01, mean, sd)
  return(b-a)
}
# a model for geography
onlyUsingNonZeros=FALSE
numberOfTerms = 13
if(onlyUsingNonZeros == TRUE){
  model1=lm(matrix_vector1[!matrix_vector3 == 0] ~ poly(matrix_vector2[!matrix_vector3 == 0], numberOfTerms, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model1$coefficients)){
    quadratic = quadratic + (model1$coefficients[x] * (matrix_vector2[!matrix_vector3 == 0]**(x-1)))
  }
} else {
  model1=lm(matrix_vector1 ~ poly(matrix_vector2, numberOfTerms, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model1$coefficients)){
    quadratic = quadratic + (model1$coefficients[x] * (matrix_vector2**(x-1)))
  }
 plot(matrix_vector2, quadratic)  
}

#a correction which makes all identical languages have predicted distance of 0
quadratic[matrix_vector2==0] = 0


#finding residuals from geographic model
residual_matrix_vector=matrix_vector1-quadratic
model1_sd = sd(residual_matrix_vector)
model2_sd = model1_sd
#a model for genetics:
onlyUsingNonZeros = FALSE
numberOfTerms = 1
if(onlyUsingNonZeros == TRUE){
  model2=lm(residual_matrix_vector[!matrix_vector3 == 0] ~ pmax(poly(matrix_vector3[!matrix_vector3 == 0], 
                                                                     numberOfTerms, raw=TRUE), 
                                                                rep(0,length(matrix_vector3[!matrix_vector3 == 0]))))
  quadratic = 0
  for(x in 1:length(model2$coefficients)){
    quadratic = quadratic + (model2$coefficients[x] * ((matrix_vector3[!matrix_vector3 == 0]**(x-1))))
  }
  #  plot(matrix_vector3[!matrix_vector3 == 0], quadratic)
} else {
  model2=lm(residual_matrix_vector ~ pmax(poly(matrix_vector3, numberOfTerms, raw=TRUE), 
                                          rep(0,length(matrix_vector3))))
  quadratic = 0
  for(x in 1:length(model2$coefficients)){
    quadratic = quadratic + (model2$coefficients[x] * (matrix_vector3**(x-1)))
  }
  #  plot(matrix_vector3, quadratic)
}

#finding the likelihoods of the models

find_error=function(x) {
  return(error_to_probability(x,0,model1_sd))
}
likelihoods1 = sapply(residual_matrix_vector, find_error)
likelihoods2 = sapply(residual_matrix_vector - quadratic, find_error)

weighting = 0.01
total_likelihoods = likelihoods1*weighting + likelihoods2* (1-weighting)
sum(sapply(total_likelihoods,log))

# a permutation test
random_matrix_vector = sample(matrix_vector3, length(matrix_vector3))
model2=lm(residual_matrix_vector ~ pmax(poly(random_matrix_vector, numberOfTerms, raw=TRUE), 
                                        rep(0,length(matrix_vector3))))
quadratic = 0
for(x in 1:length(model2$coefficients)){
  quadratic = quadratic + (model2$coefficients[x] * (random_matrix_vector**(x-1)))
}
#  plot(matrix_vector3, quadratic)

find_error=function(x) {
  return(error_to_probability(x,0,model1_sd))
}
likelihoods1 = sapply(residual_matrix_vector, find_error)
likelihoods2 = sapply(residual_matrix_vector - quadratic, find_error)

weighting = 0.01
total_likelihoods = likelihoods1*weighting + likelihoods2* (1-weighting)
sum(sapply(total_likelihoods,log))

#using mantel test
install.packages('ncf')
library(ncf)
partial.mantel.test(matrix1, matrix3, matrix2, method = 'spearman')

#finding particular languages that are better predicted after adding a residual by genetics
find_pair_of_languages = function(x) { 
  number = length(language_tree$tip.label)
  m = floor(x/number) + 1
  n = x %% number
  if(n == 0) {
    n = number
  }
  return(c(m, n))
}
significances = likelihoods2/likelihoods1
ordered_significances = order(significances, decreasing = T)
significances[ordered_significances][1:10]
result = data.frame()

for(i in 1:100) { 
  language_pair = find_pair_of_languages(ordered_significances[i])
  m = language_pair[1]
  n = language_pair[2]
  toAdd = data.frame(c(language_tree$tip.label[m]),
                     c(language_tree$tip.label[n]),
                     c(file$Longitude[which(file$ISO.code == language_tree$tip.label[m])]),
                     c(file$Latitude[which(file$ISO.code == language_tree$tip.label[m])]),
                     c(file$Longitude[which(file$ISO.code == language_tree$tip.label[n])]),
                     c(file$Latitude[which(file$ISO.code == language_tree$tip.label[n])]),
                     c(significances[ordered_significances[i]]),
                     c(file$Language[which(file$ISO.code == language_tree$tip.label[m])]),
                     c(file$Language[which(file$ISO.code == language_tree$tip.label[n])]),
                     c(file$Language.family[which(file$ISO.code == language_tree$tip.label[m])]),
                     c(file$Language.family[which(file$ISO.code == language_tree$tip.label[n])]))
  names(toAdd) = c("l1", "l2", "l1_long", "l1_lat", "l2_long", "l2_lat", 
                   "significance", "language_name1", 
                   "language_name2", "language_family1", "language_family2")
  result = rbind(result, toAdd)
}

#writing result to a file
linesToWrite = c()
for(m in 1:length(result[,1])){
  string = paste(sub("_"," ", result$language_name1[m]),' (',result$language_family1[m],')','\t',
                 sub("_"," ", result$language_name2[m]),' (',result$language_family2[m],')', sep="")
  linesToWrite = append(linesToWrite, string)
}
outputFile = "/Users/jeremycollins/Documents/Thesis/Code/Chapter 3/Run 2/ToPlot/constrainedAbsoluteTable.txt"
writeLines(linesToWrite, outputFile)



for(m in 1:length(matrix1[1,])) {
  for(n in 1:length(matrix1[,1])) {
    migrations = matrix3[m,n]
    if(!migrations == 0){
      geographical_distance = matrix2[m,n]
      geographical_predicted = 0
      if(geographical_distance == 0) {
        geographical_predicted = 0
      } else {
        for(i in 1:length(model1$coefficients)){
          geographical_predicted = geographical_predicted + model1$coefficients[i]*(geographical_distance**(i-1))
        }
      }
      genetic_predicted = 0
      for(i in 1:length(model2$coefficients)){
        genetic_predicted = genetic_predicted + model2$coefficients[i]*(migrations**(i-1))
      }
      actual = matrix1[m,n]
      genetic_probability = error_to_probability(actual, genetic_predicted, model2_sd)
      geographical_probability = error_to_probability(actual, geographical_predicted, model1_sd)
      weighted_genetic_probability = genetic_probability*(1-weighting)
      weighted_geographical_probability = geographical_probability*weighting
      if(weighted_genetic_probability > weighted_geographical_probability) { 
        if(weighted_genetic_probability > weighted_geographical_probability) { 
          toAdd = data.frame(c(constrained_language_tree$tip.label[m]),
                             c(constrained_language_tree$tip.label[n]),
                             c(file$Longitude[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                             c(file$Latitude[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                             c(file$Longitude[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                             c(file$Latitude[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                             c(actual), c(genetic_predicted), c(geographical_predicted),
                             c(file$Language[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                             c(file$Language[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                             c(file$Language.family[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                             c(file$Language.family[which(file$ISO.code == constrained_language_tree$tip.label[n])]))
          names(toAdd) = c("l1", "l2", "l1_long", "l1_lat", "l2_long", "l2_lat", 
                           "actual", "genetic_predicted", "geographical_predicted", "language_name1", 
                           "language_name2", "language_family1", "language_family2")
          result = rbind(result, toAdd)
      }
    }
    print(m)
    print(n)
  }
}
#write coordinates for making a kml file:
linesToWrite = c()
for(m in 1:length(result[,1])){
  string = paste(paste(toString(result$l1_long[m]),toString(result$l1_lat[m]), sep=","),
                 paste(toString(result$l2_long[m]),toString(result$l2_lat[m]), sep=","), sep=" ")
  linesToWrite = append(linesToWrite, string)
}
outputFile = "/Users/jeremycollins/Documents/Thesis/Code/Chapter 3/Run 2/ToPlot/constrainedAbsolute.txt"
writeLines(linesToWrite, outputFile)

#write language names and families in a table:
linesToWrite = c()
for(m in 1:length(result[,1])){
  string = paste(sub("_"," ", result$language_name1[m]),' (',result$language_family1[m],')','\t',
                 sub("_"," ", result$language_name2[m]),' (',result$language_family2[m],')', sep="")
  linesToWrite = append(linesToWrite, string)
}
outputFile = "/Users/jeremycollins/Documents/Thesis/Code/Chapter 3/Run 2/ToPlot/constrainedAbsoluteTable.txt"
writeLines(linesToWrite, outputFile)

#repeating the above analysis with probability of migration instead of absolute numbers
#model2
onlyUsingNonZeros = TRUE
if(onlyUsingNonZeros == TRUE){
  model2=lm(matrix_vector1[!is.na(matrix_vector4) & matrix_vector4>0] ~ poly(matrix_vector4[!is.na(matrix_vector4) & matrix_vector4>0], 1, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model2$coefficients)){
    quadratic = quadratic + (model2$coefficients[x] * (matrix_vector4[!is.na(matrix_vector4) & matrix_vector4>0]**(x-1)))
  }
#  plot(matrix_vector4[!is.na(matrix_vector4) & matrix_vector4>0], quadratic)
} else {
  model2=lm(matrix_vector1[!is.na(matrix_vector4)] ~ poly(matrix_vector4[!is.na(matrix_vector4)], 1, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model2$coefficients)){
    quadratic = quadratic + (model2$coefficients[x] * (matrix_vector4[!is.na(matrix_vector4)]**(x-1)))
  }
#    plot(matrix_vector4[!is.na(matrix_vector4)], quadratic)
} 
#finding the standard deviation:
total = 0
count = 0
for(m in 1:length(matrix1[1,])) {
  for(n in 1:length(matrix1[,1])) {
    migrations = matrix4[m,n]
    if(!is.na(migrations) & (!migrations == 0 | onlyUsingNonZeros == FALSE)){
      predicted = 0
      for(i in 1:length(model2$coefficients)){
        predicted = predicted + model2$coefficients[i]*(migrations**(i-1))
      }
      actual = matrix1[m,n]
      total = total + (abs(predicted - actual)**2)
      count = count + 1 
    }
  }
}
model2_sd = (total/count)**0.5
#mixture model:
weighting = 0.44
total = 0
onlyUsingNonZeros = TRUE
geoMinimum = TRUE
for(m in 1:length(matrix1[1,])) {
  for(n in 1:length(matrix1[,1])) {
    migrations = matrix4[m,n]
    geographical_distance = matrix2[m,n]
    if(!is.na(migrations) & (!migrations == 0 | onlyUsingNonZeros == FALSE)){
      geographical_predicted = 0
      if(geographical_distance == 0) {
        geographical_predicted = 0
      } else {
        for(i in 1:length(model1$coefficients)){
          geographical_predicted = geographical_predicted + model1$coefficients[i]*(geographical_distance**(i-1))
        }
      }
      genetic_predicted = 0
      for(i in 1:length(model2$coefficients)){
        genetic_predicted = genetic_predicted + model2$coefficients[i]*(migrations**(i-1))
      }
      actual = matrix1[m,n]
      genetic_probability = error_to_probability(actual, genetic_predicted, model2_sd)
      geographical_probability = error_to_probability(actual, geographical_predicted, model1_sd)
      if(genetic_predicted < geographical_predicted | geoMinimum == FALSE) {
        probability =  (geographical_probability*weighting) + (genetic_probability* (1-weighting))
      } else {
        probability = geographical_probability
      }
      total = total + log(probability)
    }
  }
}
total
#finding particular languages which are better predicted by genetics 
result = data.frame()
for(m in 1:length(matrix1[1,])) {
  for(n in 1:length(matrix1[,1])) {
    migrations = matrix4[m,n]
    if(!is.na(migrations) & !migrations == 0){
      geographical_distance = matrix2[m,n]
      geographical_predicted = 0
      if(geographical_distance == 0) {
        geographical_predicted = 0
      } else {
        for(i in 1:length(model1$coefficients)){
          geographical_predicted = geographical_predicted + model1$coefficients[i]*(geographical_distance**(i-1))
        }
      }
      genetic_predicted = 0
      for(i in 1:length(model2$coefficients)){
        genetic_predicted = genetic_predicted + model2$coefficients[i]*(migrations**(i-1))
      }
      actual = matrix1[m,n]
      genetic_probability = error_to_probability(actual, genetic_predicted, model2_sd)
      geographical_probability = error_to_probability(actual, geographical_predicted, model1_sd)
      weighted_genetic_probability = genetic_probability*(1-weighting)
      weighted_geographical_probability = geographical_probability*weighting
      if(weighted_genetic_probability > weighted_geographical_probability) { 
        toAdd = data.frame(c(constrained_language_tree$tip.label[m]),
                           c(constrained_language_tree$tip.label[n]),
                           c(file$Longitude[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                           c(file$Latitude[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                           c(file$Longitude[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                           c(file$Latitude[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                           c(actual), c(genetic_predicted), c(geographical_predicted),
                           c(file$Language[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                           c(file$Language[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                           c(file$Language.family[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                           c(file$Language.family[which(file$ISO.code == constrained_language_tree$tip.label[n])]))
        names(toAdd) = c("l1", "l2", "l1_long", "l1_lat", "l2_long", "l2_lat", 
                         "actual", "genetic_predicted", "geographical_predicted", "language_name1", 
                         "language_name2", "language_family1", "language_family2")
        result = rbind(result, toAdd)
      }
    }
    print(m)
    print(n)
  }
}
#write coordinates for making a kml file:
linesToWrite = c()
for(m in 1:length(result[,1])){
  string = paste(paste(toString(result$l1_long[m]),toString(result$l1_lat[m]), sep=","),
                 paste(toString(result$l2_long[m]),toString(result$l2_lat[m]), sep=","), sep=" ")
  linesToWrite = append(linesToWrite, string)
}
outputFile = "/Users/jeremycollins/Documents/Thesis/Code/Chapter 3/Run 2/ToPlot/constrainedRelative.txt"
writeLines(linesToWrite, outputFile)

#write language names and families in a table:
linesToWrite = c()
for(m in 1:length(result[,1])){
  string = paste(sub("_"," ", result$language_name1[m]),' (',result$language_family1[m],')','\t',
                 sub("_"," ", result$language_name2[m]),' (',result$language_family2[m],')', sep="")
  linesToWrite = append(linesToWrite, string)
}
outputFile = "/Users/jeremycollins/Documents/Thesis/Code/Chapter 3/Run 2/ToPlot/constrainedRelativeTable.txt"
writeLines(linesToWrite, outputFile)


#repeating above analyses with unconstrained language distances
matrix1 = as.matrix(distTips(language_tree))
matrix2 = languages_geographical_distance_matrix
matrix3 = matrix(0,length(language_tree$tip.label),
                 length(language_tree$tip.label))
for(m in 1:length(eurasia_movements$languages1)){
  index1 = which(language_tree$tip.label == eurasia_movements$languages1ISO[[m]])
  index2 = which(language_tree$tip.label == eurasia_movements$languages2ISO[[m]])
  matrix3[index1, index2] = matrix3[index1, index2] + 1
}
matrix4 = matrix(0,length(language_tree$tip.label),
                 length(language_tree$tip.label))
for(m in 1:length(language_tree$tip.label)){
  total = sum(matrix3[m,])
  for(n in 1:length(language_tree$tip.label)){
    matrix4[m,n] = matrix3[m,n]/total 
  }
}
matrix_vector1 = as.vector(matrix1)
matrix_vector2 = as.vector(matrix2)
matrix_vector3 = as.vector(t(matrix3))
matrix_vector4 = as.vector(t(matrix4))

#function for turning error into probability
error_to_probability = function(x, mean, sd){
    a = pnorm(x, mean, sd)
    b = pnorm(x+0.01, mean, sd)
    return(b-a)
}

# a model for geography
onlyUsingNonZeros=FALSE
if(onlyUsingNonZeros == TRUE){
  model1=lm(matrix_vector1[!matrix_vector3 == 0] ~ poly(matrix_vector2[!matrix_vector3 == 0], 5, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model1$coefficients)){
    quadratic = quadratic + (model1$coefficients[x] * (matrix_vector2[!matrix_vector3 == 0]**(x-1)))
  }
  #plot(matrix_vector2[!matrix_vector3 == 0], quadratic)
} else {
  model1=lm(matrix_vector1 ~ poly(matrix_vector2, 5, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model1$coefficients)){
    quadratic = quadratic + (model1$coefficients[x] * (matrix_vector2**(x-1)))
  }
#  plot(matrix_vector2, quadratic)
}
#finding the standard deviation for model1:
total = 0
count = 0
for(m in 1:length(matrix1[1,])) {
  for(n in 1:length(matrix1[,1])) {
    geographical_distance = matrix2[m,n]
    migrations = matrix3[m,n]
    if(!migrations == 0 | onlyUsingNonZeros == FALSE){
      predicted = 0
      if(geographical_distance == 0) {
        predicted = 0
      } else {
        for(i in 1:length(model1$coefficients)){
          predicted = predicted + model1$coefficients[i]*(geographical_distance**(i-1))
        }
      }
      actual = matrix1[m,n]
      total = total + (abs(predicted - actual)**2)
      count = count + 1
    }
  }
}
model1_sd = (total/count)**0.5
model2_sd = model1_sd
#a model for genetics:
onlyUsingNonZeros = FALSE
if(onlyUsingNonZeros == TRUE){
  model2=lm(matrix_vector1[!matrix_vector3 == 0] ~ pmax(poly(matrix_vector3[!matrix_vector3 == 0], 
                                                             5, raw=TRUE), 
                                                        rep(0,length(matrix_vector3[!matrix_vector3 == 0]))))
  quadratic = 0
  for(x in 1:length(model2$coefficients)){
    quadratic = quadratic + (model2$coefficients[x] * ((matrix_vector3[!matrix_vector3 == 0]**(x-1))))
  }
  quadratic = pmax(rep(0,length(matrix_vector3[!matrix_vector3==0])), quadratic)
#  plot(matrix_vector3[!matrix_vector3 == 0], quadratic)
} else {
  model2=lm(matrix_vector1 ~ pmax(poly(matrix_vector3,5, raw=TRUE), 
                                  rep(0,length(matrix_vector3))))
  quadratic = 0
  for(x in 1:length(model2$coefficients)){
    quadratic = quadratic + (model2$coefficients[x] * (matrix_vector3**(x-1)))
  }
  quadratic = pmax(rep(0,length(matrix_vector3)), quadratic)
   # plot(matrix_vector3, quadratic)
}
#finding the standard deviation:
total = 0
count = 0
for(m in 1:length(matrix1[1,])) {
  for(n in 1:length(matrix1[,1])) {
    migrations = matrix3[m,n]
    if(!migrations == 0 | onlyUsingNonZeros == FALSE){
      predicted = 0
      for(i in 1:length(model2$coefficients)){
        predicted = predicted + model2$coefficients[i]*(migrations**(i-1))
      }
      predicted = max(0,predicted)
      actual = matrix1[m,n]
      total = total + (abs(predicted - actual)**2)
      count = count + 1
    }
  }
}
model2_sd = (total/count)**0.5
#mixture model:
weighting = 0.93
onlyCountNonZeros=FALSE
geoMinimum = FALSE
total = 0
for(m in 1:length(matrix1[1,])) {
  for(n in 1:length(matrix1[,1])) {
    migrations = matrix3[m,n]
    if(!migrations == 0 | onlyCountNonZeros==FALSE) {
      geographical_distance = matrix2[m,n]
      geographical_predicted = 0
      if(geographical_distance == 0) {
        geographical_predicted = 0
      } else {
        for(i in 1:length(model1$coefficients)){
          geographical_predicted = geographical_predicted + model1$coefficients[i]*(geographical_distance**(i-1))
        }
      }
      genetic_predicted = 0
      for(i in 1:length(model2$coefficients)){
        genetic_predicted = genetic_predicted + model2$coefficients[i]*(migrations**(i-1))
      }
      genetic_predicted = max(0,genetic_predicted)
      actual = matrix1[m,n]
      genetic_probability = error_to_probability(actual, genetic_predicted, model2_sd)
      geographical_probability = error_to_probability(actual, geographical_predicted, model1_sd)
      if(geoMinimum == FALSE | genetic_predicted < geographical_predicted) {
        probability =  (geographical_probability*weighting) + (genetic_probability* (1- weighting))
      } else { 
        probability = geographical_probability
      }
      total = total + log(probability)
    }
  }
}
total

#finding particular languages which are better predicted by genetics 
result = data.frame()
for(m in 1:length(matrix1[1,])) {
  for(n in 1:length(matrix1[,1])) {
    migrations = matrix3[m,n]
    if(!migrations == 0){
      geographical_distance = matrix2[m,n]
      geographical_predicted = 0
      if(geographical_distance == 0) {
        geographical_predicted = 0
      } else {
        for(i in 1:length(model1$coefficients)){
          geographical_predicted = geographical_predicted + model1$coefficients[i]*(geographical_distance**(i-1))
        }
      }
      genetic_predicted = 0
      for(i in 1:length(model2$coefficients)){
        genetic_predicted = genetic_predicted + model2$coefficients[i]*(migrations**(i-1))
      }
      actual = matrix1[m,n]
      genetic_probability = error_to_probability(actual, genetic_predicted, model2_sd)
      geographical_probability = error_to_probability(actual, geographical_predicted, model1_sd)
      weighting = 0.93
#      weighting = 0.5
      weighted_genetic_probability = genetic_probability*(1-weighting)
      weighted_geographical_probability = geographical_probability*weighting
      if(weighted_genetic_probability > weighted_geographical_probability) { 
        if(weighted_genetic_probability > weighted_geographical_probability) { 
          toAdd = data.frame(c(constrained_language_tree$tip.label[m]),
                             c(constrained_language_tree$tip.label[n]),
                             c(file$Longitude[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                             c(file$Latitude[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                             c(file$Longitude[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                             c(file$Latitude[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                             c(actual), c(genetic_predicted), c(geographical_predicted),
                             c(file$Language[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                             c(file$Language[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                             c(file$Language.family[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                             c(file$Language.family[which(file$ISO.code == constrained_language_tree$tip.label[n])]))
          names(toAdd) = c("l1", "l2", "l1_long", "l1_lat", "l2_long", "l2_lat", 
                           "actual", "genetic_predicted", "geographical_predicted", "language_name1", 
                           "language_name2", "language_family1", "language_family2")
          result = rbind(result, toAdd)
      }
    }
    print(m)
    print(n)
  }
  }
}
resultBackup = result
#write coordinates for making a kml file:
  linesToWrite = c()
  for(m in 1:length(result[,1])){
    string = paste(paste(toString(result$l1_long[m]),toString(result$l1_lat[m]), sep=","),
                   paste(toString(result$l2_long[m]),toString(result$l2_lat[m]), sep=","), sep=" ")
    linesToWrite = append(linesToWrite, string)
  }
  outputFile = "/Users/jeremycollins/Documents/Thesis/Code/Chapter 3/Run 2/ToPlot/unconstrainedAbsolute.txt"
  writeLines(linesToWrite, outputFile)
  
  #write language names and families in a table:
  result = resultBackup[resultBackup$genetic_predicted < resultBackup$geographical_predicted,]
  linesToWrite = c()
  for(m in 1:length(result[,1])){
    string = paste(sub("_"," ", result$language_name1[m]),' (',result$language_family1[m],')','\t',
                   sub("_"," ", result$language_name2[m]),' (',result$language_family2[m],')', sep="")
    linesToWrite = append(linesToWrite, string)
  }
  outputFile = "/Users/jeremycollins/Documents/Thesis/Code/Chapter 3/Run 2/ToPlot/unconstrainedAbsoluteLessTable.txt"
  writeLines(linesToWrite, outputFile)

  #write language names and families in a table:
  result = resultBackup[resultBackup$genetic_predicted > resultBackup$geographical_predicted,]
  linesToWrite = c()
  for(m in 1:length(result[,1])){
    string = paste(sub("_"," ", result$language_name1[m]),' (',result$language_family1[m],')','\t',
                   sub("_"," ", result$language_name2[m]),' (',result$language_family2[m],')', sep="")
    linesToWrite = append(linesToWrite, string)
  }
  outputFile = "/Users/jeremycollins/Documents/Thesis/Code/Chapter 3/Run 2/ToPlot/unconstrainedAbsoluteMoreTable.txt"
  writeLines(linesToWrite, outputFile)
  
#repeating the above analysis with probability of migration instead of absolute numbers
#model2
onlyUsingNonZeros = TRUE
if(onlyUsingNonZeros == TRUE){
  model2=lm(matrix_vector1[!is.na(matrix_vector4) & matrix_vector4>0] ~ poly(matrix_vector4[!is.na(matrix_vector4) & matrix_vector4>0], 1, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model2$coefficients)){
    quadratic = quadratic + (model2$coefficients[x] * (matrix_vector4[!is.na(matrix_vector4) & matrix_vector4>0]**(x-1)))
  }
   # plot(matrix_vector4[!is.na(matrix_vector4) & matrix_vector4>0], quadratic)
} else {
  model2=lm(matrix_vector1[!is.na(matrix_vector4)] ~ poly(matrix_vector4[!is.na(matrix_vector4)], 1, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model2$coefficients)){
    quadratic = quadratic + (model2$coefficients[x] * (matrix_vector4[!is.na(matrix_vector4)]**(x-1)))
  }
 #     plot(matrix_vector4[!is.na(matrix_vector4)], quadratic)
} 
#finding the standard deviation:
total = 0
count = 0
for(m in 1:length(matrix1[1,])) {
  for(n in 1:length(matrix1[,1])) {
    migrations = matrix4[m,n]
    if(!is.na(migrations) & (!migrations == 0 | onlyUsingNonZeros == FALSE)){
      predicted = 0
      for(i in 1:length(model2$coefficients)){
        predicted = predicted + model2$coefficients[i]*(migrations**(i-1))
      }
      actual = matrix1[m,n]
      total = total + (abs(predicted - actual)**2)
      count = count + 1 
    }
  }
}
model2_sd = (total/count)**0.5
#mixture model:
weighting = 0.91
total = 0
onlyUsingNonZeros = TRUE
geoMinimum = TRUE
for(m in 1:length(matrix1[1,])) {
  for(n in 1:length(matrix1[,1])) {
    migrations = matrix4[m,n]
    geographical_distance = matrix2[m,n]
    if(!is.na(migrations) & (!migrations == 0 | onlyUsingNonZeros == FALSE)){
      geographical_predicted = 0
      if(geographical_distance == 0) {
        geographical_predicted = 0
      } else {
        for(i in 1:length(model1$coefficients)){
          geographical_predicted = geographical_predicted + model1$coefficients[i]*(geographical_distance**(i-1))
        }
      }
      genetic_predicted = 0
      for(i in 1:length(model2$coefficients)){
        genetic_predicted = genetic_predicted + model2$coefficients[i]*(migrations**(i-1))
      }
      actual = matrix1[m,n]
      genetic_probability = error_to_probability(actual, genetic_predicted, model2_sd)
      geographical_probability = error_to_probability(actual, geographical_predicted, model1_sd)
      if(genetic_predicted < geographical_predicted | geoMinimum == FALSE) {
        probability =  (geographical_probability*weighting) + (genetic_probability* (1-weighting))
      } else {
        probability = geographical_probability
      }
      total = total + log(probability)
    }
  }
}
total
#finding particular languages which are better predicted by genetics 
result = data.frame()
for(m in 1:length(matrix1[1,])) {
  for(n in 1:length(matrix1[,1])) {
    migrations = matrix4[m,n]
    if(!is.na(migrations) & !migrations == 0){
      geographical_distance = matrix2[m,n]
      geographical_predicted = 0
      if(geographical_distance == 0) {
        geographical_predicted = 0
      } else {
        for(i in 1:length(model1$coefficients)){
          geographical_predicted = geographical_predicted + model1$coefficients[i]*(geographical_distance**(i-1))
        }
      }
      genetic_predicted = 0
      for(i in 1:length(model2$coefficients)){
        genetic_predicted = genetic_predicted + model2$coefficients[i]*(migrations**(i-1))
      }
      actual = matrix1[m,n]
      genetic_probability = error_to_probability(actual, genetic_predicted, model2_sd)
      geographical_probability = error_to_probability(actual, geographical_predicted, model1_sd)
      weighted_genetic_probability = genetic_probability*(1-weighting)
      weighted_geographical_probability = geographical_probability*weighting
      if(weighted_genetic_probability > weighted_geographical_probability) { 
        toAdd = data.frame(c(constrained_language_tree$tip.label[m]),
                           c(constrained_language_tree$tip.label[n]),
                           c(file$Longitude[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                           c(file$Latitude[which(file$ISO.code == constrained_language_tree$tip.label[m])]),
                           c(file$Longitude[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                           c(file$Latitude[which(file$ISO.code == constrained_language_tree$tip.label[n])]),
                           c(actual), c(genetic_predicted), c(geographical_predicted))
        names(toAdd) = c("l1", "l2", "l1_long", "l1_lat", "l2_long", "l2_lat", 
                         "actual", "genetic_predicted", "geographical_predicted")
        result = rbind(result, toAdd)
      }
    }
    print(m)
    print(n)
  }
}

#test to see if a random model makes any difference, using constrained language distances

matrix1 = as.matrix(distTips(constrained_language_tree))
matrix2 = constrained_languages_geographical_distance_matrix
matrix3 = matrix(0,length(constrained_language_tree$tip.label),
                 length(constrained_language_tree$tip.label))
for(m in 1:length(constrained_eurasia_movements$languages1)){
  index1 = which(constrained_language_tree$tip.label == constrained_eurasia_movements$languages1ISO[[m]])
  index2 = which(constrained_language_tree$tip.label == constrained_eurasia_movements$languages2ISO[[m]])
  matrix3[index1, index2] = matrix3[index1, index2] + 1
}

matrix4 = matrix(0,length(constrained_language_tree$tip.label),
                 length(constrained_language_tree$tip.label))
for(m in 1:length(constrained_language_tree$tip.label)){
  total = sum(matrix3[m,])
  for(n in 1:length(constrained_language_tree$tip.label)){
    matrix4[m,n] = matrix3[m,n]/total 
  }
}

matrix_vector1 = as.vector(matrix1)
matrix_vector2 = as.vector(matrix2)
matrix_vector3 = as.vector(t(matrix3))
matrix_vector4 = as.vector(t(matrix4))

#function for turning error into probability
error_to_probability = function(x, mean, sd){
  if(x < 0){
    return(0)
  } else {
    a = pnorm(x, mean, sd)
    b = pnorm(x+0.1, mean, sd)
    return(b-a)
  }
}

# a model for geography
onlyUsingNonZeros=FALSE
if(onlyUsingNonZeros == TRUE){
  model1=lm(matrix_vector1[!matrix_vector3 == 0] ~ poly(matrix_vector2[!matrix_vector3 == 0], 13, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model1$coefficients)){
    quadratic = quadratic + (model1$coefficients[x] * (matrix_vector2[!matrix_vector3 == 0]**(x-1)))
  }
} else {
  model1=lm(matrix_vector1 ~ poly(matrix_vector2, 13, raw=TRUE))
  quadratic = 0
  for(x in 1:length(model1$coefficients)){
    quadratic = quadratic + (model1$coefficients[x] * (matrix_vector2**(x-1)))
  }
}
#plot(matrix_vector2[!matrix_vector3 == 0], quadratic)
#finding the standard deviation for model1:
total = 0
count = 0
for(m in 1:length(matrix1[1,])) {
  for(n in 1:length(matrix1[,1])) {
    geographical_distance = matrix2[m,n]
    migrations = matrix3[m,n]
    if(!migrations == 0 | onlyUsingNonZeros == FALSE){
      predicted = 0
      if(geographical_distance == 0) {
        predicted = 0
      } else {
        for(i in 1:length(model1$coefficients)){
          predicted = predicted + model1$coefficients[i]*(geographical_distance**(i-1))
        }
      }
      actual = matrix1[m,n]
      total = total + (abs(predicted - actual)**2)
      count = count + 1
    }
  }
}
model1_sd = (total/count)**0.5
#a random model
model2_sd = model1_sd
#mixture model:
weighting = 0.99
onlyCountNonZeros=FALSE
geoMinimum = FALSE
total = 0
for(m in 1:length(matrix1[1,])) {
  for(n in 1:length(matrix1[,1])) {
    migrations = matrix3[m,n]
    if(!migrations == 0 | onlyCountNonZeros==FALSE) {
      geographical_distance = matrix2[m,n]
      geographical_predicted = 0
      if(geographical_distance == 0) {
        geographical_predicted = 0
      } else {
        for(i in 1:length(model1$coefficients)){
          geographical_predicted = geographical_predicted + model1$coefficients[i]*(geographical_distance**(i-1))
        }
      }
      random_number = sample(0:500, 1)
      genetic_predicted = random_number
      actual = matrix1[m,n]
      genetic_probability = error_to_probability(actual, genetic_predicted, model2_sd)
      geographical_probability = error_to_probability(actual, geographical_predicted, model1_sd)
      if(geoMinimum == FALSE | genetic_predicted < geographical_predicted) {
        probability =  (geographical_probability*weighting) + (genetic_probability* (1- weighting))
      } else { 
        probability = geographical_probability
      }
      total = total + log(probability)
    }
  }
}
total
3908348



