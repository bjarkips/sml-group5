# This script is for the timing of kNN classification
# Created 20.02.2017
#----------------------------------------------------

if(!exists("M")) {
  M = loadSinglePersonsData(DPI_it, 'group5', 1, '../trunk/2017/')
}

system.time(replicate(10, knn(M_train, M_test, true_class_train, 200)))