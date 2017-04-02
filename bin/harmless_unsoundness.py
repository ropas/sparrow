#!/usr/bin/python
#########################################################################
##                                                                     ##
## Copyright (c) 2007-present.                                         ##
## Programming Research Laboratory (ROPAS), Seoul National University. ##
## All rights reserved.                                                ##
##                                                                     ##
## This software is distributed under the term of the BSD license.     ##
## See the LICENSE file for details.                                   ##
##                                                                     ##
#########################################################################
import sys, os, re
from sklearn import svm
import numpy as np 
import pickle

sparrow_data_path = os.environ['SPARROW_DATA_PATH'] 

def mkTrSet(fname):
  f = open(fname,"r")
  lines = f.readlines()
  trset = []
  for line in lines:
    feat = []
    tokens = re.split(r' |\t|\n',line)
    tokens = filter(lambda x: x.strip() != '', tokens)
    if int(tokens[len(tokens)-1]) == 1:   
      tokens = tokens[1:len(tokens)-1]
      for b in tokens:
        feat.append(float(b))
      trset.append(feat)
  return trset

def doOCSVM(trset):
  # fit the model
  clf = svm.OneClassSVM(nu=0.1, kernel="rbf", gamma=0.1)
  clf.fit(trset)
  return clf

def training():
  loop_training = sparrow_data_path + "/unsound_loop.dat" 
  lib_training = sparrow_data_path + "/data/unsound_lib.dat" 
  loop_trset = mkTrSet(loop_training)
  lib_trset = mkTrSet(lib_training)
  clf_loop = doOCSVM(loop_trset)
  clf_lib = doOCSVM(lib_trset)
  return (clf_loop, clf_lib)

def is_harmless(clf, vec):
  sys.stdout.flush()
  result = clf.predict(np.array(vec).reshape(1,-1))
  if result == 1: 
    return True
  else:
    return False

def store(clf, name):
  with open(name, 'w') as f:    
    pickle.dump(clf, f)

def load(name):
  with open(name, 'r') as f:
    clf = pickle.load(f)
  return clf

def main(argv):
  (clf_loop, clf_lib) = training()
  store(clf_loop, sparrow_data_path + "harmless_loop_clf") 
  store(clf_lib, sparrow_data_path + "harmless_lib_clf") 
   
if __name__ == "__main__":
  main(sys.argv[1:])
