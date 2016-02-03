import numpy as np
import loader
import util
from sklearn import preprocessing


directory = "/Users/thijs/dev/boilerplate/src/main/resources/dataset/"
featureset = "features10"

print("Load files")
features, labels = \
  loader.loadBinary(featureset+'.csv', 'labels.csv', directory)

# print("Shuffle results")
# features, labels = util.shuffle(features, labels)

print("Loaded")
# print(labels)

# features = preprocessing.scale(features)


from pystruct.models import BinaryClf
from pystruct.learners import (NSlackSSVM, OneSlackSSVM,
                               SubgradientSSVM, FrankWolfeSSVM)
clf = FrankWolfeSSVM(BinaryClf(),verbose=True)
# print(clf)
clf.fit(features,labels)
trscore = clf.score(features,labels)

# print("Training score: {0}".format(trscore))
print("Klaar")