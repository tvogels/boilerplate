import random
import numpy as np
import util
import loader
from pystruct.models import ChainCRF
from pystruct.learners import (NSlackSSVM, OneSlackSSVM,
                               SubgradientSSVM, FrankWolfeSSVM)

directory = "/Users/thijs/dev/boilerplate/src/main/resources/dataset/"
featureset = "features3"

print("Load files")
features, labels = \
  loader.load(featureset+'.csv', 'labels.csv', directory)

# print("Shuffle results")
# features, labels = util.shuffle(features, labels)


trsize = int(0.7*len(labels))
X_train = features[1:trsize]
y_train = labels[1:trsize]

X_test = features[trsize+1:]
y_test = labels[trsize+1:]

# X_train = X_test = features
# y_train = y_test = labels
# trsize = len(labels)

# Evaluate the chain
model = ChainCRF()
C=0.0001
max_iter=50
ssvm = FrankWolfeSSVM(model=model, C=C, max_iter=max_iter, verbose=True)
print(ssvm)
print(ssvm.fit(X_train, y_train))
print(ssvm.w)
trscore = ssvm.score(X_train,y_train)
# testscore = ssvm.score(X_test,y_test)
print("Training score: {0}".format(trscore))
# print("Test score: {0}".format(testscore))

# Save the result
# util.saveToSQL(featureset, C, max_iter, trsize, trscore, 2)
