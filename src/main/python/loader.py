import numpy as np

def load(featuresFile, labelsFile, directory):

  try:
    f,l = open(directory+featuresFile, 'r'), open(directory+labelsFile, 'r')

    labels = [np.array([int(float(x)) for x in line.split(",")][2:])
              for line in l]

    def parseFeature(l):
      line = [float(x) for x in l.split(",")]
      nrows, ncols = int(line[1]), int(line[2])
      data = line[3:]
      a = np.reshape(np.array(data),(ncols,nrows))
      # m = np.array([148.54839767, 0.31187591, 0.18529706])
      # sd = np.array([4.04390621e+02, 4.48327425e-01, 1.94394022e-01])
      return a

    features = [parseFeature(line) for line in f]

  finally:
    f.close()
    l.close()

  assert(len(features) == len(labels))

  return features, labels

def loadBinary(featuresFile, labelsFile, directory):

  features, labels = load(featuresFile, labelsFile, directory)

  f2 = np.array([x
        for f in features
        for x in f])

  l2 = np.array([[1.0 if (x==1) else -1.0]
        for l in labels
        for x in l])

  # print(f2.shape, l2.shape)
  assert(f2.shape[0] == l2.shape[0])

  return f2, l2