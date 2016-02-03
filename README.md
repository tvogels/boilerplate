## Setup 

Create an Eclipse environment with

```
sbt -with-bundled-scala-containers=false eclipse
```

and make sure Eclipse is configured to use Scala 2.10

## Dissolve implementation

There are three implementations in `nl.tvogels.boilerplate.classification`:

* __DissolveBinary:__ works fine, and uses dissolve's `BinarySVMWithDBCFW`.
* __DissolveChainCopy:__ close copy of `ch.ethz.dalab.dissolve.examples.ChainDemo`, with my data. Shows strange behaviour.
* __DissolveUnary:__ my own variation on the *ChainDemo*, with pairwise features removed.

The codes can be tested with one feature or 21 features. To switch, change the `featureList` property of `nl.tvogels.boilerplate.classification.FeatureExtractor` and change the static `nFeatures` property of the same class.

