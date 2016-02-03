package nl.tvogels.boilerplate.classification

import breeze.linalg.Vector
import ch.ethz.dalab.dissolve.classification.{BinarySVMWithDBCFW,BinarySVMWithSSG}
import ch.ethz.dalab.dissolve.optimization.SolverOptions
import ch.ethz.dalab.dissolve.regression.LabeledObject
import nl.tvogels.boilerplate.page.{Block,Page}
import org.apache.spark.mllib.linalg.{Vectors}
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.{SparkConf,SparkContext}
import breeze.linalg.{DenseMatrix,DenseVector,Matrix,Vector,argmax,max,sum}
import org.apache.log4j.Logger
import org.apache.log4j.Level


object DissolveBinary {

  def loadData(patternsFilename: String, labelsFilename: String): 
      Array[LabeledPoint] = {
    
    val patterns: Array[String] = scala.io.Source
                                    .fromURL(getClass.getResource(patternsFilename))
                                    .getLines
                                    .toArray[String]
    val labels: Array[String] = scala.io.Source
                                    .fromURL(getClass.getResource(labelsFilename))
                                    .getLines
                                    .toArray[String]

    val n = labels.size

    assert(patterns.size == labels.size, "#Patterns=%d, but #Labels=%d".format(patterns.size, labels.size))

    def blocksForLine(patLine: List[Double], labLine: List[Double]) = {
      val (patNumRows, patNumCols): Pair[Int,Int] = (patLine(1).toInt, patLine(2).toInt)
      val labNumEles: Int = labLine(1).toInt
      assert(patNumCols == labNumEles, "pattern_i.cols == label_i.cols violated in data")
      val patVals: Array[Double] = 
        patLine.slice(3, patLine.size).toArray[Double]
      // The pixel values should be Column-major ordered
      val labVals: Array[Double] = labLine.slice(2, labLine.size).toArray[Double]
      for (i <- 0 until patNumCols) yield {
        val lab = if (labVals(i) == 1.0) 1.0 else -1.0
        val vec = Vectors.dense(patVals.slice(i*patNumRows, (i+1)*patNumRows))
        new LabeledPoint(lab, vec)
      } 
    }
    val data = for (i <- 0 until n;
                    pat = patterns(i).split(",").map(_.toDouble).toList;
                    lab = labels(i).split(",").map(_.toDouble).toList;
                    b  <- blocksForLine(pat,lab)) yield b
    
    data.toArray
  }
  
  def loadDataFromStart(): 
      Array[LabeledPoint] = {
    val data = DissolveUnary.loadData(-1)
    data.toScalaVector().flatMap(x => {
      x.pattern.blocks.zip(x.label.toArray).map {
        case (a: Block,b: Double) => new LabeledPoint((if (b == 1.0) 1.0 else -1.0),Vectors.dense(FeatureExtractor(a,x.pattern.source, x.pattern.dom).featureList.toArray))
      }
    }).toArray
  }
  
  def main(args: Array[String]) = {
    
    Logger.getLogger("ch.ethz.dalab.dissolve.optimization.LAdap$").setLevel(Level.OFF)
    Logger.getLogger("org").setLevel(Level.OFF)
    Logger.getLogger("akka").setLevel(Level.OFF)
    
    val PERC_TRAIN = 0.5
    val dataDir = "/dataset"
    val data: Array[LabeledPoint] = 
      loadData(dataDir + "/features"+FeatureExtractor.nFeatures+".csv", dataDir + "/labels.csv")
//    val data: Array[LabeledPoint] = loadDataFromStart()

    
//    val data = DissolveUnary.loadData(-1)
//    val blockData = data.toScalaVector().flatMap(x => {
//      x.pattern.blocks.zip(x.label.toArray).map {
//        case (a: Block,b: Double) => new LabeledPoint((if (b == 1.0) 1.0 else -1.0),Vectors.dense(FeatureExtractor(a,x.pattern.source, x.pattern.dom).featureList.toArray))
//      }
//    }).toArray
//    
//    
    val conf = new SparkConf().setAppName("Dissolve Binary").setMaster("local")
    val sc = new SparkContext(conf)
    sc.setCheckpointDir("checkpoint-files")
    
    val dataRDD = sc.parallelize(data)
    val rdds = dataRDD.randomSplit(Array(PERC_TRAIN,1.0-PERC_TRAIN))
    
    val solverOptions: SolverOptions[Vector[Double], Double] = new SolverOptions()
    
    solverOptions.roundLimit = 200
    solverOptions.debug = false
//    solverOptions.sampleFrac = 1.0
    solverOptions.lambda = 0.000001
    solverOptions.doWeightedAveraging = false
    solverOptions.doLineSearch = true
//    solverOptions.debug = true
    solverOptions.testDataRDD = Some(rdds(1).map(x => new LabeledObject(x.label, Vector(x.features.toArray))))
    
    solverOptions.enableOracleCache = false
    solverOptions.oracleCacheSize = 10
    solverOptions.ssg_gamma0 = 100
    
    solverOptions.debugInfoPath = "debug/debug-bcfw-%d.csv".format(System.currentTimeMillis())

    
    
    val model = BinarySVMWithDBCFW.train(rdds(0), solverOptions)
    
    
//    var avgTrainLoss: Double = 0.0
//    for (item <- rdds(0)) {
//      val prediction = model.predict(Vector(item.features.toArray))
//      avgTrainLoss += (if (prediction == item.label) 0.0 else 1.0)
//      // if (debugOn)
//      // println("Truth = %-10s\tPrediction = %-10s".format(labelVectorToString(item.label), labelVectorToString(prediction)))
//    }
//    println("Average loss on training set = %f".format(avgTrainLoss / rdds(0).count))
    
    val avgTrainLoss = data.map(x => {
      val prediction = model.predict(Vector(x.features.toArray))
      if (prediction == x.label) 0.0 else 1.0
    }).fold(0.0) {_+_}
    println("Average loss on training set = %f".format(avgTrainLoss / data.length))
    
//    val avgTestLoss = rdds(1).map(x => {
//      val prediction = model.predict(Vector(x.features.toArray))
//      if (prediction == x.label) 0.0 else 1.0
//    }).fold(0) {_+_}
//    println("Average loss on test set = %f".format(avgTestLoss / rdds(1).count))
  }
}