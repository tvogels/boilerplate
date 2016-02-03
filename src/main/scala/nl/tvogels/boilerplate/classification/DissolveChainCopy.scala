package nl.tvogels.boilerplate.classification

import org.apache.log4j.PropertyConfigurator
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import breeze.linalg.{DenseMatrix,DenseVector,Matrix,Vector,argmax,max,sum}
import ch.ethz.dalab.dissolve.classification.{StructSVMModel,StructSVMWithBCFW,StructSVMWithDBCFW,StructSVMWithSSG}
import ch.ethz.dalab.dissolve.optimization.{LocalBCFW,DissolveFunctions, SolverOptions, SolverUtils}
import ch.ethz.dalab.dissolve.regression.LabeledObject
import ch.ethz.dalab.dissolve.utils.cli.CLAParser
import ch.ethz.dalab.dissolve.models.LinearChainCRF
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

/**
 * How to generate the input data:
 * While in the data directory, run
 * python convert-ocr-data.py
 *
 */
object DissolveChainCopy {

  val ENABLE_PERF_METRICS = false
  val disablePairwise = false
  val useBPDecoding = false
  val numStates = 2
  val debugOn = false
  
  /**
   * Reads data produced by the convert-ocr-data.py script and loads into memory as a vector of Labeled objects
   *
   *  TODO
   *  * Take foldNumber as a parameter and return training and test set
   */
  def loadData(patternsFilename: String, labelsFilename: String): Array[LabeledObject[Matrix[Double], Vector[Double]]] = {
    val patterns: Array[String] = scala.io.Source.fromURL(getClass.getResource(patternsFilename)).getLines().toArray[String]
    val labels: Array[String] = scala.io.Source.fromURL(getClass.getResource(labelsFilename)).getLines().toArray[String]

    val n = labels.size

    assert(patterns.size == labels.size, "#Patterns=%d, but #Labels=%d".format(patterns.size, labels.size))

    val data: Array[LabeledObject[Matrix[Double], Vector[Double]]] = Array.fill(n) { null }

    for (i <- 0 until n) {
      // Expected format: id, #rows, #cols, (pixels_i_j,)* pixels_n_m
      val patLine: List[Double] = patterns(i).split(",").map(x => x.toDouble) toList
      // Expected format: id, #letters, (letters_i)* letters_n
      val labLine: List[Double] = labels(i).split(",").map(x => x.toDouble) toList

      val patNumRows: Int = patLine(1) toInt
      val patNumCols: Int = patLine(2) toInt
      val labNumEles: Int = labLine(1) toInt

      assert(patNumCols == labNumEles, "pattern_i.cols == label_i.cols violated in data")

      val patVals: Array[Double] = patLine.slice(3, patLine.size).toArray[Double]
      // The pixel values should be Column-major ordered
      val thisPattern: DenseMatrix[Double] = DenseVector(patVals).toDenseMatrix.reshape(patNumRows, patNumCols)

      val labVals: Array[Double] = labLine.slice(2, labLine.size).toArray[Double]
      assert(List.fromArray(labVals).count(x => x < 0 || x > 1) == 0, "Elements in Labels should be in the range [0, 1]")
      val thisLabel: DenseVector[Double] = DenseVector(labVals)

      assert(thisPattern.cols == thisLabel.size, "pattern_i.cols == label_i.cols violated in Matrix representation")

      data(i) = new LabeledObject(thisLabel, thisPattern)

    }

    data
  }
  
  def chainBCFW(): Unit = {

    val dataDir = "/dataset/"
    
    val percTrain = 0.7

    val data: Array[LabeledObject[Matrix[Double], Vector[Double]]] =
      loadData(dataDir + "features3.csv", dataDir + "labels.csv")
    val n = data.size

    val train_data = data.slice(0, (percTrain * n).toInt)
    val test_data = data.slice((percTrain * n).toInt, n)

    println("Running chainBCFW (single worker). Loaded %d training examples, pattern:%dx%d and labels:%dx1"
      .format(train_data.size,
        train_data(0).pattern.rows,
        train_data(0).pattern.cols,
        train_data(0).label.size))
        

    
    val crfModel = new LinearChainCRF
    
    
<<<<<<< HEAD
//    val solver = new LocalBCFW(crfModel, numPasses = 10, debug = true, debugMultiplier = 0)
//    val model: StructSVMModel[Matrix[Double], Vector[Double]] = solver.train(train_data, test_data)
    
=======
    val thetaUnary: DenseMatrix[Double] = weight.unary.t * xi // Produces a 26 x (length-of-chain) matrix

    // First position has a bias
    thetaUnary(::, 0) := thetaUnary(::, 0) + weight.firstBias
    // Last position has a bias
    thetaUnary(::, -1) := thetaUnary(::, -1) + weight.lastBias

    val thetaPairwise: DenseMatrix[Double] = weight.pairwise

    // Add loss-augmentation to the score (normalized Hamming distances used for loss)
    if (yi != null) { // loss augmentation is only done if a label yi is given. 
      val l: Int = yi.size
      for (i <- 0 until numVars) {
        thetaUnary(::, i) := thetaUnary(::, i) + 1.0 / l
        val idx = yi(i).toInt // Loss augmentation
        thetaUnary(idx, i) = thetaUnary(idx, i) - 1.0 / l
      }
    }

    // Solve inference problem
    val label: Vector[Double] = decodeFn(thetaUnary.t, thetaPairwise) // - 1.0
    if (yi != null) {
      yypair = (xi,label,yi)
//      println("|<w,psi-psi>| before update = "+model.getWeights.dot(featureFn(xi,label)-featureFn(xi,yi)))
    } else {
      yypair = null
    }
    label
  }

  /**
   * Readable representation of the weight vector
   */
  class Weight(
    val unary: DenseMatrix[Double],
    val firstBias: DenseVector[Double],
    val lastBias: DenseVector[Double],
    val pairwise: DenseMatrix[Double]) {
  }

  /**
   * Converts weight vector into Weight object, by partitioning it into unary, bias and pairwise terms
   */
  def weightVecToObj(weightVec: Vector[Double], numStates: Int, numDims: Int): Weight = {
    val idx: Int = numStates * numDims

    val unary: DenseMatrix[Double] = weightVec(0 until idx)
      .toDenseVector
      .toDenseMatrix
      .reshape(numDims, numStates)
    val firstBias: Vector[Double] = weightVec(idx until (idx + numStates))
    val lastBias: Vector[Double] = weightVec((idx + numStates) until (idx + 2 * numStates))
    val pairwise: DenseMatrix[Double] = weightVec((idx + 2 * numStates) until weightVec.size)
      .toDenseVector
      .toDenseMatrix
      .reshape(numStates, numStates)

    new Weight(unary, firstBias.toDenseVector, lastBias.toDenseVector, pairwise)
  }

  override def oracleFn(model: StructSVMModel[Matrix[Double], Vector[Double]], xi: Matrix[Double], yi: Vector[Double]): Vector[Double] =
    oracleFnWithDecode(model, xi, yi, logDecode)

  /**
   * Predict function.
   * This is (non-loss-augmented) decoding
   *
   */
  def predictFn(model: StructSVMModel[Matrix[Double], Vector[Double]], xi: Matrix[Double]): Vector[Double] = {
    val label: Vector[Double] = oracleFn(model, xi, null)

    label
  }

  /**
   * Convert Vector[Double] to respective String representation
   */
  def labelVectorToString(vec: Vector[Double]): String =
    if (vec.size == 0)
      ""
    else if (vec.size == 1)
      (vec(0).toInt + 97).toChar + ""
    else
      (vec(0).toInt + 97).toChar + labelVectorToString(vec(1 until vec.size))

  /**
   * ****************************************************************
   *    ___   _____ ____ _      __
   *   / _ ) / ___// __/| | /| / /
   *  / _  |/ /__ / _/  | |/ |/ /
   * /____/ \___//_/    |__/|__/
   *
   * ****************************************************************
   */
  def chainBCFW(): Unit = {

    val PERC_TRAIN = 0.7
    val dataDir = "/dataset"
    val dataUnord: Vector[LabeledObject[Matrix[Double], Vector[Double]]] = 
      loadData(dataDir + "/features"+FeatureExtractor.nFeatures+".csv", dataDir + "/labels.csv")
    //val perm = scala.util.Random.shuffle((0 until dataUnord.size).toList)
    val perm = (0 until dataUnord.size).toList
    val train_data: Array[LabeledObject[Matrix[Double], Vector[Double]]] = 
      dataUnord(perm.slice(0, (PERC_TRAIN * dataUnord.size).toInt)).toArray
    val test_data: Array[LabeledObject[Matrix[Double], Vector[Double]]] = 
      dataUnord(perm.slice((PERC_TRAIN * train_data.size).toInt,dataUnord.length)).toArray
      
    // val train_data = train_data_unord(List.fromArray(perm))
    // val temp: DenseVector[LabeledObject] = train_data_unord(List.fromArray(perm).slice(0, 1)).toDenseVector
    // val train_data = DenseVector.fill(5){temp(0)}

    if (debugOn) {
      println("Running chainBCFW (single worker). Loaded %d training examples, pattern:%dx%d and labels:%dx1"
        .format(train_data.size,
          train_data(0).pattern.rows,
          train_data(0).pattern.cols,
          train_data(0).label.size))
      println("Running chainBCFW (single worker). Loaded %d test examples, pattern:%dx%d and labels:%dx1"
        .format(test_data.size,
          test_data(0).pattern.rows,
          test_data(0).pattern.cols,
          test_data(0).label.size))
    }

>>>>>>> 33f845f207cc62c4c63a9d2d1919f62934a07689
    val solverOptions: SolverOptions[Matrix[Double], Vector[Double]] = new SolverOptions()
    solverOptions.roundLimit = 100
    solverOptions.debug = true
    solverOptions.debugMultiplier = 100
    solverOptions.lambda = 0.01
    solverOptions.doWeightedAveraging = false
    solverOptions.doLineSearch = true
    solverOptions.testData = Some(test_data.toList)
    solverOptions.enableOracleCache = false
    solverOptions.oracleCacheSize = 10
    solverOptions.debugInfoPath = "debug/debug-bcfw-%d.csv".format(System.currentTimeMillis())
    
<<<<<<< HEAD
    val trainer: StructSVMWithBCFW[Matrix[Double],Vector[Double]] = new StructSVMWithBCFW[Matrix[Double],Vector[Double]](
        train_data.toList, crfModel, solverOptions)
=======
    /*val trainer: StructSVMWithSSG = new StructSVMWithSSG(train_data,
      featureFn,
      lossFn,
      oracleFn,
      predictFn,
      solverOptions)*/


    val trainer: StructSVMWithBCFW[Matrix[Double],Vector[Double]] = new StructSVMWithBCFW[Matrix[Double],Vector[Double]](
                    train_data, DissolveChainCopy, solverOptions)
    // gives super bad misclassification error 0.6

    //val trainer: StructSVMWithSSG[Matrix[Double],Vector[Double]] = new StructSVMWithSSG[Matrix[Double],Vector[Double]](
      //  train_data,DissolveChainCopy, solverOptions)
    // gives ok misclassification error ~0.24

>>>>>>> 33f845f207cc62c4c63a9d2d1919f62934a07689
    val model: StructSVMModel[Matrix[Double], Vector[Double]] = trainer.trainModel()
    
<<<<<<< HEAD
    println("weightVec.size = %d, x_i.size = %d".format(model.getWeights().size, train_data(0).pattern.rows))
=======
    val PERC_TRAIN = 0.7
    val dataDir = "/dataset"
    val dataUnord: Vector[LabeledObject[Matrix[Double], Vector[Double]]] = 
      loadData(dataDir + "/features"+FeatureExtractor.nFeatures+".csv", dataDir + "/labels.csv")
    val perm = (0 until dataUnord.size).toList
    val train_data: Array[LabeledObject[Matrix[Double], Vector[Double]]] = 
      dataUnord(perm.slice(0, (PERC_TRAIN * dataUnord.size).toInt)).toArray
    val test_data: Array[LabeledObject[Matrix[Double], Vector[Double]]] = 
      dataUnord(perm.slice((PERC_TRAIN * train_data.size).toInt,dataUnord.length)).toArray
>>>>>>> 33f845f207cc62c4c63a9d2d1919f62934a07689
    
    
  }
  
  def main(args: Array[String]): Unit = {
    PropertyConfigurator.configure("conf/log4j.properties")
    
    System.setProperty("spark.akka.frameSize", "512")
    
    chainBCFW()
  }

}