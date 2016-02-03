package nl.tvogels.boilerplate.classification

import ch.ethz.dalab.dissolve.optimization.{DissolveFunctions, SolverOptions}
import ch.ethz.dalab.dissolve.classification.{StructSVMModel, StructSVMWithBCFW}
import breeze.linalg.{DenseMatrix,DenseVector,Matrix,Vector,argmax,max,sum}
import ch.ethz.dalab.dissolve.regression.LabeledObject
import scala.util.Random
import nl.tvogels.boilerplate.cleaneval.AlignCleanEvalData
import nl.tvogels.boilerplate.Utilities.Util
import nl.tvogels.boilerplate.page.{Block,Page}

object DissolveUnary extends DissolveFunctions[Page, Vector[Double]] {
  
  val debugOn = true
  val nFeatures = FeatureExtractor.nFeatures
  val nLabels = 2
  override def numClasses(): Int = -1
  
  def loadData(n: Int = -1): DenseVector[LabeledObject[Page, Vector[Double]]] = {
    val items = if (n == -1) AlignCleanEvalData.files 
                else         AlignCleanEvalData.files.take(n) 
    val dir = "/cleaneval"
    val labeledObjects = items.toArray map { i => {
//      println(s"Loading file $i")
      val orig = Util.loadResourceWithoutFirstLine(s"$dir/orig/$i.html.utf8")
      val aligned = Util.loadResource(s"$dir/aligned/$i.txt.utf8")
      val p = Page.fromSource(orig)
      val res = p.blocks.toArray collect {
        case s if s.start != -1 && s.end != -1 => 
          (s,if (aligned.substring(s.start, s.end).toString matches ".*[A-Z].*") 1d else 0d)
      }
      val (blocks: Seq[Block], labels: Seq[Double]) = res.unzip
      new LabeledObject(Vector(labels.toArray), Page(p.dom,p.source,blocks.toVector))
    }}
    DenseVector(labeledObjects)
  }
  
  def featureFn(p: Page, y: Vector[Double]): Vector[Double] = {
    val n = DenseVector.zeros[Double](nFeatures)
    var out = DenseVector.zeros[Double](nLabels*nFeatures)
    val m = p.featureMatrix
    for (label <- 0 until nLabels;
         ind <- 0 until m.cols 
         if y(ind) == label.toDouble) {
      out(label*nFeatures until ((label+1)*nFeatures)) += m(::,ind)
    }
//    for (i <- 0 until m.cols) {
//      val idx = (y(i).toInt * nFeatures)
//      out((idx) until (idx + nFeatures)) :=
//        out((idx) until (idx + nFeatures)) + m(::, i)
//    }
//    println(out)  
    out
  }
  
  def lossFn(yTruth: Vector[Double], yPredict: Vector[Double]): Double =
    sum((yTruth :== yPredict).map(x => if (x) 0 else 1)) / yTruth.size.toDouble
//  def lossFn(yTruth: Vector[Double], yPredict: Vector[Double]): Double =
//    sum((yTruth :== yPredict).map(x => if (x) 0 else 1))
//  def lossFn(yTruth: Vector[Double], yPredict: Vector[Double]): Double = 0.0

  
  def rowwiseMax(matM: Matrix[Double]): DenseMatrix[Double] = {
    val mat: DenseMatrix[Double] = matM.toDenseMatrix
    val rowMax: DenseMatrix[Double] = DenseMatrix.zeros[Double](2, mat.rows)

    for (row <- 0 until mat.rows) {
      // 1st row contains max
      rowMax(0, row) = max(mat(row, ::))
      // 2nd row contains indices of the max
      rowMax(1, row) = argmax(mat(row, ::))
    }
    rowMax
  }
  
  def columnwiseMax(matM: Matrix[Double]): DenseMatrix[Double] = {
    val mat: DenseMatrix[Double] = matM.toDenseMatrix
    val colMax: DenseMatrix[Double] = DenseMatrix.zeros[Double](2, mat.cols)

    for (col <- 0 until mat.cols) {
      // 1st row contains max
      colMax(0, col) = max(mat(::, col))
      // 2nd row contains indices of the max
      colMax(1, col) = argmax(mat(::, col))
    }
    colMax
  }

    
  def logDecode(logNodePotMat: Matrix[Double]): Vector[Double] = {
    val col = rowwiseMax(logNodePotMat)
    col(1,::).t
  }
  
//  def logDecode(logNodePotMat: Matrix[Double], logEdgePotMat: Matrix[Double]): Vector[Double] = {
//
//    val logNodePot: DenseMatrix[Double] = logNodePotMat.toDenseMatrix
//    val logEdgePot: DenseMatrix[Double] = logEdgePotMat.toDenseMatrix
//
//    val nNodes: Int = logNodePot.rows
//    val nStates: Int = logNodePot.cols
//
//    /*--- Forward pass ---*/
//    val alpha: DenseMatrix[Double] = DenseMatrix.zeros[Double](nNodes, nStates) // nx26 matrix
//    val mxState: DenseMatrix[Double] = DenseMatrix.zeros[Double](nNodes, nStates) // nx26 matrix
//    alpha(0, ::) := logNodePot(0, ::)
//    for (n <- 1 until nNodes) {
//      /* Equivalent to `tmp = repmat(alpha(n-1, :)', 1, nStates) + logEdgePot` */
//      // Create an empty 26x26 repmat term
//      val alphaRepmat: DenseMatrix[Double] = DenseMatrix.zeros[Double](nStates, nStates)
//      for (col <- 0 until nStates) {
//        // Take the (n-1)th row from alpha and represent it as a column in repMat
//        // alpha(n-1, ::) returns a Transposed view, so use the below workaround
//        alphaRepmat(::, col) := alpha.t(::, n - 1)
//      }
//      val tmp: DenseMatrix[Double] = alphaRepmat + logEdgePot
//      val colMaxTmp: DenseMatrix[Double] = columnwiseMax(tmp)
//      alpha(n, ::) := logNodePot(n, ::) + colMaxTmp(0, ::)
//      mxState(n, ::) := colMaxTmp(1, ::)
//    }
//    /*--- Backward pass ---*/
//    val y: DenseVector[Double] = DenseVector.zeros[Double](nNodes)
//    // [dummy, y(nNodes)] = max(alpha(nNodes, :))
//    y(nNodes - 1) = argmax(alpha.t(::, nNodes - 1).toDenseVector)
//    for (n <- nNodes - 2 to 0 by -1) {
//      y(n) = mxState(n + 1, y(n + 1).toInt)
//    }
//    y
//  }
  
  def oracleFnWithDecode(model: StructSVMModel[Page, Vector[Double]], p: Page, yi: Vector[Double],
                         decodeFn: Matrix[Double] => Vector[Double]): Vector[Double] = {
    val weight = model.getWeights().toDenseVector.toDenseMatrix.reshape(nFeatures,nLabels)
    
    val x: DenseMatrix[Double] = p.featureMatrix
    val thetaUnary: DenseMatrix[Double] = weight.t * x
    
    val numBlocks = p.blocks.length
    
    if (yi != null) { // loss augmentation is only done if a label yi is given. 
      val l: Int = yi.size
      for (i <- 0 until numBlocks) {
        thetaUnary(::, i) := thetaUnary(::, i) + 1.0 / l
        val idx = yi(i).toInt // Loss augmentation
        thetaUnary(idx, i) = thetaUnary(idx, i) - 1.0 / l
      }
    }
    
    val d = decodeFn(thetaUnary.t)
//    Thread.sleep(100)
    d
  }
  
  override def oracleFn(model: StructSVMModel[Page, Vector[Double]], xi: Page, yi: Vector[Double]): Vector[Double] =
    oracleFnWithDecode(model, xi, yi, logDecode)
  
  def predictFn(model: StructSVMModel[Page, Vector[Double]], xi: Page): Vector[Double] =
    oracleFn(model, xi, null)
    
  def splitData(data: Vector[LabeledObject[Page, Vector[Double]]], split_percentage: Double) = {
    val ind = Random.shuffle(1 to data.size-1)
    val train_ind = ind.slice(0,(split_percentage * data.size).toInt)
    val test_ind = ind.slice((split_percentage * data.size).toInt,data.size)
    val train_data: Array[LabeledObject[Page, Vector[Double]]] = data(train_ind).toArray
    val test_data: Array[LabeledObject[Page, Vector[Double]]] = data(test_ind).toArray
    (train_data,test_data)
  }
    
  def main(args: Array[String]): Unit = {

    val PERC_TRAIN: Double = 0.3

    val dataDir: String = "../data/generated";
    
    println("Loading and parsing files.")
    val startTime = System.nanoTime()
    val data: Vector[LabeledObject[Page, Vector[Double]]] = loadData(-1)
    def elapsed = ((System.nanoTime() - startTime) / 1000000000.0)
    println("Time to load %d files: %2.2fs".format(data.size,elapsed))
    
    
//    data.toArray.foreach {
//      case LabeledObject(l: Vector[Double], p: Page) => {
//        println("________");
//        println("________");
//        println("________");
//        p.blocks.zip(l.toArray).foreach {
//          case (block, label) => {
//            println(label.toInt + " : " + block.text)
//          }
//        }
//        Thread.sleep(4000)
//      }
//    }
//    
    
    val (
      train_data:Array[LabeledObject[Page, Vector[Double]]], 
      test_data:Array[LabeledObject[Page, Vector[Double]]]
    ) = splitData(data, PERC_TRAIN)
    if (false) {
    	println("Running chainBCFW (single worker). Loaded %d training examples, and labels:%dx1"
        .format(train_data.size,
          train_data(0).label.size))
      println("Running chainBCFW (single worker). Loaded %d test examples, and labels:%dx1"
        .format(test_data.size,
          test_data(0).label.size))
    }
    
   
//    val conf = new SparkConf()
//         .setAppName("Dissolve for main story extraction")
//         .setMaster("local[4]")
//         
//    val sc = new SparkContext(conf)
//    sc.setCheckpointDir("checkpoint-files")
//    println(SolverUtils.getSparkConfString(sc.getConf))
    

    val solverOptions: SolverOptions[Page, Vector[Double]] = new SolverOptions()
    solverOptions.roundLimit = 5
    solverOptions.debug = true
    solverOptions.lambda = 0.0000001
    solverOptions.doWeightedAveraging = false
    solverOptions.doLineSearch = true
    solverOptions.debug = true
    solverOptions.testData = Some(test_data)
//    solverOptions.testDataRDD = 
//      if(solverOptions.enableManualPartitionSize)
//        Some(sc.parallelize(test_data.toSeq, solverOptions.NUM_PART))
//      else
//        Some(sc.parallelize(test_data.toSeq))
        
//    val trainDataRDD = 
//      if(solverOptions.enableManualPartitionSize)
//        sc.parallelize(train_data.toSeq, solverOptions.NUM_PART)
//      else
//        sc.parallelize(train_data.toSeq)
    
    solverOptions.enableOracleCache = false
    solverOptions.oracleCacheSize = 10
    
    solverOptions.debugInfoPath = "debug/debug-bcfw-%d.csv".format(System.currentTimeMillis())

    val trainer: StructSVMWithBCFW[Page, Vector[Double]] = 
      new StructSVMWithBCFW[Page, Vector[Double]](
        train_data,
        DissolveUnary,
        solverOptions
      )

    val model: StructSVMModel[Page, Vector[Double]] = trainer.trainModel

    var avgTrainLoss: Double = 0.0
    for (item <- train_data) {
      val prediction = model.predict(item.pattern)
      avgTrainLoss += DissolveUnary.lossFn(item.label, prediction)
      // if (debugOn)
      // println("Truth = %-10s\tPrediction = %-10s".format(labelVectorToString(item.label), labelVectorToString(prediction)))
    }
    println("Average loss on training set = %f".format(avgTrainLoss / train_data.size))

    var avgTestLoss: Double = 0.0
    for (item <- test_data) {
      val prediction = model.predict(item.pattern)
      avgTestLoss += DissolveUnary.lossFn(item.label, prediction)
      // if (debugOn)
      // println("Truth = %-10s\tPrediction = %-10s".format(labelVectorToString(item.label), labelVectorToString(prediction)))
    }
    println("Average loss on test set = %f".format(avgTestLoss / test_data.size))
  }
}