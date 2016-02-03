package nl.tvogels.boilerplate.classification

import breeze.linalg.Vector
import ch.ethz.dalab.dissolve.optimization.{LocalBCFW,SolverOptions,DistBCFW}
import ch.ethz.dalab.dissolve.classification.StructSVMModel
import ch.ethz.dalab.dissolve.models.LinearChainCRF
import ch.ethz.dalab.dissolve.regression.LabeledObject
import nl.tvogels.boilerplate.page.{Block,Page}
import org.apache.spark.mllib.linalg.{Vectors}
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.{SparkConf,SparkContext}
import breeze.linalg.{DenseMatrix,DenseVector,Matrix,Vector,argmax,max,sum}
import org.apache.log4j.Logger
import org.apache.log4j.Level
import nl.tvogels.boilerplate.JsoupBenchmark
import nl.tvogels.boilerplate.cleaneval.AlignCleanEvalData


object ChainCRF {

  def main(args: Array[String]) = {
    
    Logger.getLogger("ch.ethz.dalab.dissolve.optimization.LAdap$").setLevel(Level.OFF)
    Logger.getLogger("org").setLevel(Level.OFF)
    Logger.getLogger("akka").setLevel(Level.OFF)
    
    val PERC_TRAIN = 0.7
    val dataDir = "/dataset"

    val conf = new SparkConf().setAppName("Dissolve Chain CRF").setMaster("local")
    val sc = new SparkContext(conf)
    sc.setCheckpointDir("checkpoint-files")
    
    val dataRDD = sc.parallelize(AlignCleanEvalData.files)
      .map(JsoupBenchmark.parseCleanEvalDocument)
      .map(_.toLabeledDocument)
      
    println("COUNT", dataRDD.count)
    val rdds = dataRDD.randomSplit(Array(PERC_TRAIN,1.0-PERC_TRAIN))
    
    val numStates = 2
    val crfModel = new LinearChainCRF(numStates, disablePairwise = true, useBPDecoding = false)

    val solver = new DistBCFW(crfModel, debug = true, debugMultiplier = 0, lambda=0.000001)
    val model: StructSVMModel[Matrix[Double], Vector[Double]] = solver.train(rdds(0),rdds(1))
    
    // store the weights
    val weights = model.weights.toDenseVector.toDenseMatrix
    breeze.linalg.csvwrite(new java.io.File("/Users/thijs/Desktop/model.weights.txt"), weights)
    
  }
}