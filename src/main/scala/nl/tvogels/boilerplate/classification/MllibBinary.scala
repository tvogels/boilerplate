package nl.tvogels.boilerplate.classification

import org.apache.spark.mllib.classification.{SVMModel, SVMWithSGD}
import org.apache.spark.mllib.evaluation.BinaryClassificationMetrics
import org.apache.spark.mllib.util.MLUtils
import org.apache.spark.{SparkConf,SparkContext}
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.linalg.{Vectors}
import org.apache.spark.rdd.RDD

import nl.tvogels.boilerplate.page.{Block,Page}
import nl.tvogels.boilerplate.cleaneval.AlignCleanEvalData
import nl.tvogels.boilerplate.Utilities.Util

object MllibBinary extends App {
    
  val conf = new SparkConf().setAppName("MlLib Binary").setMaster("local")
  val sc = new SparkContext(conf)
  sc.setCheckpointDir("checkpoint-files")
  
  def loadData(): RDD[LabeledPoint] = {
    val items = sc.parallelize(AlignCleanEvalData.files)
    val dir = "/cleaneval"
    items.flatMap { i => {
//      println(s"Loading file $i")
      val orig = Util.loadResourceWithoutFirstLine(s"$dir/orig/$i.html.utf8")
      val aligned = Util.loadResource(s"$dir/aligned/$i.txt.utf8")
      val p = Page.fromSource(orig)
      val res = p.blocks.toArray collect {
        case s if s.start != -1 && s.end != -1 => 
          (s,if (aligned.substring(s.start, s.end).toString matches ".*[A-Z].*") 1d else 0d)
      }
      res.map {
        case (block,label) => new LabeledPoint(label, Vectors.dense(FeatureExtractor(block,p.source, p.dom).featureList.toArray)) 
      }
    }}
  }
  
  val data = loadData
  
  MLUtils.saveAsLibSVMFile(data, "/Users/thijs/Desktop/data")
  
  val splits = data.randomSplit(Array(0.6, 0.4), seed = 10L)
  val training = splits(0).cache()
  val test = splits(1)
  
  val svmAlg = new SVMWithSGD()
  svmAlg.optimizer.
    setNumIterations(200).
    setRegParam(0.00001)
  val model = svmAlg.run(training)

  val scoreAndLabels = test.map { point =>
    val score = model.predict(point.features)
    (score, point.label)
  }
  
  val testScore = test.map {
    point => if ((model.predict(point.features) > 0) == (point.label==1.0)) 1 else 0
  }.reduce(_+_) / test.count.toDouble
  
  val trainScore = training.map {
    point => if ((model.predict(point.features) > 0) == (point.label==1.0)) 1 else 0
  }.reduce(_+_) / training.count.toDouble
  
  println(scoreAndLabels.collect().toList)

//  model.clearThreshold()
//  val metrics = new BinaryClassificationMetrics(scoreAndLabels)
//  val auROC = metrics.areaUnderROC()
//
//  println("Area under ROC = " + auROC)
  println("Training score = " + trainScore)
  println("Test score = " + testScore)
  
  
}