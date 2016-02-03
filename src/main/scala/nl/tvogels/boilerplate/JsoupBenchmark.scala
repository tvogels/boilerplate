package nl.tvogels.boilerplate

import nl.tvogels.boilerplate.cleaneval.AlignCleanEvalData
import nl.tvogels.boilerplate.Utilities.Util
import org.jsoup.Jsoup
import scala.math.sqrt
import scala.collection.JavaConversions._
import org.jsoup.nodes.{Element,Node,TextNode}
import scala.collection.mutable.Stack
import nl.tvogels.boilerplate.page.Segmenter
import scala.collection.mutable.Buffer
import nl.tvogels.boilerplate.classification.{LanguageDependentSettings => s}
import nl.tvogels.boilerplate.Utilities.Util
import scala.collection.immutable.ListMap
import org.apache.spark.mllib.linalg.{Vectors}
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.{SparkConf,SparkContext}
import org.apache.spark.mllib.util.MLUtils
import org.apache.spark.rdd.RDD
import nl.tvogels.boilerplate.page.{Tree,Leaf,Segmenter}


object JsoupBenchmark {
  
  val dir = "/cleaneval"
  val docs = AlignCleanEvalData.files
  
  def maskLabel(tree: Tree, aligned: String): Unit = tree match {
    case Leaf(l) => {
      val start = l.nodeStats.startPosition
      val end = l.nodeStats.endPosition
      if (start != -1 && end != -1) {
        l.label = (if (aligned.substring(start, end).toString matches ".*[A-Z].*") 1 else 0)
      } else {
        println("HELP, -1")
        println(l.dom.last.nodeName)
        println(l)
        println(l.dom.last.startPosition, l.dom.last.endPosition)
      }
    }
    case t => {
      t.children.foreach(c => maskLabel(c, aligned))
    }
  }
    
  
  def parseCleanEvalDocument(i: Int): Tree = {
    val orig = Util.loadResourceWithoutFirstLine(s"$dir/orig/$i.html.utf8")
    val aligned = Util.loadResource(s"$dir/aligned/$i.txt.utf8")
    val parsed = Jsoup.parse(orig)
    val tree = Segmenter.parse(parsed.body).get
    Segmenter.prune(tree)
    maskLabel(tree, aligned)
    tree
  }
  
  
  def saveAsLibSVM(dataRDD: RDD[LabeledPoint]) = {
    val splits = dataRDD.randomSplit(Array(0.2,0.8), 103)
    MLUtils.saveAsLibSVMFile(splits(0), "/Users/thijs/Desktop/training")
    MLUtils.saveAsLibSVMFile(splits(1), "/Users/thijs/Desktop/testing")
  }
  
  def writeFlatFeatures(docRDD: RDD[Tree]) = {
    val writer = new java.io.PrintWriter(new java.io.File("/Users/thijs/Desktop/features.csv"))
    var headerDone = false;
    for (doc <- docRDD.collect; t   <- doc.leaves) {
     if (!headerDone) {
        headerDone = true
        writer.write("label\t" + t.allCombinedFeatures.map(x => x._1).mkString("\t") + "\n")
      }
      writer.write(t.label + "\t" + t.allCombinedFeatures.map(x => x._2).mkString("\t") + "\n") 
    }

  }
  

  def main(args: Array[String]) = {
   
    val conf = new SparkConf().setAppName("Dissolve Binary").setMaster("local")
    val sc = new SparkContext(conf)
    sc.setCheckpointDir("checkpoint-files")
      
    val docsRDD = sc.parallelize(docs)
    val docRDD = docsRDD.map(parseCleanEvalDocument)
    val dataRDD = docRDD.flatMap(_.toLabeledPoints)
      
    // Count zeros and ones, for statistics
    val ones = dataRDD.filter(i => i.label == 1).count
    val zeros = dataRDD.filter(i => i.label != 1).count
    println(ones,zeros)
    
  }
  
}
