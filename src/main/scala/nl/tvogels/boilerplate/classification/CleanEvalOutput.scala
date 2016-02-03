package nl.tvogels.boilerplate.classification

import nl.tvogels.boilerplate.Utilities.Util
import nl.tvogels.boilerplate.cleaneval.AlignCleanEvalData
import nl.tvogels.boilerplate.page.Segmenter
import nl.tvogels.boilerplate.page.Tree
import org.jsoup.Jsoup
import ch.ethz.dalab.dissolve.classification.{StructSVMModel,BinarySVMWithSSG, BinarySVMWithDBCFW}
import breeze.linalg
import ch.ethz.dalab.dissolve.optimization.{LocalBCFW,SolverOptions,DistBCFW}
import ch.ethz.dalab.dissolve.models.LinearChainCRF

/**
 * @author thijs

 */
object CleanEvalOutput {
  
  val files = AlignCleanEvalData.files
  val cleanevalDir = "/Users/thijs/dev/boilerplate/src/main/resources/cleaneval"
  
  val weightsurl = "/Users/thijs/Desktop/model.weights.txt"
  val model = {
    val weightsfile = new java.io.File(weightsurl)
    val weights = breeze.linalg.csvread(weightsfile).toDenseVector
    val crf = new LinearChainCRF(2, disablePairwise = true, useBPDecoding = false)
    new StructSVMModel(weights, 0.0, null, crf)
  }
  
  
  
  def classify(t: Tree): linalg.Vector[Double] = {
    val features = t.toLabeledDocument.pattern
    model.predict(features)
  }
  
  def main_predict(args: Array[String]) = {
    
    for (i <- files) {
      val orig = cleanevalDir + "/orig/" + i + ".html.utf8"
      Util.time {
        val content = scala.io.Source.fromFile(orig).mkString
        val dom = Jsoup.parse(content)
        Segmenter.parse(dom.body).map(tree => {
          Segmenter.prune(tree)
          val res = classify(tree)
          val l = tree.leaves
          var j = 0;
          l.foreach(t => { t.label = res(j).toInt; j = j+1 })
          val pars = tree.paragraphs.map(p => {
            val (start, end) = (p.nodeStats.startPosition, p.nodeStats.endPosition)
            val html = if (start != -1 && end != -1) content.slice(start, end) else null
            p.mainText
          }).toVector
          Util.save("/Users/thijs/Desktop/cleaneval/"+i+".txt", pars.mkString("\n\n"))
        })
      }
    }
  }
  
  
  def main_gold(args: Array[String]) = {
    import nl.tvogels.boilerplate.JsoupBenchmark
    for (i <- files) {
      val orig = cleanevalDir + "/orig/" + i + ".html.utf8"
      val content = scala.io.Source.fromFile(orig).mkString
      val tree = JsoupBenchmark.parseCleanEvalDocument(i)
      val res = classify(tree)
      val l = tree.leaves
      var j = 0;
      val pars = tree.paragraphs.map(p => {
        val (start, end) = (p.nodeStats.startPosition, p.nodeStats.endPosition)
        val html = if (start != -1 && end != -1) content.slice(start, end) else null
        p.mainText
      }).toVector
      Util.save("/Users/thijs/Desktop/cleaneval/"+i+".txt", pars.mkString("\n\n"))
    }
  }
  
  
  def main(args: Array[String]) = main_gold(args)
 
}