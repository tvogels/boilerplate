package nl.tvogels.boilerplate.page


// this is old stuff

import org.jsoup.nodes.Document
import org.jsoup.Jsoup
import breeze.linalg.{DenseMatrix,DenseVector}
import nl.tvogels.boilerplate.classification.{FeatureExtractor,DissolveUnary}

case class Page(dom: Document, source: String, blocks: Vector[Block]) {
  val featureMatrix: DenseMatrix[Double] = {
    var m = DenseMatrix.zeros[Double](FeatureExtractor.nFeatures,blocks.length)
    for (i <- 0 until blocks.length) {
      val v: DenseVector[Double] = DenseVector(FeatureExtractor(blocks(i),source,dom).featureList)
      m(::,i) := v
    }
    m
  } 
}

object Page {
  
  def fromSource(source: String): Page = {
    val doc = Jsoup.parse(source)
    val blocks = Segmenter.segmentation(doc)
    Page(doc, source, blocks)
  }
  
}