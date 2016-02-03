package nl.tvogels.boilerplate

import java.io.{File,FileInputStream,DataInputStream,BufferedInputStream,PrintWriter}
import scala.io.Source
import org.jsoup.Jsoup
import scala.collection.JavaConversions._
import nl.tvogels.boilerplate.cleaneval.AlignCleanEvalData
import nl.tvogels.boilerplate.Utilities.Util
import nl.tvogels.boilerplate.Utilities.Warc
import nl.tvogels.boilerplate.classification.FeatureExtractor
import nl.tvogels.boilerplate.page.{Segmenter,Page}
import nl.tvogels.boilerplate.classification.{DissolveUnary,FeatureExtractor}

object Main {
  
  def main(args: Array[String]): Unit = {
//    println("Please run the main functions of one of the classification implementations in nl.tvogels.boilerplate.classification.")
    //AlignCleanEvalData.alignAllFiles("/Users/thijs/eth/sempap/cleaneval")
//    generateChainDataset()
    testSegmentation
//    generateFlatDataset
  }
  
  def makeCleanEvalDataset() = {
    val items = AlignCleanEvalData.files
    val dir = "/Users/thijs/eth/sempap/cleaneval"
    val lines = items take 10 flatMap { i => {
      println(s"Doing file $i")
      val orig = Util.loadWithoutFirstLine(s"$dir/orig/$i.html.utf8")
      val aligned = Util.loadFile(s"$dir/aligned/$i.txt.utf8")
      val p = Page.fromSource(orig)
      val labels = p.blocks map {
        case s if s.start == -1 => "NA"
        case s if s.end   == -1 => "NA"
        case s => aligned.substring(s.start, s.end).toString matches ".*[A-Z].*"
      }
      (p.blocks zip labels) map {
        case (segment,label) =>  (FeatureExtractor(segment, p.source, p.dom).featureList mkString "\t") + "\t" + label
      }
    }}
    val writer = new PrintWriter(new File("/Users/thijs/Desktop/dataset.tsv"))
    lines foreach { x => writer.write(x + "\n") }
    writer.close()
  }
  
  def testFeatureExtractionAndSegmentation() = {
    val url = "/Users/thijs/Desktop/wiki.html"
    val pw = new PrintWriter(new File("/Users/thijs/Desktop/out.txt"))
    val html = Util.loadWithoutFirstLine(url)
    val doc = Jsoup.parse(html)
    println("Running ...")
    val segs = Util.time {Segmenter.segmentation(doc)}
    segs.foreach (x=> {
      val fe = FeatureExtractor(x, html, doc)
      pw.write("-" * x.toString.length.min(80) + "\n")
      pw.write(x.toString + "\n")
      pw.write("-" * x.toString.length.min(80) + "\n")
      pw.write(fe + "\n")
    })
    pw.close()
  }
  
  
  /*
   * Takes all segmented text blocks that have a -1 
   */
  def testSegmentation = {
    val dir = "/Users/thijs/Desktop"
    val i = 58
    val file = Util.loadWithoutFirstLine(s"$dir/$i.html.utf8")
    val parsed = Jsoup.parse(file)
    def leaves(n: org.jsoup.nodes.Node): List[org.jsoup.nodes.TextNode] = n match {
      case n: org.jsoup.nodes.TextNode if n.childNodes.length == 0 => List(n)
      case n if n.childNodes.length == 0                          => List()
      case n => n.childNodes.flatMap(leaves).toList
    }
    
    def printTree(n: org.jsoup.nodes.Node): String = {
      s"[${n.nodeName}] (${n.startPosition}/${n.endPosition})\n" +
      n.childNodes.map(printTree).mkString("\n").lines.map(l => "  "+l).mkString("\n")
    }
    
    println(printTree(parsed))
    
    val lvs = leaves(parsed).filter(n => n.startPosition == -1 || n.endPosition == -1)
    print(lvs.map(n => s"[${n.startPosition},${n.endPosition}] ${Util.preview(n.text,20)}").mkString("\n"))
  }
  
  def segmentCluewebArchive() = {
    val filename = "/Users/thijs/eth/sempap/ClueWeb12_00/0001wb/0001wb-00.warc"
    val warcIt = Warc.iteratorForFile(filename).flatten
    var i = 0
    Util.time { for(warc <- warcIt) {
      i = i+1
      val (headers, content) = Warc.headersAndContent(warc)
      // new PrintWriter("/Users/thijs/Desktop/current.html") { write(content); close }
      
      val p = Page.fromSource(content)
      val features = Util.time { p.blocks.map(x => FeatureExtractor(x, content, p.dom).toString) }
      println(s"$i. Done with a document (${p.blocks.length} blocks)")
    }} 
  }
  
  def generateChainDataset() = {
    val data = DissolveUnary.loadData(-1)
    val writer = new PrintWriter(new File("/Users/thijs/Desktop/features"+FeatureExtractor.nFeatures+".csv"))
    data.toArray.zipWithIndex foreach { case (x,i) => writer.write(i + "," + x.pattern.featureMatrix.rows + "," + x.pattern.featureMatrix.cols + "," + x.pattern.featureMatrix.toArray.mkString(",")+"\n") }
    writer.close()
    val writer2 = new PrintWriter(new File("/Users/thijs/Desktop/labels.csv"))
    data.toArray.zipWithIndex foreach { case (x,i) => writer2.write(i + "," + x.label.length + "," + x.label.toArray.mkString(",")+"\n") }
    writer2.close()
  }
  
  def generateFlatDataset() = {
    val data = DissolveUnary.loadData(-1).toArray
    val writer = new PrintWriter(new File("/Users/thijs/Desktop/flat-data"+FeatureExtractor.nFeatures+".csv"))
    for (d <- data) {
      d.label.toArray.zipWithIndex.foreach {
        case (lab, i) => writer.write(lab+","+d.pattern.featureMatrix(::,i).toArray.mkString(",")+"\n"); 
      }
    }
    writer.close()
  }

}
