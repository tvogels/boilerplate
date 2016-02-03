package nl.tvogels.boilerplate.classification

import nl.tvogels.boilerplate.Utilities.{Warc,Util}
import nl.tvogels.boilerplate.page.Segmenter
import nl.tvogels.boilerplate.page.{Tree,Leaf}
import org.jsoup.Jsoup
import ch.ethz.dalab.dissolve.classification.{StructSVMModel,BinarySVMWithSSG}
import breeze.linalg.Vector

object ClassifyWarc {

  private case class Corpus(name: String, version: String, creation: String) {
    import scala.collection.immutable.Vector
    var shards: Vector[Shard] = Vector()
    def toXML = {
      <corpus name={name} version={version} creation={creation}>
				{shards.map(_.toXML)}
			</corpus>
    }
  }
  
  private case class Shard(name: String) {
    import scala.collection.immutable.Vector
    var documents: Vector[Document] = Vector()
    def toXML = {
      <shard-file name={name} nr-documents={documents.length.toString}>
				{documents.map(_.toXML)}
			</shard-file>
    }
  }
  
  private case class Document(targetURI: String, trecID: String, recordID: String, contentLength: Int) {
    import scala.collection.immutable.Vector
    var paragraphs: Vector[Paragraph] = Vector()
    def toXML = {
      <document WARC-Target-URI={targetURI} WARC-TREC-ID={trecID} 
							  WARC-Record-ID={recordID} Content-Length={contentLength.toString}>
        {paragraphs.map(_.toXML)}
      </document>
    }
  }
  
  private case class Paragraph(id: String, start: Int, end: Int, html: String, text: String) {
    import scala.collection.immutable.Vector
    def toXML = {
      val htmlblock = 
        if (html != null) <html>{scala.xml.PCData(html)}</html>
        else <html></html>;
          
      <paragraph id={id} start={start.toString} end={end.toString}>
				<text>{scala.xml.PCData(text)}</text>
				{htmlblock}
      </paragraph>
    }
  }
  
  val weightsurl = "/Users/thijs/Desktop/model.weights.txt"
  val model = {
    val weightsfile = new java.io.File(weightsurl)
    val weights = breeze.linalg.csvread(weightsfile).toDenseVector
    new StructSVMModel[Vector[Double], Double](weights, 0.0, null, BinarySVMWithSSG)
  }
  
  
  
  def classify(t: Tree): Double = {
    val features = Vector(t.allCombinedFeatures.map(_._2).toArray)
    model.predict(features)
  }
  
  def main(args: Array[String]) = {
    
    val file = "/Users/thijs/eth/sempap/ClueWeb12_00/0001wb/0001wb-00.warc"
    val outfile = "/Users/thijs/Desktop/ClueWeb001wb-00.xml"
    
    val warcit = Warc.iteratorForFile(file).flatten
    val c = Corpus("clueweb12","1.2","2013-01-01T00:00+01:00")
    val s = Shard("0001wb/0001wb-00.warc")
    c.shards = scala.collection.immutable.Vector(s)
    var dno: Int = 0
    Util.time {
    val docs = for (record <- warcit.take(10)) yield {
      dno += 1
      val d = Document(record.warcUri, record.warcTrecId, "", record.payload.length)
      val (headers, content) = Warc.headersAndContent(record)
      Util.save(s"/Users/thijs/Desktop/pages/$dno.html",content)
      val dom = Jsoup.parse(content)
      Segmenter.parse(dom.body).map(tree => {
        Segmenter.prune(tree)
        val l = tree.leaves
        l.foreach(t => { t.label = classify(t).toInt })
        var pid: Int = 0
        d.paragraphs = tree.paragraphs.map(p => {
          pid += 1
          val (start, end) = (p.nodeStats.startPosition, p.nodeStats.endPosition)
          val html = if (start != -1 && end != -1) content.slice(start, end) else null
          Paragraph(pid.toString, start, end, html, p.mainText)
        }).toVector
        
        d
      })
    }
    s.documents = docs.flatten.toVector
    scala.xml.XML.save(outfile, c.toXML, "UTF-8", true, null)
    }
  }
  
}