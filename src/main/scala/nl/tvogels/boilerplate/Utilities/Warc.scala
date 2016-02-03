package nl.tvogels.boilerplate.Utilities

import edu.knowitall.cluewebextractor.{WarcRecordIterator,WarcRecord}
import java.io.{File, FileInputStream, DataInputStream, BufferedInputStream}

object Warc {
    
  def iteratorForFile(fileName: String) = {
    val f = new File(fileName)
    new WarcRecordIterator(new DataInputStream(new BufferedInputStream(new FileInputStream(f))))
  }
  
  def headersAndContent(warc: WarcRecord) = {
    val fileFormat = """(?s)(.*?)?(?:\r?\n|\r){2,}+[\n\r]*(.*)""".r
    warc.payload match {
      case fileFormat(headers, content) => (headers, content)
    }
  }
  
}