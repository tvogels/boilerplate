package nl.tvogels.boilerplate.classification

import org.jsoup.nodes
import org.jsoup.nodes.TextNode
import scala.collection.JavaConversions._
import nl.tvogels.boilerplate.classification.{LanguageDependentSettings => s}
import nl.tvogels.boilerplate.Utilities.Util
import nl.tvogels.boilerplate.page.Block

case class FeatureExtractor(x: Block, source: String, docRoot: nodes.Document) {
  
  lazy val body = docRoot.body
  
  lazy val docLength = source.length
  lazy val startRelative = x.start.toDouble/docLength
  lazy val endRelative = x.end.toDouble/docLength
  lazy val numCharacters = x.text.length
  lazy val relativeLength = numCharacters.toDouble / docLength
  lazy val ratioPeriods = 
    x.text.count { x => x == '.' }.toDouble / numCharacters
  lazy val ratioPunctuation = 
    x.text.count { x => x == ',' || x == '?' }.toDouble / numCharacters
  lazy val ratioDashes = 
    x.text.count { x => x == '-' || x == '_' || x == '/' || x == '\\' } / numCharacters
  lazy val tagName = x.domnode.nodeName
  lazy val parentTagName = x.domnode.parentNode.nodeName
  lazy val words = x.text.split("\\W+").filter(x => x.length > 0)
  lazy val numWords = words.length
  lazy val averageWordLength = numWords match {
    case 0 => -1
    case w => words.map(x => x.length).sum.toDouble / w
  }
  lazy val ratioCapitalizedWords = words.count(x => x.charAt(0).isUpper).toDouble / numWords
  lazy val endsInPeriod = x.text.last == '.'
  
  lazy val linkNodes: List[nodes.Element] = x.domnode match {
    case elem: nodes.Element => elem.getAllElements.toList.filter(x => x.tagName == "a")
    case elem: TextNode      => List()
  }
  lazy val ratioCharsInLink = linkNodes.map(x => x.text.length).sum.toDouble / numCharacters
  lazy val ratioStopWords = words.count(x => s.stopWords.contains(x)).toDouble / numWords
  lazy val medSentenceLength = Util.median(x.text.split("""\?|\.|!|\||\(|\)| - | – | — """).map(x => x.trim.length.toDouble)) 
  
  lazy val tagsPath: Vector[TagDescriptor] = {
    def path(node: nodes.Node, acc: List[TagDescriptor]): List[TagDescriptor] = node match {
      case elem if elem.parent == null  => acc
      case elem => path(elem.parent, TagDescriptor(elem) :: acc)
    }
    path(x.domnode, List()).toVector
  }
  lazy val classAndIdPath = tagsPath.map(x => s"${x.className} {x.id} ").mkString
  
  lazy val startPositionInBody = (body.startPosition, body.endPosition) match {
    case (-1,_) => -1
    case (_,-1) => -1
    case (s,e) if (s==e) => -1
    case (s,e) => (x.start - body.startPosition) / (body.endPosition - body.startPosition)
  }
  
  lazy val MzUnlikelyCandidate    = s.regex.unlikelyCandidates.findFirstIn(classAndIdPath) != None
  lazy val MzOkMaybeItsACandidate = s.regex.okMaybeItsACandidate.findFirstIn(classAndIdPath) != None
  lazy val MzPositive             = s.regex.positive.findFirstIn(classAndIdPath) != None
  lazy val MzNegative             = s.regex.negative.findFirstIn(classAndIdPath) != None
  lazy val MzExtraneous           = s.regex.extraneous.findFirstIn(classAndIdPath) != None
  lazy val MzByline               = s.regex.byline.findFirstIn(classAndIdPath) != None
  
  override def toString = {
    s"numCharacters           : ${numCharacters}\n"+
    s"relativeLength          : ${relativeLength}\n"+
    s"startRelative           : ${startRelative}\n"+
    s"endRelative             : ${endRelative}\n"+
    s"ratioPeriods            : ${ratioPeriods}\n"+
    s"ratioPunctuation        : ${ratioPunctuation}\n"+
    s"ratioDashes             : ${ratioDashes}\n"+
    s"tagName                 : ${tagName}\n"+
    s"parentTagName           : ${parentTagName}\n"+
    s"numWords                : ${numWords}\n"+
    s"averageWordLength       : ${averageWordLength}\n"+
    s"ratioCapitalizedWords   : ${ratioCapitalizedWords}\n"+
    s"endsInPeriod            : ${endsInPeriod}\n"+
    s"ratioCharsInLink        : ${ratioCharsInLink}\n"+
    s"ratioStopWords          : ${ratioStopWords}\n"+
    s"tagsPath                : ${tagsPath}\n"+
    s"medSentenceLength       : ${medSentenceLength}\n"+
    s"startPositionInBody     : ${startPositionInBody}\n"+
    s"MzUnlikelyCandidate     : ${MzUnlikelyCandidate}\n"+
    s"MzOkMaybeItsACandidate  : ${MzOkMaybeItsACandidate}\n"+
    s"MzPositive              : ${MzPositive}\n"+
    s"MzNegative              : ${MzNegative}\n"+
    s"MzExtraneous            : ${MzExtraneous}\n"+
    s"MzByline                : ${MzByline}\n"
  }
  
//  lazy val featureList: Array[Double] = Array(numCharacters,relativeLength,startRelative,endRelative,ratioPeriods,ratioPunctuation,
//                                ratioDashes,numWords,averageWordLength,ratioCapitalizedWords,
//                                endsInPeriod,ratioCharsInLink,ratioStopWords,medSentenceLength,startPositionInBody,
//                                MzUnlikelyCandidate,MzOkMaybeItsACandidate,MzPositive,MzNegative,MzExtraneous,MzByline).map {
//    case i: Boolean => if (i) 1d else 0d
//    case i: Int     => i.toDouble
//    case i: Double  => i
//    case i: Float   => i.toDouble
//    case i          => -1d
//  }
  //lazy val featureList: Array[Double] = Array(relativeLength, ratioCharsInLink, ratioStopWords,ratioPunctuation, ratioPeriods, if(parentTagName=="p") 1.0 else 0.0, averageWordLength, startRelative, endRelative, medSentenceLength)
  
lazy val featureList: Array[Double] = Array(numCharacters, ratioCharsInLink, ratioStopWords)  
  
//  lazy val featureList: Array[Double] = Array(numCharacters)
//  lazy val featureList: Array[Double] = Array(if(ratioPunctuation > 0) 1.0 else 0.0,if(ratioPeriods > 0) 1.0 else 0.0,if(parentTagName=="p") 1.0 else 0.0,if(ratioCharsInLink > 0) 1.0 else 0.0)
}

object FeatureExtractor {
  val nFeatures = 3
}