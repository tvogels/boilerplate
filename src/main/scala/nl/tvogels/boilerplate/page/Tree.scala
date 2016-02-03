package nl.tvogels.boilerplate.page

import org.jsoup.nodes.{Element,Node,TextNode}
import scala.collection.mutable.Buffer
import nl.tvogels.boilerplate.Utilities.Util
import nl.tvogels.boilerplate.classification.{LanguageDependentSettings => s}
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.linalg.{Vectors}
import ch.ethz.dalab.dissolve.regression.LabeledObject
import ch.ethz.dalab.dissolve.regression.LabeledObject
import breeze.linalg

/**
 * @author thijs
 */

case class Tree(var tag: List[String], var dom: List[Node], var children: Buffer[Tree]) {

  val HTMLtags = Vector("body","address","article","aside","blockquote","dd","div","dl","fieldset","figcaption","figure","figcaption","footer","form","h1","h2","h3","h4","h5","h6","header","hgroup","li","main","nav","noscript","ol","output","p","pre","section","table","tfoot","ul") ++
                 Vector("b","big","i","small","tt","abbr","acronym","cite","code","dfn","em","kbd","strong","samp","time","var","a","bdo","q","span","sub","sup","label")++
                 Vector("td","tr","th","thead","tbody")
  
  override def toString = {
    val tagStr = tag.mkString("/")
    if (dom.last.isInstanceOf[TextNode]) {
      val c = if (label == 1) Console.GREEN_B+Console.WHITE else Console.RED
       tagStr + " : " + c + " " + Util.preview((dom.last.asInstanceOf[TextNode]).text.replace("\n", " "),100) + " " + Console.RESET 
    } else {
      val c = for (c <- children; l <- c.toString.lines) yield {"  " + l}
      (tagStr :: c.toList).mkString("\n")
    }
  }
  
  var label: Int = -1
  
  var leftSibbling: Option[Tree] = None
  var rightSibbling: Option[Tree] = None
  var parent: Option[Tree] = None
  var root: Tree = null
  
  var nodeStats: NodeStats = new NodeStats
  
  def tagFeatures: Vector[(String,Double)] = {
    HTMLtags.map{case t => ("tag"+t, if (tag.contains(t)) 1.0 else 0.0)}
  }
  val emptyTagFeatures = HTMLtags.map{case t => ("tag"+t, 0.0)}
  
  def allCombinedFeatures: Vector[(String,Double)] = {
    val myfeat = normalizedSingleFeatures ++ tagFeatures
    val pfeat = Vector(("hasParent",if (parent != None) 1.0 else 0.0)) ++
                (if (parent != None) parent.get.normalizedSingleFeatures++parent.get.tagFeatures else normalizedSingleZeroFeatures++emptyTagFeatures).map {
                  case (l,v) => ("p"+l,v)
                }
    val gpfeat = Vector(("hasGrandparent",if (parent != None && parent.get.parent != None) 1.0 else 0.0)) ++
                (if (parent != None && parent.get.parent != None) parent.get.parent.get.normalizedSingleFeatures++parent.get.parent.get.tagFeatures else normalizedSingleZeroFeatures++emptyTagFeatures).map {
                  case (l,v) => ("gp"+l,v)
                }
                
    myfeat ++ pfeat ++ gpfeat
  }
  
  val normalizedSingleZeroFeatures = Vector(("logNCharacters",0.0), 
        ("rWords",0.0), 
        ("nSentences",0.0), 
        ("rPunctuation",0.0), 
        ("rDashes",0.0), 
        ("rPeriods",0.00),
        ("rCharsInLink",0.0), 
        ("endsWithPunctuation",0.0), 
        ("endsWithQuestionMark",0.0), 
        ("rWordsWithCapital",0.0), 
        ("rStopwords",0.0),  
        ("avgWordLength",0.0), 
        ("startRel",0.0), 
        ("endRel",0.0))
  
  def normalizedSingleFeatures: Vector[(String,Double)] = {
    val bstats = root.nodeStats
    val s = nodeStats
    Vector(("logNCharacters", scala.math.log(s.nCharacters)), 
        ("rWords", if (bstats.nWords != 0) s.nWords.toDouble / bstats.nWords else 0.0), 
        ("nSentences", s.nSentences.toDouble), 
        ("rPunctuation", if (s.nCharacters != 0) s.nPunctuation.toDouble / s.nCharacters else 0.0), 
        ("rDashes",  if (s.nCharacters != 0) s.nDashes.toDouble / s.nCharacters else 0.0), 
        ("rPeriods", if (s.nCharacters != 0) s.nPeriods.toDouble / s.nCharacters else 0.0),
        ("rCharsInLink", if (s.nCharacters != 0) s.nCharsInLink.toDouble / s.nCharacters else 0.0), 
        ("endsWithPunctuation", s.endsWithPunctuation.toDouble), 
        ("endsWithQuestionMark", s.endsWithQuestionMark.toDouble), 
        ("rWordsWithCapital", if (s.nWords != 0) s.nWordsWithCapital.toDouble/s.nWords else 0.0), 
        ("rStopwords", if (s.nWords != 0) s.nStopwords.toDouble/s.nWords else 0.0),  
        ("avgWordLength", if (s.nWords > 0) s.totalWordLength.toDouble/s.nWords else 0.0), 
        ("startRel", if (bstats.nCharacters > 0) (s.startPosition - bstats.startPosition) / bstats.nCharacters else 0.0), 
        ("endRel", if (bstats.nCharacters > 0) (s.endPosition - bstats.startPosition) / bstats.nCharacters else 0.0))
  }
  
  def loadStatsFromDomNode(node: Node) = {
    val text = 
      if (node.isInstanceOf[TextNode])
        Util.trim(node.asInstanceOf[TextNode].text)
      else if (node.isInstanceOf[Element])
        Util.trim(node.asInstanceOf[Element].text)
      else ""
    nodeStats.nCharacters = text.length
    val words = text.split("\\W+").filter(x => x.length > 0)
    nodeStats.nWords = words.length
    nodeStats.totalWordLength = words.map(_.length).sum
    nodeStats.nPunctuation = text.count { x => List(',','?',';',':','!').contains(x) }
    nodeStats.nPeriods = text.count { x => x == '.' }
    nodeStats.nDashes = text.count { x => x == '-' || x == '_' || x == '/' || x == '\\' }
    nodeStats.nWordsWithCapital = words.count(_.charAt(0).isUpper)
    nodeStats.nStopwords = words.count { x => s.stopWords.contains(x) }
    nodeStats.endsWithQuestionMark = if (text.last == '?') 1 else 0
    nodeStats.endsWithPunctuation = if (List(',','.',':',';','!').contains(text.last)) 1 else 0
    val sentences = text.split("""\?|\.|!|\||\(|\)| - | – | — """)
    nodeStats.nSentences = sentences.length
    nodeStats.nCharsInLink = if (node.nodeName == "a") nodeStats.nCharacters else 0
    nodeStats.startPosition = node.startPosition
    nodeStats.endPosition = node.endPosition
  }
  
  def loadStatsFromChildren() = {
    nodeStats.nCharacters = children.map(_.nodeStats.nCharacters).sum
    nodeStats.nWords = children.map(_.nodeStats.nWords).sum
    nodeStats.totalWordLength = children.map(_.nodeStats.totalWordLength).sum
    nodeStats.nPunctuation = children.map(_.nodeStats.nPunctuation).sum
    nodeStats.nPeriods = children.map(_.nodeStats.nPeriods).sum
    nodeStats.nDashes = children.map(_.nodeStats.nDashes).sum
    nodeStats.nWordsWithCapital = children.map(_.nodeStats.nWordsWithCapital).sum
    nodeStats.nStopwords = children.map(_.nodeStats.nStopwords).sum
    nodeStats.nSentences = children.map(_.nodeStats.nSentences).sum
    nodeStats.nCharsInLink =
      if (tag.contains("a")) nodeStats.nCharacters 
      else children.map(_.nodeStats.nCharsInLink).sum
      
    // one option = load position from children
    nodeStats.startPosition = children.head.nodeStats.startPosition
    nodeStats.endPosition = children.last.nodeStats.startPosition
    // other is to use the thing itself
    nodeStats.startPosition = dom.head.startPosition
    nodeStats.endPosition = dom.head.endPosition
    
    nodeStats.endsWithPunctuation = children.last.nodeStats.endsWithPunctuation
    nodeStats.endsWithQuestionMark = children.last.nodeStats.endsWithQuestionMark
    nodeStats.nChildrenDeep = children.map(_.nodeStats.nChildrenDeep).sum + children.length
  }
  
  def leaves: Buffer[Tree] = this match {
    case Leaf(l) => Buffer(l)
    case t => t.children.flatMap(_.leaves)
  }
  
  def containsBlocksOrBreaks: Boolean = {
    false
  }
  
  def paragraphs: Buffer[Tree] = dom.last match {
    case t: TextNode            => if (label == 1) Buffer(this) else Buffer()
    case Segmenter.BlockLeaf(l) => if (leaves.map(_.label).contains(1)) Buffer(this) else Buffer()
    case t                      => children.flatMap(_.paragraphs)
  }
  
  def mainText: String = this match {
    case Leaf(l) => l.dom.last.asInstanceOf[TextNode].text
    case t => t.leaves.filter(_.label == 1).map(_.mainText).mkString(" ")
  }
  
  
  def toLabeledPoints: Buffer[LabeledPoint] = {
    leaves.map(t => {
      val l = if (t.label == 1.0) 1 else -1
      val f = t.allCombinedFeatures.map(x => x._2).toArray
      new LabeledPoint(l,Vectors.dense(f))
    })
  }
  
  def toLabeledDocument: LabeledObject[linalg.Matrix[Double], linalg.Vector[Double]] = {
    val l = leaves
    val nFeatures = l(0).allCombinedFeatures.length
    val nLeaves = l.length
    val fmat = linalg.DenseMatrix.zeros[Double](nFeatures,nLeaves)
    val lvec = linalg.DenseVector.zeros[Double](nLeaves)
    for (i <- 0 until nLeaves) {
      fmat(::,i) := linalg.DenseVector[Double](l(i).allCombinedFeatures.map(x => x._2).toArray)
      lvec(i) = leaves(i).label
    }
    new LabeledObject(lvec, fmat)
  }
}

object Leaf {
  def apply(tag: List[String], dom: List[Node]) = 
    Tree(tag, dom, Buffer())
  def apply(tag: List[String], domNode: Node) = { 
    val t = Tree(tag, List(domNode), Buffer())
    t.loadStatsFromDomNode(domNode)
    t
  }
  def unapply(tree: Tree) =
    if(tree.children.length == 0) Some(tree) else None
}