package nl.tvogels.boilerplate.page

import org.jsoup.nodes
import org.jsoup.nodes.{Node,TextNode}
import scala.collection.JavaConversions._
import scala.annotation.tailrec
import scala.Vector
import nl.tvogels.boilerplate.Utilities.Util




object Segmenter {
    
  private object SkipNode {
    val skipTags = Set("#doctype", "br", "checkbox", "head", "hr", "iframe", "img",
                       "input", "meta", "noscript", "radio", "script", "select", "style",
                       "textarea", "title", "video")
                        
    def unapply(z: nodes.Node): Option[nodes.Node] = {
      if (skipTags.contains(z.nodeName)) Some(z) else None
    }
  }
  
  private object EmptyNode {
    def unapply(z: nodes.Element): Option[nodes.Element] =
      if (z.text.replaceAll("\\W+","") == "") Some(z) else None
  }
  
  private object EmptyTextNode {
    def unapply(z: nodes.TextNode): Option[nodes.TextNode] = 
      if (z.text.replaceAll("\\W+","") == "") Some(z) else None
  }
  
  object BlockLeaf {
    val blockTags = Set("address", "article", "aside", "blockquote", "body", "canvas",
                        "center", "checkbox", "dd", "div", "dl", "fieldset", "figcaption",
                        "figure", "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6",
                        "head", "header", "hgroup", "hr", "html", "iframe", "input", "li",
                        "main", "nav", "noscript", "ol", "ol", "output", "p", "pre", "radio",
                        "section", "select", "table", "tbody", "td", "textarea", "tfoot",
                        "thead", "tr", "ul", "video")
              
    def hasBlockLevelChildren(z: nodes.Element): Boolean = {
      z.getAllElements.tail.find(x => blockTags.contains(x.nodeName)) != None
    }
    
    def unapply(z: nodes.Element): Option[nodes.Element] = {
      if (!hasBlockLevelChildren(z)) Some(z) else None
    }
  }

  def parse(n: Node): Option[Tree] = n match {
    case null             => None
    case SkipNode(n)      => None
    case EmptyTextNode(n) => None
    case EmptyNode(n)     => None
    
    case t: TextNode      => Some(Leaf(List(t.nodeName), t))
//    case Seg.BlockLeaf(t) => Some(Leaf(List(t.nodeName), t))
    
    case t => {
      val children = t.childNodes.map(parse).flatten
      if (children.length > 0)
        Some(Tree( List(t.nodeName), List(t), children ))
      else
        None
    }
  }
  
  def prune(t: Tree): Unit = {
    assert(t != null, "pruning null is not an option")
    if (t.root == null) t.root = t
    
    var prev: Option[Tree] = None
    t.children.foreach(x => {
      prune(x)
      
      // Set parent pointer
      x.parent = Some(t)
      x.root = t.root
      
      // Set sibbling pointers
      x.leftSibbling = prev
      prev.map(_.rightSibbling = Some(x))
      prev = Some(x)
    }) 
    
    val nChildren = t.children.length
    if (nChildren == 1) {
      val child = t.children(0)
      
      t.tag = t.tag ++ child.tag
      t.label = child.label
      t.dom = t.dom ++ child.dom
      t.nodeStats = child.nodeStats
      t.children = child.children
      if (t.tag.head == "a") {
        t.nodeStats.nCharsInLink = t.nodeStats.nCharacters
      }
    } else if (nChildren > 1){
      t.loadStatsFromChildren()
    }
  }
  
  def segmentation_old(node: Node): Vector[Block] = node match {
    
    case SkipNode(elem)       => Vector()
    case EmptyNode(elem)      => Vector()
    case EmptyTextNode(elem)  => Vector()
    
    case BlockLeaf(elem)      => Vector(Block(elem.nodeName, Util.trim(elem.text), node))
    case elem: nodes.TextNode => Vector(Block(elem.nodeName, Util.trim(elem.text), node))
    
    case elem                 => elem.childNodes.flatMap(segmentation_old _).toVector
  }
  
  def segmentation(node: Node): Vector[Block] = {
    @tailrec
    def seg(nodes: List[Node], acc: List[Block]): List[Block] = nodes match {
      case Nil                        => acc
      case SkipNode(elem) :: ls       => seg(ls, acc)
      case EmptyNode(elem) :: ls      => seg(ls, acc)
      case EmptyTextNode(elem) :: ls  => seg(ls, acc)
    
      case BlockLeaf(elem) :: ls      => seg(ls, acc ::: List(Block(elem.nodeName, Util.trim(elem.text), elem)))
      case (elem: TextNode) :: ls     => seg(ls, acc ::: List(Block(elem.nodeName, Util.trim(elem.text), elem)))
    
      case elem :: ls                 => seg(elem.childNodes.toList ::: ls, acc)
    }
    seg(List(node),List()).toVector
  }
  
}