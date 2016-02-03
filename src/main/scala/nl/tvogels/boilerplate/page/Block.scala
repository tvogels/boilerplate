package nl.tvogels.boilerplate.page

import org.jsoup.nodes.Node

case class Block(tag: String, text: String, domnode: Node) {
 
  override def toString = 
    s"[$tag:${domnode.startPosition},${domnode.endPosition}] ${text.trim}"
    
  def start = domnode.startPosition
  def end = domnode.endPosition
  
}