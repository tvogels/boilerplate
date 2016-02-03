package nl.tvogels.boilerplate.classification

import org.jsoup.nodes

case class TagDescriptor(dn: nodes.Node) {
  def name = dn.nodeName
  def id = dn match {
    case elem: nodes.Element => elem.id
    case _ => ""
  }
  def className = dn match {
    case elem: nodes.Element => elem.className
    case _ => ""
  }
  override def toString = (name,id,className) match {
    case (name, "","") => name
    case (name,id,className) => s"$name#$id.${className.replace(" ", ".")}"
  }
  override def equals(o: Any) = o match {
    case TagDescriptor(otherNode) => otherNode == dn
    case _ => false
  }
}