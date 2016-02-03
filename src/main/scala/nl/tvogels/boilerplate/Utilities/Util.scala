package nl.tvogels.boilerplate.Utilities

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

object Util {
  
  // Median algorithm from StackOverflow
  @tailrec private def findKMedian(arr: Array[Double], k: Int): Double = {
    val a = arr(scala.util.Random.nextInt(arr.size))
    val (s, b) = arr partition (a >)
    if (s.size == k) a
    // The following test is used to avoid infinite repetition
    else if (s.isEmpty) {
        val (s, b) = arr partition (a ==)
        if (s.size > k) a
        else findKMedian(b, k - s.size)
    } else if (s.size < k) findKMedian(b, k - s.size)
    else findKMedian(s, k)
  }
  
  def median(arr: Array[Double]) = findKMedian(arr, (arr.size - 1) / 2)
    
  def time[R](block: => R): R = {  
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0).toDouble/1000000000 + "s")
    result
  }
  
  def allSubstringOccurences(str1:String, substr:String): List[Int] = {
   @tailrec def count(pos:Int, c:List[Int]): List[Int] = {
      val idx=str1 indexOf(substr, pos)
      if(idx == -1) c else count(idx+substr.size, idx::c)
   }
   count(0,List())
  }
  
  def allSubstringOccurences(str1:String, substr:String, sourceStart: Int, sourceEnd: Int): List[Int] = {
   @tailrec def count(pos:Int, c:List[Int]): List[Int] = {
      val idx=str1 indexOf(substr, pos)
      if(idx == -1 || idx > sourceEnd) c else count(idx+substr.size, idx::c)
   }
   count(sourceStart,List())
  }
  
  def loadWithoutFirstLine(path: String) = 
    Source.fromFile(path).getLines.drop(1) mkString "\n"
    
  def loadResourceWithoutFirstLine(path: String) =
    Source.fromInputStream(getClass.getResourceAsStream(path)).getLines.drop(1) mkString "\n"
    
  def loadFile(path: String) = 
    Source.fromFile(path).getLines mkString "\n"
    
  def loadResource(path: String) = 
    Source.fromInputStream(getClass.getResourceAsStream(path)).getLines mkString "\n"
  
  def randomSelectionWeighted[X](elements: Seq[X], weights: Seq[Double]): X = {
    assert(weights.length == elements.length)
    def normalize(l: Seq[Double]) = {
      val sum = l.sum
      if (sum == 0d) l.map(x => x/l.length) else l.map(x => x/sum)
    }
    val nweights = normalize(weights)
    val cum_ = nweights.scan(0d)(_ + _).tail
    val cum  = cum_.updated(cum_.length-1,1d)
    val rand = Random.nextDouble
    elements((cum.indexWhere { x => x > rand } - 1).max(0))
  }
  
  def preview(s: String, n: Int) = 
    if (s.length <= n) s
    else s.take(s.lastIndexWhere(_.isSpaceChar, n)).trim
  
  def save(filename: String, content: String): Unit = {
    val file = new java.io.File(filename)
    val out = new java.io.BufferedWriter(new java.io.FileWriter(file))
    out.write(content)
    out.close
  }
    

  def trim(s: String): String = s.replaceAll("(^\\h*)|(\\h*$)","")
    
}