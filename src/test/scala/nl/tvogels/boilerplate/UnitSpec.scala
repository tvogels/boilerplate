package nl.tvogels.boilerplate

import org.scalatest._
import breeze.linalg.{DenseVector,Matrix,Vector,max}
import ch.ethz.dalab.dissolve.classification.StructSVMModel
import ch.ethz.dalab.dissolve.optimization.DissolveFunctions
import ch.ethz.dalab.dissolve.regression.LabeledObject
import nl.tvogels.boilerplate.classification.DissolveUnary
import nl.tvogels.boilerplate.page.Page

abstract class UnitSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {
  
  type X = Page
  type Y = Vector[Double]
  
  val dissolveFunctions: DissolveFunctions[X, Y] = DissolveUnary
  
  val data = DissolveUnary.loadData(20).toArray
  
  private val lo = data(0)
  
  val numd = DissolveUnary.featureFn(lo.pattern, lo.label).size
  val model: StructSVMModel[X, Y] =
    new StructSVMModel[X, Y](DenseVector.zeros(numd), 0.0,
      DenseVector.zeros(numd), dissolveFunctions, 1)
  
  def perturb(y: Y, degree: Double = 0.1): Y = {
    val d = y.size
    val numSwaps = max(1, (degree * d).toInt)
    for (swapNo <- 0 until numSwaps) {
      // Swap two random values in y
      val (i, j) = (scala.util.Random.nextInt(d), scala.util.Random.nextInt(d))
      val temp = y(i)
      y(i) = y(j)
      y(j) = temp
    }
    y
  }
  
  def phi = dissolveFunctions.featureFn _
  def delta = dissolveFunctions.lossFn _
  def maxoracle = dissolveFunctions.oracleFn _
  def predict = dissolveFunctions.predictFn _
  
  def psi(lo: LabeledObject[X, Y], ymap: Y) =
    phi(lo.pattern, lo.label) - phi(lo.pattern, ymap)

  def F(x: X, y: Y, w: Vector[Double]) =
    w dot phi(x, y)
  def deltaF(lo: LabeledObject[X, Y], ystar: Y, w: Vector[Double]) =
    w dot psi(lo, ystar)
  
}