package nl.tvogels.boilerplate.classification

import cc.factorie._
import cc.factorie.variable._            // The base library: variables, factors
import cc.factorie.model._            // The base library: variables, factors
import cc.factorie.infer._
import cc.factorie.la           // Linear algebra: tensors, dot-products, etc.
import cc.factorie.optimize._   // Gradient-based optimization and training


object Factorie extends App {
  
  val nFeatures = 21
  
  def loadData(a: Int, b: Int): List[LabelSeq] = {
    val features = scala.io.Source.fromURL(getClass.getResource("/dataset/features"+nFeatures+".csv"))
      .getLines()
      .slice(a,b)
      .map(l => {
        val line = l.split(",").map(_.toDouble).toList
        val nFeatures: Int = line(1).toInt
        val nBlocks: Int = line(2).toInt
        val features = line.slice(3,line.size)
        (0 until nBlocks).map(b => features.slice(nFeatures*b,nFeatures*(b+1)).toArray).toList
      })
      .filter { l => l.length > 1 }
      .toList
    val labels = scala.io.Source.fromURL(getClass.getResource("/dataset/labels.csv"))
      .getLines()
      .map(l => {
        val line = l.split(",").map(_.toDouble).toList
        val nBlocks: Int = line(1).toInt
        line.slice(2,line.size).map(x => x == 1.0).toArray
      })
      .filter { l => l.length > 1 }
      .toList
    (features zip labels).map { 
      case (f, l)  => new LabelSeq ++= (f zip l).map {
        case (feat,lab) => {
          val tok = new Token()
          tok := new DenseTensor1(feat)
          new Label(lab, tok)
        }
      }
    }
  }
  
  // Declare random variable types
  // A domain and variable type for storing words

 
  object TokenDomain extends VectorDomain { 
    def dimensionDomain = new DiscreteDomain(nFeatures) 
  } 
  
  class Token extends VectorVariable { def domain = TokenDomain }
 
  
  object LabelDomain extends BooleanDomain 
  class Label(lab:Boolean, val token:Token) extends LabeledBooleanVariable(lab)

  class LabelSeq extends scala.collection.mutable.ArrayBuffer[Label]
  val labelSequences = loadData(0,600)
  val testData = loadData(600,700)
  // Define a model structure
  val model = new Model with Parameters {
    // Two families of factors, where factor scores are dot-products of sufficient statistics and weights.
    // (The weights will set in training below.)
    val markov = new DotFamilyWithStatistics2[Label,Label] { 
      val weights = Weights(new la.DenseTensor2(LabelDomain.size, LabelDomain.size))
    }
    val observ = new DotFamilyWithStatistics2[Label,Token] {
      val weights = Weights(new la.DenseTensor2(LabelDomain.size, TokenDomain.dimensionSize))
    }
    // Given some variables, return the collection of factors that neighbor them.
    def factors(labels:Iterable[Var]) = labels match {
      case labels:LabelSeq => 
        labels.map(label => new observ.Factor(label, label.token)) ++ 
        (labels.sliding(2).map(window => new markov.Factor(window.head, window.last)))
    }
  }
  
  
  val realLabels = for (s <- labelSequences; l <- s) yield l.value
  val realLabelsTest = for (s <- testData; l <- s) yield l.value
  
  // Learn parameters
  val trainer = new BatchTrainer(model.parameters, new ConjugateGradient)
  trainer.trainFromExamples(labelSequences.map(labels => new LikelihoodExample(labels, model, InferByBPChain)))


  println(model.markov.weights.value)
  
  labelSequences.foreach(labels => BP.inferChainMax(labels, model).setToMaximize(null))
  testData.foreach(labels => BP.inferChainMax(labels, model).setToMaximize(null))
  
  
  println("training token accuracy=" + HammingObjective.accuracy(labelSequences.flatten))
  println("test token accuracy=" + HammingObjective.accuracy(testData.flatten))
  // Print the learned parameters on the Markov factors.
  // Print the inferred tags
  println(model.markov.weights.value)
  println(model.observ.weights.value)
  
  
  
  val predictions = for (s <- labelSequences; l <- s) yield l.value
  val predictionsTest = for (s <- testData; l <- s) yield l.value
  println("Prediction error training: "+(predictions zip realLabels).collect {
    case (p,l) if p != l => 1
  }.length.toDouble / predictions.length)
  println("Prediction error test: "+(predictionsTest zip realLabelsTest).collect {
    case (p,l) if p != l => 1
  }.length.toDouble / predictionsTest.length)
}