package nl.tvogels.boilerplate.cleaneval

import jaligner.{Alignment,Sequence,SmithWatermanGotoh}
import jaligner.matrix.MatrixLoader
import scala.util.Random
import java.io.{PrintWriter,File}
import java.nio.file.{Paths, Files}
import nl.tvogels.boilerplate.Utilities.Util
import scala.Vector

object AlignCleanEvalData {
  
  private sealed trait SourceSegment {
    def start: Int
    def sourceStart: Int
    def length: Int
  }
  
  private case class OpenSegment(val start: Int, end: Int, sourceStart: Int, sourceEnd: Int) extends SourceSegment {
    def length = end - start
    def sourceLength = sourceEnd - sourceStart
    def takeCleaned(cleaned: String) = cleaned.substring(start,end)
    def takeSource(source: String) = source.substring(sourceStart,sourceEnd)
  }
  
  private case class MatchSegment(val start: Int, sourceStart: Int, length: Int) extends SourceSegment
  
  val splitTries = 300
  val searchFragmentLength = 20
  val problematic = List(638,747,748)
  val files = List(183,1,10,104,105,106,107,108,109,11,110,111,112,113,114,115,116,117,118,119,12,120,121,122,123,124,125,126,127,128,129,13,130,131,132,133,134,135,136,137,138,139,14,140,141,142,143,144,145,146,147,148,149,15,150,151,152,153,154,155,156,157,158,159,16,160,161,162,163,164,165,166,167,168,169,17,170,171,172,173,174,175,176,177,178,179,18,180,181,182,184,185,186,187,188,189,19,190,191,192,193,194,195,196,197,198,199,2,20,200,201,202,203,204,205,206,207,208,209,21,210,211,212,213,214,215,216,217,218,219,22,220,221,222,223,224,225,226,227,228,229,23,230,231,232,233,234,235,236,237,238,239,24,240,241,242,243,244,245,246,247,249,25,250,251,252,253,254,255,256,257,258,259,26,260,261,262,263,264,265,266,267,268,269,27,270,271,272,273,274,275,276,277,278,279,28,280,281,282,283,284,285,286,287,288,289,29,290,291,292,293,294,295,296,297,298,299,3,30,300,301,302,303,304,305,306,307,308,309,31,310,311,312,313,314,315,316,317,318,319,32,320,321,322,323,324,325,326,327,328,329,33,330,331,332,333,334,335,336,337,338,339,34,340,341,342,343,344,345,346,347,348,349,35,350,351,352,353,354,355,356,357,358,359,36,360,361,362,363,364,365,366,367,368,369,37,370,371,372,373,374,375,376,377,378,379,38,380,381,382,383,384,385,386,387,388,389,39,390,391,392,393,394,395,396,397,398,399,4,40,400,401,402,403,404,405,406,407,408,409,41,410,411,412,413,414,415,416,417,418,419,42,420,421,422,424,425,426,427,428,429,43,430,431,433,434,435,436,437,438,439,44,440,441,442,443,444,445,446,447,448,449,45,450,451,452,453,454,455,456,457,458,459,46,460,461,462,463,464,465,466,467,468,469,47,470,471,472,473,474,475,476,477,478,479,48,480,481,482,483,484,485,486,487,488,489,49,490,491,492,493,494,495,496,497,498,499,5,50,500,501,502,503,504,505,506,507,508,509,51,510,52,53,54,561,562,563,564,565,566,567,568,569,57,570,571,572,573,574,575,576,577,578,579,58,580,581,582,583,584,585,586,587,588,589,59,590,591,592,593,594,595,596,597,598,599,6,60,600,601,602,603,604,605,606,607,608,609,61,610,611,612,613,614,615,616,617,618,619,62,620,621,622,623,624,625,626,627,628,629,63,630,631,632,633,634,635,636,637,639,64,640,641,642,643,644,645,646,647,648,649,65,650,651,652,653,654,655,656,657,658,659,66,660,661,662,663,664,665,666,667,668,669,67,670,671,672,673,674,675,676,677,678,679,68,680,681,682,683,684,685,686,687,688,689,69,690,691,692,693,694,695,696,697,698,699,7,70,700,701,702,703,704,705,706,707,708,709,71,710,711,712,713,714,715,716,717,718,719,72,720,721,722,723,724,725,726,727,728,729,73,730,731,732,733,734,735,736,737,738,739,74,740,741,742,743,744,745,746,749,75,750,752,754,755,756,757,758,759,76,760,761,762,763,764,765,766,767,768,77,770,771,772,773,774,775,776,777,778,779,78,780,781,782,783,784,785,786,787,788,789,79,790,791,792,793,794,795,796,797,798,799,8,80,81,82,83,84,85,86,87,88,89,9,90,91,92,93,94,95,96,97,98,99)

  /*
   * Align all CleanEval files 
   */
  def alignAllFiles(dir: String) = {
    // Index of the current CleanEval file to process
    problematic.foreach { index => {
      val outFile = s"$dir/aligned/$index.txt.utf8"
      println("")
      println(s"###########################")
      println(s"### Aligning $index ...")
      println(s"###########################")
      if (!Files.exists(Paths.get(outFile))) {
        val url = s"$dir/orig/$index.html.utf8"
        val source = Util.loadWithoutFirstLine(url)
        val url2 = s"$dir/clean/$index.txt.utf8"
        val clean = normalizeCleaned(Util.loadWithoutFirstLine(url2))
        val output = findFileAlignment(source, clean)
        val writer = new PrintWriter(new File(outFile))
        writer.write(output)
        writer.close
      }
    } }
  }
  
  def findFileAlignment(source: String, clean: String): String = {
    val mask = maskTags(source)
    assert(mask.length == source.length)
    
    var open: Vector[OpenSegment] = Vector(OpenSegment(0,clean.length, 0, mask.length))
    var matches: Vector[MatchSegment] = Vector()
    
    var i = 0
    while (i < splitTries) {
      i = i+1
      
      val os = Util.randomSelectionWeighted(open, open.map { x => (x.length - 10).max(0).toDouble })
      
      if (os.length - searchFragmentLength > 0) {
        val start = Random.nextInt(os.length - searchFragmentLength) + os.start
        val end   = start + searchFragmentLength
        val subs  = clean.substring(start,end)
        val res   = Util.allSubstringOccurences(mask,subs,os.sourceStart, os.sourceEnd - searchFragmentLength - 1)
        
        if (res.length == 1) {
          open    = open.filter(x => x != os) ++ 
                    Vector(
                      OpenSegment(os.start,start, os.sourceStart, res(0)), 
                      OpenSegment(end,os.end, res(0)+searchFragmentLength, os.sourceEnd)
                     )
          matches = matches ++ 
                    Vector(MatchSegment(start,res(0),searchFragmentLength))
        }
      }
    }
    println(matches sortBy { _.sourceStart })
    println(open sortBy { _.sourceStart })
    val combo: Vector[SourceSegment] = open++matches
    val res = combo.sortBy { _.sourceStart }.map {
      case MatchSegment(start,sourceStart,length) => clean.substring(start,start+length)
      case os: OpenSegment => {
        println(os)
        println("Source  : '" + os.takeSource(source) + "'")
        println("Cleaned : '" + os.takeCleaned(clean) + "'")
        stringify(alignment(os.takeSource(mask), os.takeCleaned(clean)),os.sourceLength)
      }
    }.mkString
    assert(res.length == source.length)
    res
  }
  
  def stringify(align: Alignment, masklength: Int) = {
    val a: String = String.valueOf(align.getSequence1)
    val b: String = String.valueOf(align.getSequence2)
    val gap = String.valueOf(Alignment.GAP)
    val c: String = b.zipWithIndex.filter(x => a(x._2) != Alignment.GAP).unzip._1.mkString
    val d = gap * align.getStart1 + c
    d + gap * (masklength - d.length)
  }
  
  def alignment(a: String, b: String) = {
    val x = new Sequence(a)
    val y = new Sequence(b)
    SmithWatermanGotoh.align(x, y, alignmentMatrix, 0.5f, 0f)
  }
  
  lazy val alignmentMatrix = 
    MatrixLoader.load(
      new jaligner.ui.filechooser.NamedInputStream(
        "THIJS",
        getClass().getResourceAsStream("/ALIGNMENT_MATRIX")
      )
    )
  
  def normalizeCleaned(txt: String): String =
    txt.toUpperCase
       .replaceAll("""\s+"""," ")
       .replaceAll("""<(P|L|H)>|\* |[0-9]+\. |_+""","")
       .trim
       .replaceAll("""[^\x00-\x7F]"""," ")
       //.replaceAll("""\W"""," ")
  
  def maskTags(html: String): String = {
    val htmlTags = """(?s)<(.*?)>"""r
    val fmTags = """(?s)(<HEAD[^>]*>.*?</HEAD>|<SCRIPT[^>]*>.*?</SCRIPT>|<STYLE[^>]*>.*?</STYLE>|&[A-Z]+;|<.*?>)"""r
    
    val html2 = html.toUpperCase.replaceAll("""\s"""," ")
    fmTags.replaceAllIn(html2, m => "#" * m.group(0).length)
          .replaceAll("""[^\x00-\x7F]"""," ")
          //.replaceAll("""\W"""," ")
          .replaceAllLiterally(String.valueOf(Alignment.GAP), " ")
  }
  
}