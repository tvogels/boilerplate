package nl.tvogels.boilerplate.page

/**
 * @author thijs
 */

class NodeStats {
  var nCharacters = -1
  var nWords = -1
  var nSentences = -1
  var nPunctuation = -1
  var nDashes = -1
  var nPeriods = -1
  var nCharsInLink = -1
  var endsWithPunctuation = -1
  var endsWithQuestionMark = -1
  var nWordsWithCapital = -1
  var nStopwords = -1
  var totalWordLength = -1
  var startPosition = -1
  var endPosition = -1
  var nChildrenDeep = 0
  
  override def toString: String = {
    s"char:$nCharacters word:$nWords sent:$nSentences pnc:$nPunctuation dsh:$nDashes " +
    s"peri:$nPeriods link:$nCharsInLink epnc:$endsWithPunctuation eq:$endsWithQuestionMark " +
    s"capw:$nWordsWithCapital stop:$nStopwords wl:$totalWordLength st:$startPosition end:$endPosition " +
    s"ncd:$nChildrenDeep"
    
  }
}