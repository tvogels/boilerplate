package nl.tvogels.boilerplate.classification

object LanguageDependentSettings {
    
  // English stop words from http://www.ranks.nl/stopwords
  val stopWords = Set("a","about","above","after","again","against","all","am","an","and","any","are","as","at","be","because","been","before","being","below","between","both","but","by","cannot","could","did","do","does","doing","down","during","each","few","for","from","further","had","has","have","having","he","her","here","hers","herself","him","himself","his","how","i","if","in","into","is","it","its","itself","me","more","most","my","myself","no","nor","not","of","off","on","once","only","or","other","ought","our","ours", "ourselves","out","over","own","same","she","should","so","some","such","than","that","the","their","theirs","them","themselves","then","there","these","they","this","those","through","to","too","under","until","up","very","was","we","were","what","when","where","which","while","who","whom","why","with","would","you","your","yours","yourself","yourselves")

  object regex {
    // Regular expressions from Mozilla Readability (https://github.com/mozilla/readability/blob/master/Readability.js)
    val unlikelyCandidates = """(?i)banner|combx|comment|community|disqus|extra|foot|header|menu|related|remark|rss|sh|are|shoutbox|sidebar|skyscraper|sponsor|ad-break|agegate|pagination|pager|popup""".r
    val okMaybeItsACandidate = """(?i)and|article|body|column|main|shadow""".r
    val positive = """(?i)article|body|content|entry|hentry|main|page|pagination|post|text|blog|story""".r
    val negative = """(?i)hidden|banner|combx|comment|com-|contact|foot|footer|footnote|masthead|media|meta|outbrain|promo|related|scroll|share|shoutbox|sidebar|skyscraper|sponsor|shopping|tags|tool|widget""".r
    val extraneous = """(?i)print|archive|comment|discuss|e[\-]?mail|share|reply|all|login|sign|single|utility""".r
    val byline = """(?i)byline|author|dateline|writtenby""".r
  }
  
}