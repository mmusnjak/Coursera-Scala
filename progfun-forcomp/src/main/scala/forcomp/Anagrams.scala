package forcomp

//import common._


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = 
    w.toLowerCase.toList.groupBy(_.toChar).map(p => (p._1, p._2.length)).toList.sortBy(_._1)
    // Part up to map creates a mapping from a character to a list containing N repetitions of character
    // map converts a list to a mapping from a character to number of occurences
    // finally, convert to list and sort by character

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = 
    wordOccurrences(("" /: s)((concat, word) => concat + word))
    // left fold (/:), joining words into one string, then calculating number of occurences of each character

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = 
    dictionary.groupBy(w => Anagrams.wordOccurrences(w))
    // groups words by occurence lists
    
  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
	dictionaryByOccurrences.get(wordOccurrences(word)) match {
      case Some(occurences) => occurences
      case None => List[Word]()
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    occurrences.foldLeft(List(List.empty[(Char, Int)])) {
      // first parameter is the accumulated sequence, second is the current occurrence object
      (seq, element) =>
        // to existing items we add the lists prepended by increasing numbers of current element
        seq union (for (rest <- seq; i <- 1 to element._2) yield (element._1, i) :: rest)
    }.map(_.sortBy(_._1)) // sorted by character
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   *  
   *  Hint: you can use foldLeft, and -, apply and updated operations on Map.
   *  alternative: https://github.com/stpyang/coursera/blob/927219a2c7fae3621403cc00a896ed1136d3119e/progfun-004/forcomp/src/main/scala/forcomp/deleteme.sc
   */
  
  case class BadOccurrences(s: String) extends Exception(s) {}
  
  def subtract(x: Occurrences, y: Occurrences): Occurrences = 
    // join two input lists
    (for (element <- (x ::: y).groupBy(_._1).toList)
      yield (element match {
        // if only the first is present, nothing is subtracted
        case (_, singleton :: Nil) => (singleton._1, singleton._2)
        // if there are two elements, we return the character and subtract the second from first
        case (_, first :: second :: Nil) => (first._1, first._2 - second._2)
        // subtraction without first occurrence being present is an exception
        case _ => throw new BadOccurrences("bad subtraction list")
        // filter out the negative elements
      })).filter(_._2 != 0).sortBy(_._1)

  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def _sentenceAnagrams(occs: Occurrences): List[Sentence] = occs match {
    // occs is the character occurrence list for the entire sentence (char -> count)
    case List() => List(List())
    case _ => for {
      combo <- combinations(occs)  // combo contains all subsets of the occurrence list
      word <- dictionaryByOccurrences.getOrElse(combo, Nil)  // a word for a combination
      sentence <- _sentenceAnagrams(subtract(occs, combo))  // anagrams for the remaining characters
      if !combo.isEmpty
    } yield {
      word :: sentence
    }
  }  
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = _sentenceAnagrams(sentenceOccurrences(sentence))

  def sentenceAnagramsFromCache(sentenceOccurrence: Occurrences, wordFromDictionaryByOccurence: Occurrences => List[Word]): List[Sentence] = {
    if(sentenceOccurrence.isEmpty) List(List())
    else {
      for {
        sentenceSubsetOccurrence <- combinations(sentenceOccurrence)
        if sentenceSubsetOccurrence != Nil
        word <- wordFromDictionaryByOccurence(sentenceSubsetOccurrence)
        if word != Nil
        anagrams <- sentenceAnagramsFromCache(subtract(sentenceOccurrence, sentenceSubsetOccurrence), wordFromDictionaryByOccurence)
      } yield word :: anagrams
    }
  }
  
  def sentenceAnagramsMemo(sentence: Sentence): List[Sentence] = {
    lazy val mutableDictByOccurrence = scala.collection.mutable.Map.empty[Occurrences, List[Word]]

    def cachedWordFromDictByOccurrence(occurrence: Occurrences): List[Word] = {
      mutableDictByOccurrence get occurrence match {
        case Some(v) => v
        case None => {
          val words = dictionaryByOccurrences.getOrElse(occurrence, List())
          mutableDictByOccurrence += (occurrence -> words)
          words
        }
      }
    }
    sentenceAnagramsFromCache(sentenceOccurrences(sentence), cachedWordFromDictByOccurrence)
  }
  
}

