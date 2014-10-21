package forcomp

object scratch {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  println("nitalnez")                             //> nitalnez
  type Word = String
  val w:Word = "markoMARKOmajaborkojosip"         //> w  : forcomp.scratch.Word = markoMARKOmajaborkojosip
  //  w.toLowerCase.toList.groupBy(_.toChar).map(p => (p._1, p._2.length)).toList.sortBy(_._1)
  val a = w.toLowerCase                           //> a  : String = markomarkomajaborkojosip
  val b = a.toList                                //> b  : List[Char] = List(m, a, r, k, o, m, a, r, k, o, m, a, j, a, b, o, r, k,
                                                  //|  o, j, o, s, i, p)
  val c = b.groupBy(_.toChar)                     //> c  : scala.collection.immutable.Map[Char,List[Char]] = Map(s -> List(s), j -
                                                  //| > List(j, j), a -> List(a, a, a, a), m -> List(m, m, m), i -> List(i), b -> 
                                                  //| List(b), p -> List(p), r -> List(r, r, r), k -> List(k, k, k), o -> List(o, 
                                                  //| o, o, o, o))
  val d = c.map(p=>(p._1, p._2.length))           //> d  : scala.collection.immutable.Map[Char,Int] = Map(s -> 1, j -> 2, a -> 4, 
                                                  //| m -> 3, i -> 1, b -> 1, p -> 1, r -> 3, k -> 3, o -> 5)
  val e = d.toList                                //> e  : List[(Char, Int)] = List((s,1), (j,2), (a,4), (m,3), (i,1), (b,1), (p,1
                                                  //| ), (r,3), (k,3), (o,5))
  val f = e.sortBy(_._1)                          //> f  : List[(Char, Int)] = List((a,4), (b,1), (i,1), (j,2), (k,3), (m,3), (o,5
                                                  //| ), (p,1), (r,3), (s,1))

  type Sentence= List[Word]
  
  val a1 = List("marko", "svaki", "dan", "ide", "na", "posao" )
                                                  //> a1  : List[String] = List(marko, svaki, dan, ide, na, posao)
  val b1 = Anagrams.wordOccurrences(("" /: a1)((concat,word)=>concat+word))
                                                  //> b1  : forcomp.Anagrams.Occurrences = List((a,5), (d,2), (e,1), (i,2), (k,2),
                                                  //|  (m,1), (n,2), (o,3), (p,1), (r,1), (s,2), (v,1))
  // Anagrams.wordOccurrences(("" /: a1)((concat,word)=>concat+word))
	//val a2 = Anagrams.dictionary.groupBy(w => Anagrams.wordOccurrences(w))

  //val a3 = Anagrams.wordAnagrams("players")
	val a4 = Anagrams.wordOccurrences("srp")  //> a4  : forcomp.Anagrams.Occurrences = List((p,1), (r,1), (s,1))
    /**
    occurrences.foldLeft(List(List.empty[(Char, Int)])) {
      (seq, element) =>
        seq union (for (rest <- seq; i <- 1 to element._2) yield (element._1, i) :: rest)
    }.map(_.sortBy(_._1))
    */
  val b4 = a4.foldLeft(List(List.empty[(Char, Int)])){
    (seq,element) => seq
  }                                               //> b4  : List[List[(Char, Int)]] = List(List())
  val c4 = Anagrams.combinations(List[(Char,Int)](('a',2),('b',3)))
                                                  //> c4  : List[forcomp.Anagrams.Occurrences] = List(List(), List((a,1)), List((
                                                  //| a,2)), List((b,1)), List((b,2)), List((b,3)), List((a,1), (b,1)), List((a,1
                                                  //| ), (b,2)), List((a,1), (b,3)), List((a,2), (b,1)), List((a,2), (b,2)), List
                                                  //| ((a,2), (b,3)))
  //for(i <- List.range(0,10) if i%2 == 0; a <- -1 to 4 if a%3==0) yield (i,a)

  //val d4 = List(List(), List((a,1)), List(( a,2)), List((b,1)), List((b,2)), List((b,3)),
  //List((b,1), (a,1)), List((b,2), (a,1)), List((b,3), (a,1)),
  //List((b,1), (a,2)), List((b,2), (a,2)), List ((b,3), (a,2)))
  for(i <- List.range(0,10) if i%2 == 0; a <- -1 to 4 if a%3==0) yield (i,a)
                                                  //> res0: List[(Int, Int)] = List((0,0), (0,3), (2,0), (2,3), (4,0), (4,3), (6,
                                                  //| 0), (6,3), (8,0), (8,3))


	//val s = Anagrams.sentenceAnagrams(List("i", "love", "you"))

	for {
	  a <- 1 until 30
	  b <- a until 79
	} yield (a,b)                             //> res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (1,
                                                  //| 2), (1,3), (1,4), (1,5), (1,6), (1,7), (1,8), (1,9), (1,10), (1,11), (1,12)
                                                  //| , (1,13), (1,14), (1,15), (1,16), (1,17), (1,18), (1,19), (1,20), (1,21), (
                                                  //| 1,22), (1,23), (1,24), (1,25), (1,26), (1,27), (1,28), (1,29), (1,30), (1,3
                                                  //| 1), (1,32), (1,33), (1,34), (1,35), (1,36), (1,37), (1,38), (1,39), (1,40),
                                                  //|  (1,41), (1,42), (1,43), (1,44), (1,45), (1,46), (1,47), (1,48), (1,49), (1
                                                  //| ,50), (1,51), (1,52), (1,53), (1,54), (1,55), (1,56), (1,57), (1,58), (1,59
                                                  //| ), (1,60), (1,61), (1,62), (1,63), (1,64), (1,65), (1,66), (1,67), (1,68), 
                                                  //| (1,69), (1,70), (1,71), (1,72), (1,73), (1,74), (1,75), (1,76), (1,77), (1,
                                                  //| 78), (2,2), (2,3), (2,4), (2,5), (2,6), (2,7), (2,8), (2,9), (2,10), (2,11)
                                                  //| , (2,12), (2,13), (2,14), (2,15), (2,16), (2,17), (2,18), (2,19), (2,20), (
                                                  //| 2,21), (2,22), (2,23), (2,24), (2,25), (2,26), (2,27), (2,28), (2,29), (2,3
                                                  //| 0), (2,31), (2,32), (2,
                                                  //| Output exceeds cutoff limit.

}