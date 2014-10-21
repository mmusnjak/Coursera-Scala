package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("create code tree of a list") {
    val listChars = List('a', 'b', 'a', 'b', 'b', 'c')
    assert(createCodeTree(listChars) === Fork(Leaf('b',3),Fork(Leaf('c',1),Leaf('a',2),List('c', 'a'),3),List('b', 'c', 'a'),6))
  }

   test("decode message") {
   val tree = Fork(Leaf('a',8), Fork(Fork(Leaf('b', 3), Fork(Leaf('c', 1), Leaf('d', 1), List('c', 'd'), 2), List('b', 'c', 'd'), 5), Fork(Fork(Leaf('e', 1), Leaf('f', 1), List('e', 'f'), 2), Fork(Leaf('g', 1), Leaf('h', 1), List('g', 'h'), 2), List('e', 'f', 'g', 'h'), 4), List('b', 'c', 'd', 'e', 'f', 'g', 'h'), 9), List('a','b', 'c', 'd', 'e', 'f', 'g', 'h'), 17)
   val msg: List[Bit] = List(1, 0, 0, 0, 1, 0, 1, 0)
   assert(decode(tree, msg) === List('b', 'a', 'c'))
  }
   
  test("encode message") {
   val tree = Fork(Leaf('a',8), Fork(Fork(Leaf('b', 3), Fork(Leaf('c', 1), Leaf('d', 1), List('c', 'd'), 2), List('b', 'c', 'd'), 5), Fork(Fork(Leaf('e', 1), Leaf('f', 1), List('e', 'f'), 2), Fork(Leaf('g', 1), Leaf('h', 1), List('g', 'h'), 2), List('e', 'f', 'g', 'h'), 4), List('b', 'c', 'd', 'e', 'f', 'g', 'h'), 9), List('a','b', 'c', 'd', 'e', 'f', 'g', 'h'), 17)
   val text: List[Char] = string2Chars("bac")
   assert(encode(tree)(text) === List(1, 0, 0, 0, 1, 0, 1, 0))
  }
    
}
