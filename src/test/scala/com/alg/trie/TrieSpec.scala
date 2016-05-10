package com.alg.trie

import org.scalatest.{FlatSpec, Matchers}

/*
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

class TrieSpec extends FlatSpec with Matchers {
  it should "add words properly" in {
    val root = new Trie
    val res = root.add("ada")

    //root
    res.children.size should equal(1)
    res.children.keys.toSet should equal(Set('a'))

    //a
    var aNode = res.children('a')
    aNode.isWord should equal (false)
    aNode.children.size should equal (1)
    aNode.children.keys.toSet should equal (Set('d'))

    //d
    var dNode = aNode.children('d')
    dNode.isWord should equal (false)
    dNode.children.size should equal (1)
    dNode.children.keys.toSet should equal (Set('a'))

    //a
    aNode = dNode.children('a')
    aNode.isWord should equal (true)
    aNode.children.size should equal (0)
    aNode.children.keys.toSet should equal (Set.empty)

    val res2 = res.add("ad")

    aNode = res2.children('a')
    aNode.isWord should equal (false)
    aNode.children.size should equal (1)
    aNode.children.keys.toSet should equal (Set('d'))

    //d
    dNode = aNode.children('d')
    dNode.isWord should equal (true)
    dNode.children.size should equal (1)
    dNode.children.keys.toSet should equal (Set('a'))

    val res3 = res2.add("adb")

    //a
    aNode = res3.children('a')
    aNode.isWord should equal (false)
    aNode.children.size should equal (1)
    aNode.children.keys.toSet should equal (Set('d'))

    //d
    dNode = aNode.children('d')
    dNode.isWord should equal (true)
    dNode.children.size should equal (2)
    dNode.children.keys.toSet should equal (Set('a','b'))

    //a
    aNode = dNode.children('a')
    aNode.isWord should equal (true)
    aNode.children.size should equal (0)
    aNode.children.keys.toSet should equal (Set.empty)

    //b
    val bNode = dNode.children('b')
    bNode.isWord should equal (true)
    bNode.children.size should equal (0)
    bNode.children.keys.toSet should equal (Set.empty)
  }

  it should "build trie" in {
    var trie = new Trie
    trie = trie.add("adapter")
    trie = trie.add("adam")
    trie = trie.add("bee")

    val res = trie.search("ada")
    res.size should equal (2)
    res should equal(Set("adapter", "adam"))

    val res2 = trie.search("other")
    res2 should equal (Set.empty)

    val res3 = trie.search("bel")
    res3 should equal (Set.empty)

    val res4 = trie.search("be")
    res4.size should equal (1)
    res4 should equal (Set("bee"))
  }
}
