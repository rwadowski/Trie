package com.alg

import com.alg.trie.Trie

/**
  * Created by rwadowski on 2016-05-07.
  */
object Application {
  def main(args: Array[String]): Unit = {
    println("Running app")

    val words = Set("Poland", "Portugal", "France", "England", "Germany", "Denmark", "Sweden", "Norway", "Spain", "Italy", "Greece", "Hungary", "Austria", "Ireland", "Island", "Switzerland")

    val trie = words.foldLeft(new Trie){case (trie, word) => trie.add(word)}

    println(s"Words in set ${words} starting for Po -> ${trie.search("Po")}")
  }
}
