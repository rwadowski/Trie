package com.alg.trie

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

class Trie(val character: Option[Char],
           val children: Map[Char, Trie],
           val isWord: Boolean) {

  def this() {
    this(None, Map.empty, false)
  }

  private def toWordNode: Trie = if(isWord) this else new Trie(character, children, true)

  val isRoot = character.isEmpty

  def add(w: String): Trie = {
    val ch = w.charAt(0)
    children.get(ch) match {
      case Some(child) => {
        val updatedChild = w.length match {
          case 1 => child.toWordNode
          case _ => child.add(w.substring(1))
        }

        isRoot match {
          case true => new Trie(None, children + (updatedChild.character.get -> updatedChild), false)
          case false => new Trie(character, children + (updatedChild.character.get -> updatedChild), isWord)
        }
      }
      case None => {
        val child = createNode(w)

        isRoot match {
          case true => new Trie(None, children + (child.character.get -> child), false)
          case false => new Trie(character, children + (child.character.get -> child), isWord)
        }
      }
    }
  }

  private def createNode(w: String): Trie =  {
    val ch = w.charAt(0)
    w.length match {
      case 1 => new Trie(Some(ch), Map.empty, true)
      case _ =>
        val child = createNode(w.substring(1))
        new Trie(Some(ch), Map(child.character.get -> child), false)
    }
  }

  override def toString: String = {
    character match {
      case Some(ch) => ch.toString
      case None => new String
    }
  }

  def search(prefix: String): Set[String] = {
    def loop(n: Trie, p: String): Set[String] = {
      val ch = p.charAt(0)
      val node = n.children.get(ch)
      node match {
        case Some(next) => if(1 == p.length) next.children.values.flatMap{child => child.words(prefix)}.toSet else loop(next, p.substring(1))
        case None => Set.empty
      }
    }
    loop(this, prefix)
  }

  def words(prefix: String): Set[String] = {
    def loop(n: Trie, w: String, set: Set[String]): Set[String] = {

      val ch = n.character.get.toString
      val nextSet = n.isWord match {
        case true => set + (w + ch)
        case false => set
      }
      n.children.isEmpty match {
        case true => nextSet
        case false => n.children.values.flatMap{next => loop(next, w + ch, nextSet)}.toSet
      }
    }
    loop(this, prefix, Set.empty)
  }
}
