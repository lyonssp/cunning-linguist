package main

import twister.TongueTwister

object Linguist {
  def main(args: Array[String]): Unit = {
    val (source, tongueTwister) = TongueTwister.tongueTwisterWithHistory
    println(source.toString)
    println(tongueTwister.toString)
  }
}
