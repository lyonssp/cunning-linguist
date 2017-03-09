package main

import twister.TongueTwister

object Linguist {
  def main(args: Array[String]): Unit = {
    val (source, tongueTwister) = TongueTwister.tongueTwisterWithHistory
    print(
      s"""
         |Sentence Structure:
         |    ${source.template.toString}
         |Original Sentence:
         |    ${source.toString}
         |Generated Tongue Twister:
         |    ${tongueTwister.toString}
       """.stripMargin
    )
  }
}
