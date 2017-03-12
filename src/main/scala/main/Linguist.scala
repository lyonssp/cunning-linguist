package main

import twister.TongueTwister

object Linguist {
  def main(args: Array[String]): Unit =
    for {
      (source, tongueTwister) <- TongueTwister.tongueTwisterWithHistory.sample
    } yield {
      print(
        s"""
         |Sentence Structure:
         |    ${source.template.toString}
         |Original Sentence:
         |    ${source.toString}
         |Generated Tongue Twister:
         |    ${tongueTwister.toString}
       |""".stripMargin
      )
    }
}
