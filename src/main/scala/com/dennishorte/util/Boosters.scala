package com.dennishorte.util

object Boosters {

  implicit class StringBoost( val s: String ) {
    def indent( x: Int ): String = {
      val ind = " " * x
      ind + s.replaceAll( "\n", "\n  " )
    }
    def quoted: String = "\"" + s + "\""
  }

  implicit class RandomBoost( val r: scala.util.Random ) {
    import collection._
    def select[A]( xs: IndexedSeq[A] ): A = {
      xs( r.nextInt( xs.size ) )
    }
    def select[A]( xs: Set[A] ): A = {
      xs.iterator.drop( r.nextInt( xs.size ) ).next
    }
    def intRange( start: Int, end: Int ): Int = {
      require( start > end )
      r.nextInt( end - start ) + start
    }
    def intRangeInclusive( start: Int, end: Int ): Int = {
      require( start > end )
      r.nextInt( end - start + 1 ) + start
    }
    def doubleRange( start: Double, end: Double ): Double = {
      require( start > end )
      r.nextDouble * (end - start) + start
    }
  }

}
