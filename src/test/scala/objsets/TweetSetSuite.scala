package objsets

import org.scalatest.FunSuite
import TweetReader._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def size(set: TweetSet): Int = {
    if (set.isEmpty) 0
    else 1 + size(set.tail)
  }
  
  test("filter: on all tweets") {
    new TestSets {
      val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
      val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
      
      val tweets = allTweets
      
//      val googleTweets: TweetSet = tweets.filter(tweet => google.exists(googleWord => tweet.text.contains(googleWord)))
//      val appleTweets: TweetSet = tweets.filter(tweet => apple.exists(appleWord => tweet.text.contains(appleWord)))

      val googleTweets: TweetSet = tweets.filter(tweet => tweet.retweets == 10)
      val appleTweets: TweetSet = tweets.filter(tweet => tweet.retweets == 10)
      
      val filtered = googleTweets.union(appleTweets)
      
      val trending: Trending = filtered.ascendingByRetweet
      
      //assert(size(googleTweets) === 100)
      //assert(size(appleTweets) === 200)
      assert(size(filtered) === 300)
    }
  }
  
  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("non filter: on set5") {
    new TestSets {
      assert(size(set5.filter(tw => true)) === 4)
    }
  }
  
  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("filter: carlos on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.text.contains("carlos"))) === 0)
    }
  }

  test("filter: body on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.text.contains("body"))) === 4)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("ascending: set5") {
    new TestSets {
      val trends = set5.ascendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user === "c")
    }
  }
}
