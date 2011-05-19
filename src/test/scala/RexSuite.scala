/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-01-19, 3:21 PM
 * License: LGPL
 */

package com.digitaldoodles.rex

import Implicits._
import patterns._

import org.scalatest.FunSuite

class RexSuite extends FunSuite {
    test("~= and ~~= match within string and exact string respectively") {
        assert("Hello" ~~= "Hello")
        assert(!(Lit("Hello") ~~=  "Hey, Hello"))
        assert(Lit("ello")~="Hello")
    }

    test("special characters are escaped correctly in Lit and CharSet") {
        assert(Lit("[]^$\\.{,}|+*?") ~~= "[]^$\\.{,}|+*?")
        assert(CharSet("[]-^&\\.{}")**0 ~~= "[]-^&\\.{}")
	    assert(CharSet("-") ~~= "-")
    }

    test("CharSet functionality") {
        assert(CharSet("abc") ~= "cde")
        assert(CharSet("abc") !~= "de")
        assert((CharSet("abc") - CharSet("cde")) !~= "c")
        assert((CharSet("abc") - "c") !~= "c")
        assert((CharAny ** 0) ~~= "\na.,*")
        assert("[\\s\\S&&[^\\p{ASCII}]]" === (CharAny - CharASCII).pattern)
    }

    test("Complex number pattern") {
        // A complex is a float followed by a + or - followed by a float, followed by an "i"
        // The two numeric parts and the sign are named for access.
        val complexMatcher = (PatFloat.group("re") & (Lit("-")|"+").group("sign") & PatFloat.group("im") & "i").group("all")
        /* Match against a floating-point complex number and print the result. */
        val result = complexMatcher.findFirst("3.2+4.5i") match {
            case None => None
            case Some(m) => Some(m.group("re") + " " + m.group("sign") + " " + m.group("im") + "i")
        }
        assert(Some("3.2 + 4.5i") === result)
    }

    test("Boundary patterns") {
	    assert(BndryStringStart & "a" & BndryStringEnd ~~= "a")
	    assert(BndryStringStart & "a" & BndryStringEnd & "a" !~~= "aa")
        assert(BndryStringStart & Lit("a")**0 & BndryStringEnd ~~= "aaa")
        assert(BndryStringStart & Lit("a")**(0,2) & BndryStringEnd !~~= "aaa")
	    // TODO many more tests
    }

    test("Catch + if misused as pattern concatenator") {
        intercept[RuntimeException] {
          (BndryStringStart + "a") !~~= "aaa"
        }
        intercept[RuntimeException] {
          (BndryStringStart + BndryStringEnd) !~~= "aaa"
        }
        //Unfortunately, this works
        assert("a" + BndryStringEnd != "b")
    }

    test("HTML tag pattern") {
        val tagPat = ("<" & CharAny *? 1 & ">").group("tag")
        assert(tagPat ~~= "<a count=0>")
        val minFind:String = tagPat.findFirst("<a><b>") match {
            case None => ""
            case Some(m) => m.group("tag")
        }
        assert(minFind === "<a>")
    }

    test("Check that `findAllIn` correctly handles edge cases") {
        assert( (for(m <- Lit("a").findAllIn("aabbaba")) yield m.string).mkString("") === "aabbaba")
        assert( (for(m <- Lit("a").findAllIn("babbaba")) yield m.string).mkString("") === "babbaba")
        assert( (for(m <- Lit("a").findAllIn("aabbabb")) yield m.string).mkString("") === "aabbabb")
        assert( (for(m <- Lit("a").findAllIn("aabbabb") if (m.matched)) yield m.string).mkString("") === "aaa")
        assert( (for(m <- Lit("a").findAllIn("aabbabb") if (!m.matched)) yield m.string).mkString("") === "bbbb")
    }

    test("`replaceAllIn`") {
        val tagPat = "<td" & PatMinChars & ">"
        val target = "Hello<td style='color:black'>Goodbye<td >Now"
        assert (tagPat.replaceAllIn(target, "<td>") === "Hello<td>Goodbye<td>Now")
    }

    test("Grouping produces correct patterns") {
        assert("abc".anonGroup.pattern === "abc")
        assert((CharSet("abc").pattern === "[abc]"))
        assert((CharRange('a','z') & "abc").pattern === "[a-z]abc")
        assert("abc".lookahead.pattern === "(?=abc)")
        assert(("a" & "bc" & "def").pattern === "(?:abc)def")

    }

    test("Lookahead and lookback operators") {
        assert(("a" & "b".|> & "b") ~~= "ab")
        assert(("a" & "b".|> & "bb") !~~= "ab")
        assert(("a" & "c".!|> & "b") ~~= "ab")
        assert(("a" & "c".!|> & "c") !~~= "ac")

        assert(("a" & "c".!<| & "b") ~~= "ab")
        assert(("a" & "a".<| & "b") ~~= "ab")
        assert(("a" & "a".!<| & "b") !~~= "ab")
    }

    test("CharClass union, intersection, and subtraction operations") {
        val alnum = CharAlpha.digit
        assert(alnum**0 ~~= "abc123")
        val alpha = CharAlnum - CharDigit
        assert(alpha**0 ~~= "abc")
        assert(alpha**0 !~~= "abc123")
	    // TODO intersection
    }

    test("MatchOperators") {
      assert(Lit("aa") ~= "bbaabb")
      assert(Lit("a")*?1 !~~= "aaa")
    }

    test("MatchType") {
      assert(Lit("a")**1 ~~= "aaa")
      // The nongreedy match should fail because it only takes the first character.
      assert(Lit("a")*?1 !~~= "aaa")
      assert(Lit("a")*+1 ~~= "aaa")
      assert(CharSet("ab")**0 & CharSet("a")**0 ~~= "ababaa")
      //This next should fail; the possessive operator takes all the input, and allows
      // for no backtracking.
      assert(CharSet("ab")*+0 & CharSet("a")**1 !~~= "ababaa")
    }
}