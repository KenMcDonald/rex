/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-01-19, 3:21 PM
 * License: LGPL
 */

package com.digitaldoodles.rex

import Implicits._
import patterns._
import org.scalatest.FunSuite

class CharClassSuite extends FunSuite {
	test("Char Class Intersection 1") {
		val cc = CharSet("ab") /\ CharSet("bc")
		assert(cc ~~= "b")
		assert(cc !~~= "a")
		assert(cc !~~= "c")
		assert(cc !~~= "d")
	}

	test("Char Class Intersection 2") {
		val cc = !CharSet("ab") /\ CharSet("bc")
		assert(cc ~~= "c")
		assert(cc !~~= "a")
		assert(cc !~~= "b")
		assert(cc !~~= "d")
	}
}

class RexSuite extends FunSuite {

	test("~= and ~~= match within string and exact string respectively") {
		assert("Hello" ~~= "Hello")
		assert(!("Hello" ~~=  "Hey, Hello"))
		assert("ello"~="Hello")
	}

	test("special characters are escaped correctly in Lit and CharSet") {
		assert("[]^$\\.{,}|+*<" ~~= "[]^$\\.{,}|+*<")
		assert(CharSet("[]-^&\\.{}")*>0 ~~= "[]-^&\\.{}")
		assert(CharSet("-") ~~= "-")
	}

	test("CharSet functionality") {
		assert(CharSet("abc") ~= "cde")
		assert(CharSet("abc") !~= "de")
		assert((CharSet("abc") - CharSet("cde")) !~= "c")
		assert((CharSet("abc") - "c") !~= "c")
		assert((Chars.Any *> 0) ~~= "\na.,*")
		assert(Chars.Any.pattern === "[\\s\\S]")
		assert(Chars.Any.characters === "\\s\\S")
		assert("[\\s\\S&&[\\p{ASCII}]]" === (Chars.Any /\ Chars.ASCII).pattern)
		assert("[\\s\\S&&[^\\p{ASCII}]]" === (Chars.Any - Chars.ASCII).pattern)
	}

	test("CharSet negation") {
		assert(!CharSet("abc")*>1 ~~= "def")
		assert(!CharSet("abc")*>1 !~~= "dbf")
	}

	test("Complex number pattern") {
		// A complex is a float followed by a + or - followed by a float, followed by an "i"
		// The two numeric parts and the sign are named for access.
		val complexMatcher = (Number.SignedFloat.name("re") +~ (Lit("-")|"+").name("sign") +~ Number.SignedFloat.name("im") +~ "i").name("all")
		/** Match against a floating-point complex number and print the result. */
		val result = complexMatcher.findFirst("3.2+4.5i") match {
			case None => None
			case Some(m) => Some(m.group("re") + " " + m.group("sign") + " " + m.group("im") + "i")
		}
		assert(Some("3.2 + 4.5i") === result)
	}

	test("Boundary patterns") {
		assert(Input.Start +~ "a" +~ Input.End ~~= "a")
		assert(Input.Start +~ "a" +~ Input.End +~ "a" !~~= "aa")
		assert(Input.Start +~ Lit("a")*>0 +~ Input.End ~~= "aaa")
		assert(Input.Start +~ Lit("a")*>(0,2) +~ Input.End !~~= "aaa")
		// TODO many more tests
	}

	test("HTML tag pattern") {
		val tagPat = ("<" +~ Chars.Any*<1 +~ ">").name("tag")
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
		val tagPat = Lit("<td") +~ Chars.Any*<0 +~ ">"
		val target = "Hello<td style='color:black'>Goodbye<td >Now"
		assert (tagPat.replaceAllIn(target, "<td>") === "Hello<td>Goodbye<td>Now")
	}

	test("Grouping produces correct patterns") {
		assert("abc".anonGroup.pattern === "(?:abc)")
		assert((CharSet("abc").pattern === "[abc]"))
		assert((CharRange('a','z') +~ "abc").pattern === "[a-z](?:abc)")
		assert("abc".lookahead.pattern === "(?=abc)")
		assert(("a" +~ "bc" +~ "def").pattern === "(?:(?:a)(?:bc))(?:def)")

	}

	test("Lookahead and lookback operators") {
		assert(("a" +~ "b".>>) +~ "b" ~~= "ab")
		assert((("a" +~ "b".>>) +~ "bb") !~~= "ab")
		assert((("a" +~ "c".!>>) +~ "b") ~~= "ab")
		assert((("a" +~ "c".!>>) +~ "c") !~~= "ac")
		assert((("x".!<< +~ "a") +~ "b") ~~= "ab")
		assert("x" +~ CharRange('a', 'z').>> ~= "axb")
		assert(CharRange('a', 'z').<< +~ Lit(".") ~= "a.")
		assert(CharRange('a', 'z').<< +~ Lit(".") +~ CharRange('a', 'z').>> ~= "a.b")
		assert(CharRange('a', 'z').<< +~ Lit(".") +~ CharRange('a', 'z').>> !~= "a .b")
		assert(CharRange('a', 'z').<< +~ Lit(".") +~ CharRange('a', 'z').>> !~= "a. b")
		assert(CharRange('a', 'z').<< +~ Lit(".") +~ CharRange('a', 'z').>> ~= "a.b")
		//println("<"+(CharRange('a', 'z') <=: Lit(".") :=> CharRange('a', 'z')).findFirst("a.b").get +">")
		assert((CharRange('a', 'z').<< +~ Lit("^") +~ CharRange('a', 'z').>>).findFirst("a^b").getOrElse("?").toString === "^")
	}

	test("CharClass union, intersection, and subtraction operations") {
		val alnum = Chars.Alphabetic.Digit
		assert(alnum*>0 ~~= "abc123")
		val alpha = Chars.Alphanumeric - Chars.Digit
		assert(alpha*>0 ~~= "abc")
		assert(alpha*>0 !~~= "abc123")
		// TODO intersection
	}

	test("MatchOperators") {
		assert(Lit("aa") ~= "bbaabb")
		assert(Lit("a")*<1 !~~= "aaa")
		assert(Lit("\"\"\"") +~ Chars.Any*<0 +~ "\"\"\"" ~~= "\"\"\"abc\"\"\"")
		assert(Lit("\"") +~ ((Lit("\\") +~ Chars.Any) | !CharSet("\""))*<0 +~ "\"" ~~= "\"A string\\n\\\" thing.\"")
	}

	test("MatchType") {
		assert("a"*>1 ~~= "aaa")
		// The nongreedy match should fail because it only takes the first character.
		assert("a"*<1 !~~= "aaa")
		assert("a"*!1 ~~= "aaa")
		assert("ab"*>1 ~~= "ab")
		assert("ab"*>1 ~~= "ababab")
		//assert("ab"*<1 +~ "ab"*>1 ~~= "ababab")
		//This next should fail; the possessive operator takes all the input, and allows
		// for no backtracking.
		assert("ab"*!1 +~ "ab"*>1 !~~= "ababab")
	}

	test("Optional") {
		assert(Lit("a") +~ Lit("b").? ~~= "a")
	}

	test("Tokenizer") {
		val t = new Tokenizer(x=>"?", (Lit("a"), (x:MatchResult)=>"1"), (Lit("b"), (x:MatchResult)=>"2"))
		assert(t.tokenize("fabaabbc").mkString === "?121122?")
	}
}