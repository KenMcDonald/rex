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
	test("Special characters are escaped in char classes") {
		assert(CharSet("[]-^&\\.{}")*>0 ~~= "[]-^&\\.{}")
		assert(CharSet("-") ~~= "-")
	}

	test("CharSet functionality") {
		assert(CharSet("abc")*>0 ~= "cde")
		assert(CharSet("abc")*>0 !~~= "de")
		assert(Chars.Any *> 0 ~~= "\na.,*")
		assert(Chars.Any.pattern === "[\\s\\S]")
		assert(Chars.Any.characters === "\\s\\S")
		assert("[\\s\\S&&[\\p{ASCII}]]" === (Chars.Any /\ Chars.ASCII).pattern)
		assert("[\\s\\S&&[^\\p{ASCII}]]" === (Chars.Any - Chars.ASCII).pattern)
	}

	test("CharSet negation") {
		assert(!CharSet("abc")*>1 ~~= "def")
		assert(!CharSet("abc")*>1 !~~= "dbf")
	}

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

	test("Char class subtraction") {
		assert((CharSet("abc") - CharSet("cde")) !~= "c")
		assert((CharSet("abc") - "c") !~= "c")
	}

	test("CharClass union and subtraction operations") {
		val alnum = Chars.Alphabetic.Digit
		assert(alnum*>0 ~~= "abc123")
		val alpha = Chars.Alphanumeric - Chars.Digit
		assert(alpha*>0 ~~= "abc")
		assert(alpha*>0 !~~= "abc123")
		// TODO intersection
	}
}

class RexSuite extends FunSuite {
	test("Automatic grouping to keep precedences working properly") {
		assert(("a" +~ "b" +~ "c").pattern === "abc")
		assert(("ab"*>0).pattern === "(?:ab){0,}")
		assert((("a"|"b") +~ ("c"|"d")).pattern === "(?:a|b)(?:c|d)")
		assert((CharSet("abc").pattern === "[abc]"))
		assert((CharRange('a','z') +~ "abc").pattern === "[a-z]abc")
		assert("abc".>>.pattern === "(?=abc)")
		assert(("a" +~ "bc" +~ "def").pattern === "abcdef")
	}

	test("Backreferences") {
		val simpleQuote = CharSet("\"'").name("quote") +~ Chars.Digit*>0 +~ SameAs("quote")
		assert(simpleQuote ~~= "\"123\"")
		assert(simpleQuote ~~= "'123'")
		assert(simpleQuote !~~= "\"123'")
		val badQuote = CharSet("\"'").name("quote") +~ Chars.Digit +~ SameAs("quot")
		intercept[NameException] { badQuote  ~~= "'123'" }
		val quoteMark = CharSet("\"'").name("quote")
		val stringBody = ("\\" +~ Chars.Any | SameAs("quote").!>> +~ Chars.Any) *>0
		val quotedString = quoteMark +~ stringBody +~ SameAs("quote")
		assert(quotedString ~~= "\"Don't say \\\"No\\\"!\"")
		assert(quotedString ~~= "'Don\\'t say No!'")
		assert(quotedString !~~= "\"Don\\'t say No!'")
	}

	test("Group naming exceptions") {
		intercept[NameException] { ("a".name("char") +~ "b").name("char") }
		intercept[NameException] { "a".name("char+=") }
		intercept[NameException] { "a".name("char") +~ "b".name("char") }
	}

	test("~= and ~~= match within string and exact string respectively") {
		assert(Chars.Any *<0 !~~= "Hello")
		assert("Hello" ~~= "Hello")
		assert("Hello" !~~=  "Hey, Hello")
		assert("Hello" !~~=  "Hello, Hey")
		assert("ello" ~= "Hello")
	}

	test("special characters are escaped correctly in Lit") {
		assert("[]^$\\.{,}|+*<" ~~= "[]^$\\.{,}|+*<")
	}

	test("Boundary patterns") {
		assert(Input.Start +~ "a" +~ Input.End ~~= "a")
		assert(Input.Start +~ "a" +~ Input.End +~ "a" !~~= "aa")
		assert(Input.Start +~ "a"*>0 +~ Input.End ~~= "aaa")
		assert(Input.Start +~ "a"*>(0,2) +~ Input.End !~~= "aaa")
		assert(Line.Start +~ "A"*<0 +~ Line.End ~= "BBB\nAAA\nCCC\n")
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

	test("Lookahead and lookback operators") {
		assert(("a" +~ "b".>>) +~ "b" ~~= "ab")
		assert((("a" +~ "b".>>) +~ "bb") !~~= "ab")
		assert((("a" +~ "c".!>>) +~ "b") ~~= "ab")
		assert((("a" +~ "c".!>>) +~ "c") !~~= "ac")
		assert((("x".!<< +~ "a") +~ "b") ~~= "ab")
		assert("x" +~ CharRange('a', 'z').>> ~= "axb")
		assert(CharRange('a', 'z').<< +~ "." ~= "a.")
		assert(CharRange('a', 'b').<< +~ "." +~ CharRange('a', 'b').>> !~= "x.y")
		assert(CharRange('a', 'b').<< +~ "." +~ CharRange('a', 'b').>> ~= "x.y a.b")
		assert(CharRange('a', 'z').<< +~ "." +~ CharRange('a', 'z').>> !~= "a .b")
		assert(CharRange('a', 'z').<< +~ "." +~ CharRange('a', 'z').>> !~= "a. b")
		//println("<"+(CharRange('a', 'z') <=: Lit(".") :=> CharRange('a', 'z')).findFirstIn("a.b").get +">")
		assert((CharRange('a', 'z').<< +~ Lit("^") +~ CharRange('a', 'z').>>).findFirstIn("a.b a^b").get.string === "^")
	}

	test("MatchOperators") {
		assert(Lit("aa") ~= "bbaabb")
		assert(Lit("a")*<1 !~~= "aaa")
		assert(Lit("\"\"\"") +~ Chars.Any*<0 +~ "\"\"\"" ~~= "\"\"\"abc\"\"\"")
		assert("\"" +~ ((Lit("\\") +~ Chars.Any) | !CharSet("\""))*<0 +~ "\"" ~~= "\"A string\\n\\\" thing.\"")
	}

	test("Repetition type (greedy, minimal, possessive)") {
		assert("a"*>1 ~~= "aaa")
		// The nongreedy match should fail because it only takes the first character.
		assert("a"*<1 !~~= "aaa")
		assert("a"*!1 ~~= "aaa")
		assert("ab"*>1 ~~= "ab")
		assert("ab"*>1 ~~= "ababab")
		assert("ab"*<1 +~ "ab"*>1 ~~= "ababab")
		assert("ab"*>1 +~ "ab"*<1 ~~= "ababab")
		//This next should fail; the possessive operator takes all the input, and allows
		// for no backtracking.
		assert("ab"*!1 +~ "ab"*>1 !~~= "ababab")
	}

	test("Optional") {
		assert(Lit("a") +~ Lit("b").? ~~= "a")
	}

	test("Tokenizer") {
		val t = new Tokenizer(
			(mr: MatchResult) => "?",
			Seq(
				Lit("a") -> ((mr: MatchResult) => "1"),
				Lit("b") -> ((mr: MatchResult) => "2")
			)
		)
		assert(t.tokenize("fabaabbc").mkString === "?121122?")
	}

	test("flags") {
		assert("Hello".ASCIICaseInsensitive ~~= "HELLO")
		assert("Hello".ASCIICaseSensitive.ASCIICaseInsensitive !~~= "HELLO")
		assert("Héllo".ASCIICaseInsensitive !~~= "HÉLLO")
		assert("Héllo".UnicodeCaseInsensitive ~~= "HÉLLO")
		assert("Héllo".UnicodeCaseSensitive.UnicodeCaseInsensitive !~~= "HÉLLO")
	}
}

class ComplexPatternsSuite extends FunSuite {

	test("Complex number pattern") {
		// A complex is a float followed by a + or - followed by a float, followed by an "i"
		// The two numeric parts and the sign are named for access.
		val complexMatcher = Number.SignedFloat.name("re") +~ ("-"|"+").name("sign") +~ Number.SignedFloat.name("im") +~ "i"
		/** Match against a floating-point complex number and print the result. */
		val found: Option[MatchResult] = complexMatcher.findFirstIn("3.2+4.5i")
		val complex = found match {
			case None => None
			case Some(mr) => mr("re") + " " + mr("sign") + " " + mr("im") + "i"
		}
		assert(complex === "3.2 + 4.5i")

		val doubleMatcher = complexMatcher.name("num1.") +~~ complexMatcher.name("num2.")
		val doubleResult = doubleMatcher.findFirstIn("1+2i 3+4i").get
		assert(doubleResult("num1.re") === "1")
		assert(doubleResult("num2.im") === "4")
	}

	test("HTML tag pattern") {
		val tagPat = ("<" +~ Chars.Any*<1 +~ ">").name("tag")
		assert(tagPat ~~= "<a count=0>")
		val minFind:String = tagPat.findFirstIn("<a><b>") match {
			case None => ""
			case Some(m) => m("tag")
		}
		assert(minFind === "<a>")
	}
}