/*
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-01-19, 2:56 PM
 * License: LGPL
 */

package com.digitaldoodles.rex
import scala.util.matching.Regex

/**
 * =Introduction=
 *
 * This documentation assumes that you already understand the basic concepts
 * of regular expressions and their uses. Future versions will, it is hoped, assume
 * more of a tutorial nature.
 *
 * Before becoming more formal, let me give you an example of how rex makes it
 * easy to build complex regular expressions from simpler ones. Here's how to build
 * a regular expression that recognizes complex numbers:
 * {{{
 *   val posInt = CharRange('0','9')*1 // *1 means 1 or more
 *   val sign = Lit("+")|"-" // Lit means literal. The "-" is automatically converted to a literal.
 *   val optionalSign = sign.optional
 *   val floatPat = optionalSign & posInt & (Lit(".") & posInt).optional // & represents concatenation
 *   val complex = floatPat.group("re") & sign.group("op") & floatPat.group("im") & "i" // "group" creates _named_ groups.
 * }}}
 * When the complex pattern is used to find a complex number, the real and imaginary parts,
 * and operator, can be pulled out of the MatchResult by name: 're', 'im', and 'op'.
 *
 * By the way, I haven't run the above code, so it may have some errors. But you can
 * see real code for this task in the rex test suite.
 *
 * Notice how regular expressions in rex can be easily composed, without the necessity of worrying
 * about parenthisation and precedence. This is in sharp contrast to trying to build regular
 * expressions manually, from smaller regular expressions. In rex, anonymous groups are added
 * as needed. (If you don't know what an anonymous group is, don't worry. It's all taken care of
 * for you.)
 *
 * Here's a more formal description of the
 * package.
 *
 * =Goals for `rex`=
 *
 * The `rex` package provides an enhanced interface to regular expressions. It
 * has four major goals:
 *
 * - Allows regular expressions to be built in small pieces and then easily
 * composed into larger regular expression. This enhances readability, and makes
 * it easy to test the subparts of a complex regular expression as standalone entities.
 * Separate regular expressions can be used to build new regex's with various operators
 * such as **, &, and |, and further operators exist for character classes.
 *
 * - Allow groups to be named when they are defined, and then accessed by their
 * names. This is different from Scala, where the onus is on the programmer to ensure
 * that (if provided) a list of group names correctly corresponds to the parentheses
 * in a regular expression.
 *
 * - Provides a more flexible mechanism for iterating through a `findAllIn` regular
 * expression search. In particular, when a regular expression is used to iterate
 * over a target string, both matches and non-matches are returned as `MatchResult`
 * objects; the value of a `matched` boolean variable determines whether
 * this represents a successful match or not. This permits one to easily process
 * all of a string, or just those parts of it that matched, or just those parts that
 * didn't.
 *
 * - Provides useful predefined regular expressions. For example, there is a
 * predefined `PatFloat` that will match floating point numbers. Since
 * regular expressions can be easily combined, the regex for a complex number now
 * becomes just `PatFloat.group("re") & (Lit("-")|"+").group("sign") & PatFloat.group("im") & "i"`.
 *
 * =Constructors and Operators for Producing Rex Regular Expressions=
 *
 * rex can be considered to be composed of several common constructors or operators (corresponding to
 * the constructs most often used in regular expressions), and several less common constructs (corresponding,
 * not surprisingly, to those constructs used less often in regular expressions.). Each construct is
 * explained in detail in its method documentation. Here, we give a very brief overview, with the
 * intent that the reader will then understand what is most important, and what is less important,
 * in regular expressions.
 *
 * In addition, `rex` can test for exact matches, substring matches, or iterative searches. This will
 * be touched on later in this description, and will be documented thoroughly in the appropriate methods.
 *
 * @note `+` is not a valid method of constructing a regular expression, and when used in this manner,
 * will (usually) throw an exception. The reason for this is to avoid confusion with string concatenation,
 * as `rex` regular expressions use implicits to (when appropriate) convert from string to literal regular
 * expressions (see below). Instead of `+`, see `&`.
 *
 * == Common Methods of Building Regular Expressions ==
 *
 * The most common methods of constructing rex regular expressions are:
 *
 * - `Lit("abc")`: This is the most fundamental construct of regular expressions. In Scala (there are
 * other language variants of `rex`), `Lit` is a case constructor, and it constructs a regular expression
 * that exactly matches the given string. In rex, there are no special characters; any character in the
 * argument string is treated as that literal character.
 *
 * - ''rex1'' `&` ''rex2'': Constructs a regular expression which matches the regex in ''rex1'' followed by
 * the regex in 'rex2'. See the documentation on `&` for details.
 *
 * - ''rex1'' `|` ''rex2'': Constructs a regular expression which matches the regex in ''rex1'' or
 * the regex in ''rex2''. See the documentation on `|` for details.
 *
 * - ''rex1'' `**` ''int'': Constructs a (greedy) regular expression which matches ''int'' or more instances of ''rex1''. See
 * the documentation on `**` (with a single int argument) for more details. Also see the section on "Greedy, Non-Greedy, and
 * Possessive Matches" for details on greediness.
 *
 * - ''rex1'' `** (`''int1, int2''`)`: Constructs a (greedy) regular expression which matches at least ''int1'' and
 * at most ''int2'' of ''rex1''. See the documentation on `**` (with a 2-tuple of ints for an argument) for details. Also
 * note the comments for `**`''int''.
 *
 * - `CharSet(`''string''`)`: Constructs a regular expression that matches any single character in ''chars''.
 * Unlike normal regular expressions, no characters have special meanings in ''chars''; any such characters
 * are automatically escaped before being passed to [[scala.util.matching.Regex]]. See the documentation
 * on `CharSet` for details.
 *
 * - `CharRange(`''charStart''`,`'charEnd'`)`: Constructs a regular expression which matches any single character
 * in the range ''charStart'' to ''charEnd'', inclusive. Character ordering is as per [[scala.util.matching.Regex]].
 * See the documentation on `CharRange` for details.
 *
 * -''r''.group(''name''): Does not change the semantics of a rex expression, but identifies the expression as one that,
 * upon a successful match, may be extracted from the match using the group name. See [[ykken.rex.MatchResult.group]]
 * for details.
 *
 * - Implicit conversion of strings to literal regexes: If ''rex'' is a rex regex, and ''op'' is a method defined on it
 * for combining it with another rex regex, then an implicit conversion means that a standard string following ''op''
 * will be converted to a rex literal regex.
 *
 * - Predefined patterns: The package ykken.rex.patterns provides a number of useful predefined patterns for use with
 * `rex`. Some of these patterns map one-to-one onto constructs found in the Java regular expression package, while
 * others are more complex `rex` constructs.
 *
 * ==Less Common Methods of Building Regular Expressions==
 *
 * These methods for building regular expressions are less commonly encountered than the above. They can be very
 * powerful, but can also make it much more difficult to debug regular expressions, so be forewarned!
 *
 * - `*?` and `*+`: Like `**`, except non-greedy and possessive, respectively. See the section on "Greedy, Non-Greedy, and
 * Possessive Matches" for details.
 *
 * - `|>`, `!|>`, `<|`, and `!<|`: These are lookahead/lookback operators. Basically, starting from wherever the current
 * match position is in the string being matched against, they look ahead or backwards for another regex, __without__
 * actually consuming any input. (In other words, the current match position is not changed by these operators.) Mnemomics
 * to remember these by are: `|` indicates the operation is being done by the current match position (supposed to
 * remind you of a cursor), `>` says lookahead for a regular expression, `<` says look backward for a regular expression,
 * and `!` is negation--in other words, `!|>` matches only if its regex does __not__ match whatever immediately follows
 * the current cursor. There are some restrictions on lookahead/lookback operators, consult the documentation for the
 * specific operator. Also, there are longer form names for these, if you don't like the operator syntax.
 *
 * ==Methods for Building Regular Expressions from `CharSet`'s and `CharRange`'s==
 *
 * There are three special operators that can be uses to construct regexes from `CharSet`'s/`CharRange`'s.
 *
 * - `\/`: Binary union operator. Given CharSets or CharRanges as its arguments, produces a new character class that
 * matches any single character in either of the arguments.
 *
 * - `/\`: Binary intersection operator. Given CharSets or CharRanges as its arguments, produces a new character class that
 * matches any single character in both argument char classes. Due to restrictions in the underlying regex code, a
 * character class produced in this manner may not be subsequently used with the special 'character class' operators.
 * It may be freely used as a standard regex with other operators.
 *
 * - `-`: Binary subtraction operator. Given CharSets or CharRanges as its arguments, produces a new character class
 * that matches any single character in the first argument, __unless__ it is also in the second argument. Due to restrictions
 * in the underlying regex code, a
 * character class produced in this manner may not be subsequently used with the special 'character class' operators.
 * It may be freely used as a standard regex with other operators.
 *
 * =Full and Substring Matches=
 *
 * rex offers two matching operators, `~=` and `~~=`. These are discussed below. Whenever you use one of these two
 * operators, the result is a `Boolean`.
 *
 * `~=` is a "substring match"; it succeeds if the regex matches anywhere in the given string. So, for example, all of
 * the following matches succeed:
 * {{{
 * Lit("b") ~= "abc" // matches the middle "b".
 * CharSet("ab")**0 ~= "aababa" //matches the entire string "aababa".
 * (Lit("a")|"b")**1 ~= "ccacc" // matches the middle "a".
 * }}}
 *
 * `~~=` is an "identity match"; it matches only if the given regex matches the given string __in its entirety__.
 *
 * =Working with `MatchResult`s=
 *
 * The result of a match of a pattern against a string (whether the match was successful or not) is a `MatchResult`.
 * Such results indicate a successful or unsuccessful match(es), allow one to iterate over the sucessful, unsuccessful,
 * or both, parts of a match, and in general allow various other things, such as named access to successful parts of
 * a match. In general, `MatchResult` objects are significantly more powerful than the match results returned by Scala
 * (or by other languages, for that matter.)
 *
 * ==Extracting Information From Matches==
 *
 * One of the most common operations to do with regular expressions is to extract some substring from a given match.
 * Typically, this is done by organizing sections of the regular expression into groups using parentheses, and then
 * specifying the contents of a group according to the numbering of the leftmost parenthesis in the group. This approach
 * is difficult and fragile, and very brittle--changing the groups in a regex also involves tracking down and changing
 * all the numeric references to those groups.
 *
 * Some languages, such as Python, allow named groups. When you group together a section of a regex, you also assign
 * it a name, and that name is used to extract the contents of that particular substring of the regex. This mode of
 * work is slightly more verbose, but far more error-resistant than the numeric mode.
 *
 * In rex, the standard method for extracting information from regexes is to use named groups. It is possible to extract
 * substring matches using numbers, but there is not even an option for defining groups that are not named. This is
 * intentional. The advantages of named groups over numbered groups are so numerous that I'm not even going to list
 * them here.
 *
 * The standard way of defining a group in rex is just:
 * {{{
 * r.group("name")
 * }}}
 * where ''name'' is the name desired for the given group. This should be different from any name assigned to any other
 * group.
 *
 * When a match has successfully been performed, you'll get a `MatchResult` object as the result. If we call this object
 * ''m'', and the group ''name'' successfully matched in ''m'', then we can obtain the substring that was matched via
 * ''m''.group(''name'').
 *
 * =Greedy, Non-Greedy, and Possessive Matches=
 *
 * This is possibly the most confusing aspect of regular expressions. I hope to be able to shed some light on the matter,
 * but we shall see.
 *
 * ==Greedy Expressions==
 *
 * First, consider the expression
 * {{{
 * Lit("a")**1 & Lit("a")**1 ~~= "aaaa"
 * }}}
 * Should this match succeed for fail? We are asking for one or more instances of "a" followed by one or more instances of "a".
 * Depending on how the operator `**` actually works, we could have multiple possible solutions, or we could have no
 * match at all.
 *
 * As it turns out, `**` is 'greedy' but also 'backtracking'. This means the match process will be carried out as follows:
 * 1. The first Lit("a")**1, on its first go-around, matches all of the "a"'s, leaving nothing for the second Lit("a")**1.
 * So on this attempt, the match fails.
 * 2. BUT, `**` is backtracking, which means that it is willing to give up parts of matches in order to make the whole
 * match succeed. In this case, the first Lit("a")**1 goes back to matching only the first three "a"'s, leaving one "a"
 * for the second Lit("a")**1, and the match succeeds.
 *
 * The greedy, backtracking operator is what you will (probably) normally use. It's been around the longest, and has proven
 * the most useful.
 *
 * ==Non-greedy Matching==
 *
 * However, there are two other ways of performing matches: Non-Greedy and Possessive. Let's look at non-greedy first.
 * {{{
 * Lit("a")*?1 & Lit("a")*?1 ~~= "aaaa"
 * }}}
 * The big change here is the change from the greedy repetition operator (`**`) to the 'nongreedy' repetition operator,
 * `*?`. The question is will the match succeed?
 *
 * The answer is no, it will not, by definition, nongreedy operators consume as little input as possible. So the first
 * Lit will consume one "a", the second Lit will consume one "a", and there will be two unmatched "a"'s left over.
 *
 * There are a number of ways of fixing this (assuming it needs to be fixed). One such way is as follows:
 * {{{
 * Lit("a")*?1 & Lit("a")*?1 & BndryStringEnd ~~= "aaaa"
 * }}}
 * `BndryStringEnd` is a predefined pattern (from ykken.rex.patterns) which matches the end of a string. Nongreedy
 * operations will still consume more than the absolute minimum if that is required for the rest of the match to
 * succeed. So in this case, the first Lit consumes one "a", the second Lit tries to consume only one "a" but is
 * "pulled" to consume more to satisfy the BndryStringEnd condition, and ends up consuming all the rest of the "a"'s;
 * and BndryStringEnd matches at the end of the string, so the whole thing succeeds.
 *
 * ==Possessive Matching==
 *
 * The final form of matching is the so called 'possessive' case. This is like greedy matching, but without the backtracking;
 * possessive matches never give up something they've matched.
 *
 * Let's look at a simple example, where `*+` is the possessive repetition operator. Here we go:
 * {{{
 * Lit("a")*+1 & Lit("a")**1 ~~= "aaaa"
 * }}}
 * At first glance, this doesn't seem so different from the 'greedy' example we looked at before. All we've done is to
 * substitute `*+` for `**` in one of the Lit clauses. But that makes all the difference in the world.
 *
 * `*+` says, "take as much of the input as you can" (which is the same as `**`), but then goes on to say, "if you've
 * grabbed some of the input, never give it up". So what will happen?
 *
 * 1. The `Lit("a")*+1` clause will grab all of the "a"'s. Having grabbed them, it will never give them up.
 * 2. There is nothing left for the `Lit("a")**1` to grab! The match fails.
 *
 * Notice the difference between backtracking behavior and non-backtracking behavior. It literally is a matter
 * of being willing to backtrack your own footsteps. If you are willing to do so, you may find a match. If not,
 * then your chances of finding a match of greatly reduced.
 *
 * Why would one ever wish to engage in possessive (non-backtracking) behavior, since is reduces the chance of
 * finding a match? There are several reasons, but the basic answer is efficiency. If you don't have to backtrack
 * and try alternative methods of obtaining a match, you can obtain huge performance gains, if the match is over
 * a large string.
 *
 * =Looping Through Matches=
 * rex possesses a somewhat different approach to looping through matches, which is a conclusion I've had from a great
 * deal of regex code. Typically packages loop through successful matches. I've found, as often as not, that one
 * also wants to know the __unsuccessful__ matches between the __successful__ matches. As a result, rex ''matches''
 * actually iterate through the entire matched string, giving the user the option of deciding if they want to
 * extract ''matched'' matches, ''unmatched'' matches, or both.
 *
 * The iteration is done through standard Scala ''for...yield'' constructs. Let's take a look.
 * {{{ for(m <- Lit("a").findAllIn("aabbabb") if (m.matched)) yield m.string }}}
 * Here we see the `findAllIn` method, which in this context, yields a series of [ykken.rex.MatchResult]] objects. These
 * objects have a number of interesting characteristics--for example, they can return substrings of the match based
 * on name (defined at the time the regex was defined) rather than based on parenthesis count. However, the most
 * surprising thing about them is that they don't just return matched results! Instead, they include a `matched`
 * attribute, which can be checked as part of the generator. In the above, we would receive a sequence of three
 * MatchResults, each containing the single character "a".
 *
 * Let's take a look at another example, taken directly from the test suite:
 * {{{assert( (for(m <- Lit("a").findAllIn("aabbaba")) yield m.string).mkString("") === "aabbaba")}}}
 * Obviously this won't work without the test scaffolding around it, but hopefully the intent is clear.
 * `findAllIn` will return __everything__, not just successful matches.
 *
 * So what, you say, is the point of this? Well, I've often needed to parse strings of the form
 * ''expr op expr op expr . . .''. Being able to specify a regex just for the `expr` portions, and being
 * able to extract the `op`'s without any further fiddling about is a major plus.
 *
 * And remember, the cost of adding on a filter to extract only the successful (or only the unsuccessful) matches is
 * almost nothing. So at a small cost in inconvenience, you've gained a great deal of expressive power.
 *
 * A final note: There is no reason to assume successful and unsuccessful matches will alternate. You could have three
 * successful matches in a row. However, you'll never have more than one unsuccessful match, because by definition, a
 * an unsuccessful match starts at the end of a succcessful match and goes to the beginning of the next successful
 * match. (OK, there are string start/end boundary conditions in here, but they don't change the principle.)
 *
 * =Replacing in Strings with Regexes=
 * First, let's be clear; we don't "replace" in a string, we create a new string with the given replacements. This is
 * particularly important given Scala's emphasis on a functional styl of programming. The original string is __never__
 * modified.
 *
 * Due to lack of time, I'm going to take an example exactly from the test suite:
 * {{{
 *     @Test def testReplaceAllIn() {
 *				val tagPat = "<td" & PatMinChars & ">"
 *				val target = "Hello<td style='color:black'>Goodbye<td >Now"
 *				assert (tagPat.replaceAllIn(target, "<td>") === "Hello<td>Goodbye<td>Now")
 *		}
 * }}}
 * `PatMinChars` is a predefined pattern that will match the minimum number of characters, consonant with
 * finding an overall match that works for the expression. Assuming we have conformant HTML (no extraneous < or >
 * characters), then `<td" & PatMinChars & ">` will match a "td" tag followed by some possible attributes, followed by
 * the closing ">".
 *
 * The key phrase here is ''tagPat.replaceAllIn(target, "<td>")''. It basically says, using the tag pattern (tagPat),
 * look through the entire target string, and whenever you find a match, substitute the second argument of `replaceAllIn`
 * (in this case, `<td>` for the found pattern.
 *
 * There are two instances of `td` in `target` that match. In both cases, `<td>` is substituted.
 */
package object rex {}

object Implicits {
	implicit def stringToLit(s:String) = Lit(s)
}


private object Matcher {
	/** Add a backquote immediately before any characters that have a special
	 * meaning in character classes. */
	def backQuoteCharClassSpecials(s:String):String =
		(for (c <- s) yield (if ("[]-^&\\.{}".indexOf(c) >= 0) "\\" + c else c)).mkString("")

	/** Add a backquote immediately before any characters that have a special meaning
	 * in regular expressions outside of character classes. */
	def backQuoteLiteralSpecials(s:String):String =
		(for (c <- s) yield (if ("[]^$\\.{,}|+*?".indexOf(c) >= 0) "\\" + c else c)).mkString("")

	/** Create a character class from a two sets of strings and a negated flag.
	*
	* Roughly speaking, the character class will look like [set1&&[<negated>set2]],
	* where <negated> may be a ^ or the empty string. The resultant character class
	* is either the instersection of set1 and set2 (if negated flag is empty) or
	* set1 - set2 if the negated flag is true. See the Java documentation
	* on regular expressions for details.
	*
	* @param set1 The set of characters which, subject to modification by set2,
	* is accepted by the character class.
	*
	* @param negated If true, then the character class will reject characters that
	* are in set2. If false, then the character class will match characters that are
	* in both set1 and set2.
	*
	* @param set2 If present, is used as above with the negated flag. If this is
	* absent (i.e. a zero-length string), then the entire character class will be
	* just of the form [set1], i.e. it will accept any character in set1.
	*/
	def assembleCharClass(set1:String, negated:Boolean, set2:String) = {
		val sign = if (negated) "^" else ""
		val clause2 = if (set2.length==0) "" else "&&[" + sign + set2 + "]"
		"[" + set1 + clause2 + "]"
	}
}

/**
 * @param string The regular expression that will be stored internally by the Matcher instance, and used by
 * [[scala.util.matching.Regex]] to perform the actual pattern matches.
 */
protected class Matcher(string:String) {
	private val regex = new Regex(string)
	//---------------------------
	def nameToGroupNumber = Map[String, Int]()
	def groupCount = 0
	protected val grouped = false

	/** Encloses `this` in an anonymous group--one that cannot
		 be accessed and does not contribute to the numbering of groups. Does nothing if `this`
		 is already in a group of some sort. */
	def anonGroup = {
		if (!this.grouped) "(?:" + this.pattern + ")"
		else this.pattern
	}

	/** Returns the regular expression patten corresponding to this rex entity. */
	def pattern = regex.pattern.pattern()

	/** Returns the regular expression patten corresponding to this rex entity. */
	def mkString = pattern

	/** Matches instances of `this` followed by `other`. */
	def &(other:Matcher) = new BinopMatcher(this, "", other)

	/** Catches and warns of the common error of using '+' for pattern concatenation. */
	def +(other:String) = {
		throw new RuntimeException("Cannot use '+' to concatenate pattern with pattern or string. Use '&' instead.")
		CharSet("a")
	}

	/** Catches and warns of the common error of using '+' for pattern concatenation. */
	def +(other:Matcher) = {
		throw new RuntimeException("Cannot use '+' to concatenate pattern with pattern or string. Use '&' instead.")
		CharSet("a")
	}

	/** Matches instances of `this` or `other`. */
	def |(other:Matcher) = new BinopMatcher(this, "|", other)

	/** Matches `count` or more instances of `this`, greedily. */
	def **(count:Int) = new Matcher(anonGroup + "{" + count + ",}")

	/** Matches `count` or more instances of `this`, non-greedily. */
	def *?(count:Int) = new Matcher(anonGroup + "{" + count + ",}?")

	/** Matches `count` or more instances of `this`, possessively.
		 When a substring is matched possessively, backtracking into that substring to further
		 the match as later processing is done is not allowed; the substring must either be accepted
		 in its entirety within the possessive match, or rejected entirely. Depending on the regular
		 expression, this can significantly speed things up. It can also alter the behavior of patterns
		 in difficult-to-predict ways, so use with caution. */
	def *+(count:Int) = new Matcher(anonGroup + "{" + count + ",}+")

	/** Greedily matches at least m and at most n instances of `this`, where m and n are
		 the two elements of the `count` argument.

		 @param count A 2-tuple of integers, defining a minimum and maximum number of repetitions
		 of the pattern specified by `this`. */
	def **(count: (Int,Int)) = new Matcher(anonGroup + "{" + count._1 + "," + count._2 + "}")

	/** Non-greedily matches at least m and at most n instances of this, where m and n are
		 the two elements of the `count` argument.

		 @param count A 2-tuple of integers, defining a minimum and maximum number of repetitions
		 of the pattern specified by `this`. */
	def *?(count: (Int,Int)) = new Matcher(anonGroup + "{" + count._1 + "," + count._2 + "}?")

	/** Possessively matches at least m and at most n instances of this, where m and n are
		 the two elements of the `count` argument.

		 @param count A 2-tuple of integers, defining a minimum and maximum number of repetitions
		 of the pattern specified by `this`. */
	def *+(count: (Int,Int)) = new Matcher(anonGroup + "{" + count._1 + "," + count._2 + "}+")

	def optional = this ** (0,1)

	/** Make this pattern into a named group; the contents of the group,
		 from a match, will be retrieved by its name, not by a number.

		 @param name The name by which the contents matching the group may be extracted
		 following a successful match. See the documentation for `MatchResult` for details
		 on how to extract named groups.
	 */
	def group(name:String) = new GroupMatcher(this, name)


	/** Wherever `this` matches within `target`, replace the
		 matched subsequence with `target`.

		 @param target The string in which replacements will be done.

	@param replacement The string that will be substituted for whatever matched `this`.
	 */
	def replaceAllIn(target:String, replacement:String) = regex.replaceAllIn(target, replacement)

	/** Returns true if `this` exactly matches `target`, false otherwise. */
	def ~~=(target:String):Boolean = {
		regex.findFirstIn(target) match {
			case None => false
			case Some(m) => m == target
		}
	}

	/** Returns true if `this` matches somewhere in `target`, false otherwise. */
	def ~=(target:String):Boolean = {
		regex.findFirstIn(target) match {
			case None => false
			case Some(m) => true
		}
	}

	/** Negation of ~=. */
	def !~=(target:String) = !(this ~= target)

	/** Negation of ~~=. */
	def !~~=(target:String) = !(this ~~= target)

	/** Find and return the first substring in `target` matching `this`. */
	def findFirst(target:String) : Option[MatchResult] = {
		regex.findFirstMatchIn(target) match {
			case None => None
			case Some(m) => Some(new MatchResult(true, m, null, this))
		}
	}


	/** This gives an iterator that iterates over both successful and failed
		 matches. (A failed match is the text between two successful matches.) In this
		 way, you have access to all the text in the target string.

		 To see how to access results look at `MatchResult`.
	 */
	def findAllIn(target:String) = new MatchResultIterator(target, regex.findAllIn(target).matchData, this)

	/** Returns a lookahead pattern match.

		 @return A rex pattern which succeeds only if `this` matches
		 the next piece of the string being matched against; does
		 not consume the next piece of the string.
	 */
	def lookahead = new AnonGroup("(?=" + this.pattern + ")")

	/** Shorthand for `lookahead`. */
	def |>() = lookahead

	/** Returns a negated lookahead pattern match.

		 @return A rex pattern which succeeds only if `this` does
		 not match the next piece of the string being matched against; does
		 not consume the next piece of the string.
	 */
	def notlookahead = new AnonGroup("(?!" + this.pattern + ")")

	/** Shorthand for `notlookahead`. */
	def !|>() = notlookahead

	/** Returns a lookback pattern match.

		 @return A rex pattern which succeeds only if `this` matches
		 the previous piece of the string being matched against; does
		 not consume any of the input.
	 */
	def lookback = new AnonGroup("(?<=" + this.pattern + ")")

	/** Shorthand for `lookback`. */
	def <|() = lookback

	/** Returns a negated lookback pattern match.

		 @return A rex pattern which succeeds only if `this` does
		 not match the previous piece of the string being matched against; does
		 not consume any of the input.
	 */
	def notlookback = new AnonGroup("(?<!" + this.pattern + ")")

	/** Shorthand for `notlookback`. */
	def !<|() = notlookback
}




/** Define a literal pattern. This pattern is quoted internally, so no regular expression
 characters have special meanings in it. Of course, string special characters still have
 their special meanings in the string argument. */
case class Lit(lit:String) extends Matcher(Matcher.backQuoteLiteralSpecials(lit)) {
	override val grouped = true
}




/** Represents an instance of an anonymous (unnumbered, unnamed) group. */
protected class AnonGroup(pattern:String) extends Matcher(pattern) {
	// TODO Check to ensure pattern is in fact an anonymous group.
	//require(pattern.subString(0,))

	override val grouped = true
}




/** Used in constructing binary rex operations. */
protected class BinopMatcher(val m1:Matcher, op:String, val m2:Matcher) extends
Matcher(m1.anonGroup + op + m2.anonGroup) {

	/** Number of non-anonymous (counting) groups in this regular expression. */
	override def groupCount = m1.groupCount + m2.groupCount

	/** Given a group name, return its corresponding group number.

		 @see Matcher.group(name)
	 */
	override def nameToGroupNumber = m1.nameToGroupNumber ++
			m2.nameToGroupNumber.map(x => (x._1, x._2 + m1.groupCount))
}

/** Responsible for numbering of named groups.*/
protected class GroupMatcher(pat:Matcher, val name:String) extends Matcher("("+pat.pattern+")") {
	/** Creates a new map of group names to left parenthesis numbers by mapping over the previously existing
				map. */
	override val nameToGroupNumber:Map[String, Int] = Map(name -> 1) ++ pat.nameToGroupNumber.map(x => (x._1, x._2 + 1))

	/** If a pattern is already grouped, we can sometimes avoid surrounding it with the "(?:...)" pattern. */
	override val grouped = true

	override def groupCount = 1 + pat.groupCount
}






