/*
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-01-19, 2:56 PM
 * License: LGPL
 */

package com.digitaldoodles.rex
import scala.util.matching.Regex

object Implicits {
	implicit def stringToLit(s:String) = Lit(s)
}
private object Matcher {
	implicit def stringToLit(s:String) = Lit(s)

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
	val regex = new Regex(string)
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

	/**`A ~ B` is the same as `A & (patterns.CharWhitespace ** 0) & B`. In other words, `A` followed by `B`,
	 * optionally separated by whitespace.
	 */
	def ~(other: Matcher) = this & (patterns.CharWhitespace ** 0) & other

	/**`A ~~ B` is the same as `A & (patterns.CharWhitespace ** 1) & B`. In other words, `A` followed by `B`,
	 * separated by at least some whitespace.
	 */
	def ~~(other: Matcher) = this & (patterns.CharWhitespace ** 1) & other

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
	def findAllIn(target: String) = new MatchResultIterator(target, regex.findAllIn(target).matchData, this)

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
 their special meanings in the string argument.
 */
case class Lit(lit: String) extends Matcher(Matcher.backQuoteLiteralSpecials(lit)) {
	override val grouped = true
}

/** Represents an instance of an anonymous (unnumbered, unnamed) group. */
protected class AnonGroup(pattern:String) extends Matcher(pattern) {
	// TODO Check to ensure pattern is in fact an anonymous group.
	//require(pattern.subString(0,))

	override val grouped = true
}

/** Used in constructing binary rex operations. */
protected[rex] class BinopMatcher(val m1:Matcher, op:String, val m2:Matcher) extends Matcher(m1.anonGroup + op + m2.anonGroup) {

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






