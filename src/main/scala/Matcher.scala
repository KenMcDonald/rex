package com.digitaldoodles.rex

import util.matching.Regex

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 6/14/11, 9:14 PM
 * License: LGPL
 */

private object Matcher {
	implicit def stringToLit(s: String) = Lit(s)

	def publicGroupNamePat = CharSet("_").Alphabetic +~ CharSet("_").Alphanumeric*>0 +~ ".".?
	def privateGroupNamePat = CharSet("_$").Alphabetic +~ CharSet("_$").Alphanumeric*>0 +~ ".".?

	/** Add a backquote immediately before any characters that have a special
	 * meaning in character classes. */
	private[rex] def backQuoteCharClassSpecials(s: String): String =
		(for (c <- s) yield (if ("[]-^&\\.{}".indexOf(c) >= 0) "\\" + c else c)).mkString("")
}

/** Matcher is the root class for all other classes that implement regular expression matching.
 *
 * @param regex The regular expression that will be stored internally by the Matcher instance, and used by
 * [[scala.util.matching.Regex]] to perform the actual pattern matches.
 */
abstract private[rex] class Matcher {
	private[rex] val nameToGroupNumber: Map[String, Int] = Map[String, Int]()
	private[rex] def groupCount = 0

	private[rex] def buildPattern(nameMap: Map[String, Int]): String;
	lazy val pattern = buildPattern(nameToGroupNumber)
	lazy val regex = new Regex(pattern)
	/** Returns the Scala regex for this Matcher--supported to be in accord with Scala's "stringpattern".r */
	def r = regex

	/**Defines the precedence of the _lowest_-precedence regular expression operator in
	 * this Matcher's pattern. In this case we count 0 as the highest precedence, as as
	 * numbers go up, preceduce goes down. Precedences are:
	 *
	 * - 0: The precedence for character classes, patterns enclosed in a group, or single-character literals.
	 * - 1: Repetition-type operators, eg. "a*" or "b{2,3}" or "c?".
	 * - 2: Concatenated patterns, including multi-character literals.
	 * - 3: The | alternation operator.
	 */
	private[rex] val lowestPrecedenceInPattern: Int;

	/** Encloses `this` in an anonymous name--one that cannot
		 be accessed and does not contribute to the numbering of groups. Does nothing if `this`
		 is already in a name of some sort. */
	private[rex] def anonGroup(nameMap: Map[String, Int]) = "(?:" + this.buildPattern(nameMap) + ")"

	/** `A +~ B` tries to match against input by matching A against the initial part of the input and,
	 * if A succeeds, matching B against the following part of the input.
	 *
	 * NOTE: The symbol "+~" was chosen to avoid confusion with String.+ in the presence of implicit conversions
	 * from strings to pattern. "&" was also tried, but its ranking in the operator precedence hierarchy made
	 * for some unexpected associations in patterns. */
	def +~(other:Matcher) = new BinopMatcher(this, "", other)

	/**`A +~~? B` is the same as `A +~ (patterns.CharWhitespace *> 0) +~ B`. In other words, `A` followed by `B`,
	 * optionally separated by whitespace.
	 */
	def +~~?(other: Matcher) = this +~ (Chars.Whitespace *> 0) +~ other

	/**`A +~~ B` is the same as `A +~ (patterns.CharWhitespace *> 1) +~ B`. In other words, `A` followed by `B`,
	 * separated by at least some whitespace.
	 */
	def +~~(other: Matcher) = this +~ (Chars.Whitespace *> 1) +~ other

	/** `A | B` matches if either A matches against the input, or if B matches against the input. */
	def |(other:Matcher) = new BinopMatcher(this, "|", other)

	/** `A*>n` matches by first matching A against the input at least n times, and
	 * then as many more times as possible. Most commonly, this is used as `A*>0` or `A*>1`*/
	def *>(count:Int) = new RepMatcher(this, "{" + count + ",}")

	/** `A*<n` matches at least n instances of A, and then as few more as possible. */
	def *<(count:Int) = new RepMatcher(this, "{" + count + ",}?")

	/** Matches `count` or more instances of `this`, possessively.
		 When a substring is matched possessively, backtracking into that substring to further
		 the match as later processing is done is not allowed; the substring must either be accepted
		 in its entirety within the possessive match, or rejected entirely. Depending on the regular
		 expression, this can significantly speed things up. It can also alter the behavior of patterns
		 in difficult-to-predict ways, so use with caution. */
	def *!(count:Int) = new RepMatcher(this, "{" + count + ",}+")

	/** Greedily matches at least m and at most n instances of `this`, where m and n are
		 the two elements of the `count` argument.

		 @param count A 2-tuple of integers, defining a minimum and maximum number of repetitions
		 of the pattern specified by `this`. */
	def *>(count: (Int,Int)) = new RepMatcher(this, "{" + count._1 + "," + count._2 + "}")

	/** Non-greedily matches at least m and at most n instances of this, where m and n are
		 the two elements of the `count` argument.

		 @param count A 2-tuple of integers, defining a minimum and maximum number of repetitions
		 of the pattern specified by `this`. */
	def *<(count: (Int,Int)) = new RepMatcher(this, "{" + count._1 + "," + count._2 + "}?")

	/** Possessively matches at least m and at most n instances of this, where m and n are
		 the two elements of the `count` argument.

		 @param count A 2-tuple of integers, defining a minimum and maximum number of repetitions
		 of the pattern specified by `this`. */
	def *!(count: (Int,Int)) = new RepMatcher(this, "{" + count._1 + "," + count._2 + "}+")

	def optional = this *> (0,1)
	def ? = optional

	/** Make this pattern into a named name; the contents of the name,
		 from a match, will be retrieved by its name, not by a number.

		 @param name The name by which the contents matching the name may be extracted
		 following a successful match. See the documentation for `MatchResult` for details
		 on how to extract named groups.
	 */
	def name(groupName: String) = {
		if (Matcher.publicGroupNamePat !~~= groupName) {
			throw new NameException("Attempt to name group with illegal name: '%s'" format groupName)
		}
		new GroupMatcher(this, groupName)
	}

	def internalName(groupName: String) = {
		if (Matcher.privateGroupNamePat !~~= groupName) {
			throw new NameException("Attempt to name group with illegal name: '%s'" format groupName)
		}
		new GroupMatcher(this, groupName)
	}

	/** Wherever `this` matches within `target`, replace the
	 *	matched subsequence with `target`.
	 *
	 *	@param target The string in which replacements will be done.
	 *
	 *	@param replacement The string that will be substituted for whatever matched `this`.
	 */
	def replaceAllIn(target:String, replacement:String) = regex.replaceAllIn(target, replacement)

	/** Returns true if `this` exactly matches `target`, false otherwise. Note that by
	 * the rules of Scala precedence, the trailing = gives this operator the lowest possible
	 * precedence, which works well in practice. */
	def ~~=(target: String): Boolean = {
		regex.findFirstIn(target) match {
			case None => false
			case Some(m) => m == target
		}
	}

	/** Returns true if `this` matches somewhere in `target`, false otherwise. Note that by
	 * the rules of Scala precedence, the trailing = gives this operator the lowest possible
	 * precedence, which works will in practice.*/
	def ~=(target: String): Boolean = {
		regex.findFirstIn(target) match {
			case None => false
			case Some(m) => true
		}
	}

	/** Negation of ~=. Note that by
	 * the rules of Scala precedence, the trailing = gives this operator the lowest possible
	 * precedence, which works will in practice.*/
	def !~=(target: String) = !(this ~= target)

	/** Negation of ~~=. Note that by
	 * the rules of Scala precedence, the trailing = gives this operator the lowest possible
	 * precedence, which works will in practice.*/
	def !~~=(target: String) = !(this ~~= target)

	/** Find and return the first substring in `target` matching `this`. */
	def findFirst(target: String): Option[MatchResult] = {
		regex.findFirstMatchIn(target) match {
			case None => None
			case Some(m) => Some(new MatchResult(true, m, null, this))
		}
	}

	def apply(target: String) = new Matching(this, target)

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
	def lookahead = new AnonGroup("=" + this.pattern + ")")
	def >> = lookahead

	/** Returns a negated lookahead pattern match.

		 @return A rex pattern which succeeds only if `this` does
		 not match the next piece of the string being matched against; does
		 not consume the next piece of the string.
	 */
	def notlookahead = new AnonGroup("!" + this.pattern + ")")
	def !>> = notlookahead

	/** Returns a lookback pattern match.

		 @return A rex pattern which succeeds only if `this` matches
		 the previous piece of the string being matched against; does
		 not consume any of the input.
	 */
	def lookback = new AnonGroup("<=" + this.pattern + ")")
	def << = lookback

	/** Returns a negated lookback pattern match.

		 @return A rex pattern which succeeds only if `this` does
		 not match the previous piece of the string being matched against; does
		 not consume any of the input.
	 */
	def notlookback = new AnonGroup("<!" + this.pattern + ")")
	def !<< = notlookback
}
