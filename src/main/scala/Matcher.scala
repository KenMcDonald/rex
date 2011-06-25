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
	///////////////////////////////////////////////////////////
	// Non-public API
	///////////////////////////////////////////////////////////
	private[rex] val nameToGroupNumber: Map[String, Int] = Map[String, Int]()
	private[rex] def groupCount = 0

	private[rex] def buildPattern(nameMap: Map[String, Int]): String;

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

	private[rex] def internalName(groupName: String) = {
		if (Matcher.privateGroupNamePat !~~= groupName) {
			throw new NameException("Attempt to name group with illegal name: '%s'" format groupName)
		}
		new GroupMatcher(this, groupName)
	}

	///////////////////////////////////////////////////////////
	// Pattern and Regex
	///////////////////////////////////////////////////////////
	/** The text specification of a pattern that is fed into the Java regex engine to generate an actual regex. */
	lazy val pattern = buildPattern(nameToGroupNumber)
	/** The Java/Scala Regex instance that does all the real work. */
	lazy val regex = new Regex("(?m:" + pattern + ")") /* We use ?m so that ^ and $ match at the beginning and end of lines */
	/** Returns the Scala regex for this Matcher--supported to be in accord with Scala's "stringpattern".r */
	def r = regex

	///////////////////////////////////////////////////////////
	// Pattern Creation Methods
	///////////////////////////////////////////////////////////
	/** `A +~ B` tries to match against input by matching A against the initial part of the input and,
	 * if A succeeds, matching B against the following part of the input.
	 *
	 * NOTE: The symbol "+~" was chosen to avoid confusion with String.+ in the presence of implicit conversions
	 * from strings to pattern. "&" was also tried, but its ranking in the operator precedence hierarchy made
	 * for some unexpected associations in patterns. */
	def +~(other:Matcher) = new BinopMatcher(this, "", other)

	/**`A +~~? B` is the same as `A +~ (Chars.Whitespace *> 0) +~ B`. In other words, `A` followed by `B`,
	 * optionally separated by whitespace.
	 */
	def +~~?(other: Matcher) = this +~ (Chars.Whitespace *> 0) +~ other

	/**`A +~~ B` is the same as `A +~ (Chars.Whitespace *> 1) +~ B`. In other words, `A` followed by `B`,
	 * separated by at least some whitespace.
	 */
	def +~~(other: Matcher) = this +~ (Chars.Whitespace *> 1) +~ other

	/** `A | B` matches if either A matches against the input, or if B matches against the input. */
	def |(other: Matcher) = new BinopMatcher(this, "|", other)

	/** `A*>n` matches "greedily" by first matching `A` against the input at least n times, and
	 * then as many more times as possible while still allowing the overall match to succeed. Most commonly, this is used as `A*>0` or `A*>1`*/
	def *>(count: Int) = new RepMatcher(this, "{" + count + ",}")

	/** `A*>n` matches "non-greedily" by first matching `A` against the input at least n times, and
	 * then only as many more times as necessary for the overall match to succeed. Most commonly, this is used as `A*<0` or `A*<1`*/
	def *<(count: Int) = new RepMatcher(this, "{" + count + ",}?")

	/** `A*!n` matches by first matching A against the input at least n times, and
	 * then as many more times as possible, using a possessive (non-backtracking) approach.
	 * 
	 * When a substring is matched possessively, backtracking into that substring to further
	 * the match as later processing is done is not allowed; once the substring has been matched
	 * possessively, it cannot be "unmatched" to match with other parts of the pattern. Depending on the regular
	 * expression, this can significantly speed things up. It can also alter the behavior of patterns
	 * in difficult-to-predict ways, so use with caution. */
	def *!(count:Int) = new RepMatcher(this, "{" + count + ",}+")

	/** `A*>(m, n)` greedily matches at least m and at most n instances of `A`, where m and n are
	 * the two elements of the `count` argument.
	 *
	 * See `*>(count: Int)` for details on the meaning of "greedy".
	 *
	 * @param count A 2-tuple of integers, defining a minimum and maximum number of repetitions
	 * of the pattern specified by `this`. */
	def *>(count: (Int,Int)) = new RepMatcher(this, "{" + count._1 + "," + count._2 + "}")

	/** `A*<(m, n)` non-greedily matches at least m and at most n instances of `A`, where m and n are
	 * the two elements of the `count` argument.
	 *
	 * See `*<(count: Int)` for details on the meaning of "non-greedy".
	 *
	 * @param count A 2-tuple of integers, defining a minimum and maximum number of repetitions
	 * of the pattern specified by `this`. */

 	def *<(count: (Int,Int)) = new RepMatcher(this, "{" + count._1 + "," + count._2 + "}?")

	/** `A*!(m, n)` possessively matches at least m and at most n instances of `A`, where m and n are
	 * the two elements of the `count` argument.
	 *
	 * See `*!(count: Int)` for details on the meaning of "possessive".
	 *
	 * @param count A 2-tuple of integers, defining a minimum and maximum number of repetitions
	 * of the pattern specified by `this`. */
	def *!(count: (Int,Int)) = new RepMatcher(this, "{" + count._1 + "," + count._2 + "}+")

	/** Lookahead operator: `A.>>` matches if `A` matches starting at the current match position; however, even if this
	 * match succeeds, the current match position is NOT altered.
	 *
	 * In addition, portions of the input matched with a lookahead match are NOT included in MatchResults (unless they
	 * are also matched by a "normal" matching operator.)
	 */
	def >> = new AnonGroup("=", this)

	/** Negated lookahead operator: `A.!>>` matches if `A` does NOT match starting at the current match position; however, even if this
	 * match succeeds, the current match position is NOT altered.
	 *
	 * In addition, portions of the input matched with a negated lookahead match are NOT included in MatchResults (unless they
	 * are also matched by a "normal" matching operator.)
	 */
	def !>> = new AnonGroup("!", this)

	/** Lookback operator: `A.<<` matches if `A` matches immediately before the current match position; however, even if this
	 * match succeeds, the current match position is NOT altered.
	 *
	 * In addition, portions of the input matched with a lookback match are NOT included in MatchResults (unless they
	 * are also matched by a "normal" matching operator.)
	 */
	def << = new AnonGroup("<=", this)

	/** Negated lookback operator: `A.!<<` matches if `A` does NOT match immediately before the current match position; however, even if this
	 * match succeeds, the current match position is NOT altered.
	 *
	 * In addition, portions of the input matched with a negated lookback match are NOT included in MatchResults (unless they
	 * are also matched by a "normal" matching operator.)
	 */
	def !<< = new AnonGroup("<!", this)

	/** Makes {{code this}} optional in the match; it will match if it can, but if it cannot, the overall match
	 * of which it is a part can still succeed.
	 */
	def ? = this *> (0,1)

	/** Make this pattern into a named group; the contents of the group,
	 * from a match, will be retrieved by its name.
	 *
	 * @param name The name by which the contents matching the name may be extracted
	 * following a successful match. See the documentation for `MatchResult` for details
	 * on how to extract named groups.
	 */
	def name(groupName: String) = {
		if (Matcher.publicGroupNamePat !~~= groupName) {
			throw new NameException("Attempt to name group with illegal name: '%s'" format groupName)
		}
		new GroupMatcher(this, groupName)
	}

	///////////////////////////////////////////////////////////
	// Flags
	///////////////////////////////////////////////////////////
	/** Makes `this` case-insensitive over the ASCII character set. */
	def ASCIICaseInsensitive = new AnonGroup("i:", this)

	/** Makes `this` case-sensitive over the ASCII character set. (This is the default.) */
	def ASCIICaseSensitive = new AnonGroup("-i:", this)

	/** Makes `this` case-insensitive over all Unicode characters. */
	def UnicodeCaseInsensitive = new AnonGroup("iu:", this)

	/** Makes `this` case-sensitive over allUnicode characters. (This is the default.) */
	def UnicodeCaseSensitive = new AnonGroup("-iu:", this)

	///////////////////////////////////////////////////////////
	// Pattern string comparison operators
	///////////////////////////////////////////////////////////
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
	 * precedence, which works well in practice.*/
	def ~=(target: String): Boolean = {
		regex.findFirstIn(target) match {
			case None => false
			case Some(m) => true
		}
	}

	/** Negation of ~=. Note that by
	 * the rules of Scala precedence, the trailing = gives this operator the lowest possible
	 * precedence, which works well in practice.*/
	def !~=(target: String) = !(this ~= target)

	/** Negation of ~~=. Note that by
	 * the rules of Scala precedence, the trailing = gives this operator the lowest possible
	 * precedence, which works well in practice.*/
	def !~~=(target: String) = !(this ~~= target)

	///////////////////////////////////////////////////////////
	// Text processing operations.
	///////////////////////////////////////////////////////////
	/** Find and return the first match in `target` matching `this`. */
	def findFirstIn(target: String): Option[MatchResult] = {
		regex.findFirstMatchIn(target) match {
			case None => None
			case Some(m) => Some(new MatchResult(true, m, null, this))
		}
	}

	/** Wherever `this` matches within `target`, replace the
	 *	matched subsequence with `replacement`.
	 *
	 *	@param target The string in which replacements will be done.
	 *
	 *	@param replacement The string that will be substituted for whatever matched `this`.
	 */
	def replaceAllIn(target:String, replacement:String) = regex.replaceAllIn(target, replacement)

	/** Experimental feature, not suggested for production use. */
	def apply(target: String) = new Matching(this, target)

	/** This returns an iterator that iterates over both successful and failed
	 * matches. (A failed match is the text between two successful matches.) In this
	 * way, you have access to all the text in the target string.
	 *
	 * To see how to access results look at `MatchResult`.
	 */
	def findAllIn(target: String) = new MatchResultIterator(target, regex.findAllIn(target).matchData, this)
}
