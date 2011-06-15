package com.digitaldoodles.rex

import patterns.Word
import scala.util.matching.Regex
import com.digitaldoodles.rex.Matcher._
import java.lang.IllegalAccessError
import collection.mutable.ArrayBuffer

private[rex] object Implicits {
	implicit def stringToLit(s:String): Matcher = Lit(s)
}

private object Matcher {
	implicit def stringToLit(s:String) = Lit(s)

	/** Add a backquote immediately before any characters that have a special
	 * meaning in character classes. */
	private[rex] def backQuoteCharClassSpecials(s:String):String =
		(for (c <- s) yield (if ("[]-^&\\.{}".indexOf(c) >= 0) "\\" + c else c)).mkString("")

	/** Add a backquote immediately before any characters that have a special meaning
	 * in regular expressions outside of character classes. */
	private[rex] def backQuoteLiteralSpecials(s:String):String =
		(for (c <- s) yield (if ("[]^$\\.{,}|+*<".indexOf(c) >= 0) "\\" + c else c)).mkString("")

}

/** Matcher is the root class for all other classes that implement regular expression matching.
 *
 * @param regex The regular expression that will be stored internally by the Matcher instance, and used by
 * [[scala.util.matching.Regex]] to perform the actual pattern matches.
 */
private[rex] class Matcher(regexPattern: String) {
	val regex = new Regex(regexPattern)
	/** Returns the Scala regex for this Matcher--supported to be in accord with "stringpattern".r */
	def r = regex
	private[rex] def nameToGroupNumber = Map[String, Int]()
	private[rex] def groupCount = 0
	protected val grouped = false

	/** Encloses `this` in an anonymous name--one that cannot
		 be accessed and does not contribute to the numbering of groups. Does nothing if `this`
		 is already in a name of some sort. */
	private[rex] def anonGroup = {
		if (!this.grouped) "(?:" + this.pattern + ")"
		else this.pattern
	}

	/** Returns the regular expression pattern corresponding to this rex entity. */
	def pattern = regex.pattern.pattern()

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
	def *>(count:Int) = new Matcher(anonGroup + "{" + count + ",}")

	/** `A*<n` matches at least n instances of A, and then as few more as possible. */
	def *<(count:Int) = new Matcher(anonGroup + "{" + count + ",}?")

	/** Matches `count` or more instances of `this`, possessively.
		 When a substring is matched possessively, backtracking into that substring to further
		 the match as later processing is done is not allowed; the substring must either be accepted
		 in its entirety within the possessive match, or rejected entirely. Depending on the regular
		 expression, this can significantly speed things up. It can also alter the behavior of patterns
		 in difficult-to-predict ways, so use with caution. */
	def *!(count:Int) = new Matcher(anonGroup + "{" + count + ",}+")

	/** Greedily matches at least m and at most n instances of `this`, where m and n are
		 the two elements of the `count` argument.

		 @param count A 2-tuple of integers, defining a minimum and maximum number of repetitions
		 of the pattern specified by `this`. */
	def *>(count: (Int,Int)) = new Matcher(anonGroup + "{" + count._1 + "," + count._2 + "}")

	/** Non-greedily matches at least m and at most n instances of this, where m and n are
		 the two elements of the `count` argument.

		 @param count A 2-tuple of integers, defining a minimum and maximum number of repetitions
		 of the pattern specified by `this`. */
	def *<(count: (Int,Int)) = new Matcher(anonGroup + "{" + count._1 + "," + count._2 + "}?")

	/** Possessively matches at least m and at most n instances of this, where m and n are
		 the two elements of the `count` argument.

		 @param count A 2-tuple of integers, defining a minimum and maximum number of repetitions
		 of the pattern specified by `this`. */
	def *!(count: (Int,Int)) = new Matcher(anonGroup + "{" + count._1 + "," + count._2 + "}+")

		def optional = this *> (0,1)
		def ? = optional

	/** Make this pattern into a named name; the contents of the name,
		 from a match, will be retrieved by its name, not by a number.

		 @param name The name by which the contents matching the name may be extracted
		 following a successful match. See the documentation for `MatchResult` for details
		 on how to extract named groups.
	 */
	def name(groupName:String) = new GroupMatcher(this, groupName)


	/** Wherever `this` matches within `target`, replace the
		 matched subsequence with `target`.

		 @param target The string in which replacements will be done.

	@param replacement The string that will be substituted for whatever matched `this`.
	 */
	def replaceAllIn(target:String, replacement:String) = regex.replaceAllIn(target, replacement)

	/** Returns true if `this` exactly matches `target`, false otherwise. Note that by
	 * the rules of Scala precedence, the trailing = gives this operator the lowest possible
	 * precedence, which works will in practice. */
	def ~~=(target:String):Boolean = {
		regex.findFirstIn(target) match {
			case None => false
			case Some(m) => m == target
		}
	}

	/** Returns true if `this` matches somewhere in `target`, false otherwise. Note that by
	 * the rules of Scala precedence, the trailing = gives this operator the lowest possible
	 * precedence, which works will in practice.*/
	def ~=(target:String):Boolean = {
		regex.findFirstIn(target) match {
			case None => false
			case Some(m) => true
		}
	}

	/** Negation of ~=. Note that by
	 * the rules of Scala precedence, the trailing = gives this operator the lowest possible
	 * precedence, which works will in practice.*/
	def !~=(target:String) = !(this ~= target)

	/** Negation of ~~=. Note that by
	 * the rules of Scala precedence, the trailing = gives this operator the lowest possible
	 * precedence, which works will in practice.*/
	def !~~=(target:String) = !(this ~~= target)

	/** Find and return the first substring in `target` matching `this`. */
	def findFirst(target:String) : Option[MatchResult] = {
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
	 * NOTE: It is usually clearer to use the operator `A ==> B`.
		 @return A rex pattern which succeeds only if `this` matches
		 the next piece of the string being matched against; does
		 not consume the next piece of the string.
	 */
	def lookahead = new AnonGroup("=" + this.pattern + ")")
	def >> = lookahead

	/*** Returns a negated lookahead pattern match.
	 * NOTE: It is usually clearer to use the operator `A !==> B`.

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

final case class MatchingResult(input:String, start: Int, end: Int)

final class Tokenizer[T](default: (MatchResult=>T), alternatives: (Matcher, MatchResult=>T)*) {
	var tokenizer: Matcher = alternatives(0)._1.name("alt0")
	var processors = scala.collection.mutable.HashMap("alt0" -> alternatives(0)._2)
	for (i <- 1 until alternatives.length) {
		tokenizer = tokenizer | alternatives(i)._1.name("alt" + i)
		processors += ("alt"+i) -> alternatives(i)._2
	}

	private def process(mr: MatchResult): T = {
		if (!mr.matched) default(mr)
		else {
			for (n <- processors.keys) {
				if (mr.group(n) != null) {
					return processors(n)(mr)
				}
			}
			throw new IllegalAccessError("Could not find a name associated with a tokenized substring. This indicates a bug in com.digitaldoodles.rex")
		}
	}

	def tokenize(input: String): Seq[T] = new Matching(tokenizer, input).iterator.toSeq.map(process(_))
}

/**This class represents an instance of a regular expression (Matcher) associated with
 * an input string. It is intended for "heavy-duty" usage, for example when you have a Matcher
 * and want to do various things with it using the same input string.
 */
final class Matching(val matcher: Matcher, val input: String) extends Iterable[MatchResult] {
	val jMatcher = matcher.regex.pattern.matcher(input)
	jMatcher.reset

	def matchPrefix(from: Int): Option[MatchingResult] = {
		jMatcher.region(from, input.length)
		if (!jMatcher.lookingAt() == true) Some(MatchingResult(input, jMatcher.start, jMatcher.end))
		else None
	}
	
	/**Allows iterating over the substrings the input was broken into by the regex. For
	 * example: for (s <- Charset("abc")("an input string")) { ... }
	 */
	def iterator = new MatchResultIterator(input, matcher.regex.findAllIn(input).matchData, matcher)

	/**Returns a new string with any matching substrings in the input string replaced by
	 * `replacement`.
	 */
	def replaceWith(replacement: String) = matcher.regex.replaceAllIn(input, replacement)

	def replaceWith(matchFun: MatchResult => String, nonmatchedFun: String => String = ((s: String) => s)) = {
		val result = new StringBuilder
		for (m <- this) {
			if (m.matched) result append matchFun(m)
			else result append nonmatchedFun(m.string)
		}
		result.toString
	}
}

/** Define a literal pattern. This pattern is quoted internally, so no regular expression
 characters have special meanings in it. Of course, string special characters still have
 their special meanings in the string argument, if you use single-quoted strings.
 */
case class Lit(lit: String) extends Matcher(Matcher.backQuoteLiteralSpecials(lit))

/** Represents an instance of an anonymous (unnumbered, unnamed) name. */
private[rex] class AnonGroup(pattern:String) extends Matcher("(?" + pattern) {
	override val grouped = true
}

/** Used in constructing binary rex operations. */
private[rex] class BinopMatcher(val m1:Matcher, op:String, val m2:Matcher) extends Matcher(m1.anonGroup + op + m2.anonGroup) {

	/** Number of non-anonymous (counting) groups in this regular expression. */
	override def groupCount = m1.groupCount + m2.groupCount

	/** Given a name name, return its corresponding name number.

		 @see Matcher.name(name)
	 */
	override def nameToGroupNumber = m1.nameToGroupNumber ++
			m2.nameToGroupNumber.map(x => (x._1, x._2 + m1.groupCount))
}

/** Responsible for numbering of named groups.*/
private[rex] class GroupMatcher(pat:Matcher, val name: String) extends Matcher("("+pat.pattern+")") {

	/** Creates a new map of name names to left parenthesis numbers by mapping over the previously existing
				map. */
	override val nameToGroupNumber:Map[String, Int] = Map(name -> 1) ++ pat.nameToGroupNumber.map(x => (x._1, x._2 + 1))

	/** If a pattern is already grouped, we can sometimes avoid surrounding it with the "(?:...)" pattern. */
	override val grouped = true

	override def groupCount = 1 + pat.groupCount
}






