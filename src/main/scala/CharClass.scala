package com.digitaldoodles.rex

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 5/18/11, 11:16 AM
 * License: LGPL
 */

/** A final char class is typically of form [...&&[...]] or [...&&[^...]], and
 once formed, cannot be further combined with any other char classes. */
final private[rex] case class FinalCharClass(set1: RawCharClass, set2: RawCharClass) extends Matcher {

	private[rex] def buildPattern(nameMap: Map[String, Int]) = {
		val sign1 = if (set1.negated) "^" else ""
		val sign2 = if (set2.negated) "^" else ""
		val clause2 = "&&[" + sign2 + set2.characters + "]"
		"[" + sign1 + set1.characters + clause2 + "]"
	}

	private[rex] val lowestPrecedenceInPattern = 0

	override def anonGroup(nameMap: Map[String, Int]) = this.pattern
}

/** Defines a character class that matches any of the characters in the provided string.
 *
 * Characters with special meanings to the regex engine are backquoted
 * internally, so do not need to be backquoted when calling this constructor. (However, it may
 * be necessary to backquote characters that have special meanings in strings).
 */
case class CharSet(set: String) extends RawCharClass(Matcher.backQuoteCharClassSpecials(set))

/** Defines a character class that matches a range of characters from `start` to `end` inclusive.
 * Characters with special meanings to the regex engine are backquoted
 * internally, so do not need to be backquoted when calling this constructor. */
case class CharRange(start: Char, end: Char) extends
	RawCharClass(Matcher.backQuoteCharClassSpecials(start.toString) +
		"-" + Matcher.backQuoteCharClassSpecials(end.toString)
	)



