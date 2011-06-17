package com.digitaldoodles.rex

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 5/18/11, 11:16 AM
 * License: LGPL
 */

/** A final char class is typically of form [...&&[...]] or [...&&[^...]], and
 once formed, cannot be further combined with any other char classes. */
final private[rex] case class FinalCharClass(set1: RawCharClass, set2: RawCharClass) extends
	Matcher({
		val sign1 = if (set1.negated) "^" else ""
		val sign2 = if (set2.negated) "^" else ""
		val clause2 = "&&[" + sign2 + set2.characters + "]"
		"[" + sign1 + set1.characters + clause2 + "]"
	}) {

	private[rex] val lowestPrecedenceInPattern = 0

	override def anonGroup = this.pattern
}

/** Defines a character class that matches any of the characters in the provided string.

 The string is quoted internally, so no characters have special meanings to the regex engine. (However, you
 may need to backquote-escape characters that have special meanings in strings.

 */
case class CharSet(set: String) extends RawCharClass(Matcher.backQuoteCharClassSpecials(set))

/** Defines a character class that matches a range of characters. Characters with special meanings are backquoted
 internally, so you do not need to backquote them */
case class CharRange(start:Char, end:Char) extends
	RawCharClass(Matcher.backQuoteCharClassSpecials(start.toString) +
		"-" + Matcher.backQuoteCharClassSpecials(end.toString)
	)



