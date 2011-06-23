package com.digitaldoodles.rex.patterns

import com.digitaldoodles.rex._
//import org.scalatest.matchers.Matcher
import com.digitaldoodles.rex.Implicits._

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-01-19, 3:12 PM
 * License: LGPL
 */

private[rex] class SpecialChar(pattern: String) extends Matcher {

	private[rex] def buildPattern(nameMap: Map[String, Int]) = pattern

	private[rex] val lowestPrecedenceInPattern = 0
}

/** Patterns having to do with lines. */
object Line {
	/** Matches the start of a line. */
	object Start extends SpecialChar("^")

	/** Matches the end of a line. */
	object End extends SpecialChar("$")

	/** Matches an entire line. */
	val Entire = Start +~ Chars.Any*<0 +~ End
}

/** Patterns related to words */
object Word {
	/** Matches a word boundary. */
	object Boundary extends SpecialChar("\\b")

	/** Matches a nonword boundary, i.e. a position that is entirely within a word or
	 *entirely outside a word. */
	object NonBoundary extends SpecialChar("\\B")

	/** Matches a "word" character; this is just an alias for Chars.Word */
	val Character = Chars.Word

	/** Matches at the start of a word, does not consume input. */
	val Start = Boundary +~ Character.>>

	/** Matches at the end of a word, does not consume input. */
	val End = Character.<< +~ Boundary

	/** Matches an entire word. This will never match "empty" words. */
	val Entire = Start +~ Character*<1 +~ End

}

object Input {
	/** Matches the start of an input string. */
	object Start extends SpecialChar("\\A")

	/** Matches the end of an input string. */
	object End extends SpecialChar("\\z")
	/** Matches the end of a line, except the  trailing newline will not be included. */

	object BndryPreviousMatchEnd extends SpecialChar("\\G")
	object BndryStringEndExceptTerminator extends SpecialChar("\\Z")
}

object Number {
	/** Greedily match as many digits as possible, at least one. */
	val UnsignedInt = Chars.Digit*>1

	/** Greedily match as many digits as possible (at least one), optionally preceded by a "+" or "-" */
	val SignedInt = ("+"|"-").? +~ UnsignedInt

	/** Matches a series of digits followed by an optional period and 1 or more further digits.
	 * Note that numbers of the form "123." (i.e. no digits after decimal point) are not matched. */
	val UnsignedFloat = UnsignedInt +~ ("." +~ UnsignedInt).?

	/** Like `UnsignedFloat`, but allows an optional "-" or "+" at the start of the match. */
	val SignedFloat = ("+"|"-").? +~ UnsignedFloat

	/** Value in scientific notation, eg. -1e-3 or 3.14E2 */
	val Scientific = SignedFloat +~ (CharSet("eE") +~ SignedInt).?

	/** A single digit, simply an alias for Chars.Digit */
	val Digit = Chars.Digit

	/** Any character that is not a digit, simply an alias for Chars.NonDigit. */
	val NonDigit = Chars.NonDigit

	/** Matches at the start of a sequence of one or more digits, does not consume input. */
	val Start = NonDigit.<< +~ Digit.>>

	/** Matches at the end of one or more digits, does not consume input. */
	val End = Digit.<< +~ NonDigit.>>

	/**An entire sequence of digits; this differs from UnsignedInt in that Entire is bounded by
	 * non-digits, while a match of UnsignedInt could be preceded by digits.
	 */
	val Entire = Start +~ UnsignedInt +~ End
}

/** The regex "." pattern. */
object Dot extends SpecialChar(".")