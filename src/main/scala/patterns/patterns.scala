package com.digitaldoodles.rex.patterns

import com.digitaldoodles.rex._

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-01-19, 3:12 PM
 * License: LGPL
 */

/** Patterns having to do with lines. */
object Line {
	/** Matches the start of a line. */
	case object Start extends Matcher("^")

	/** Matches the end of a line. */
	case object End extends Matcher("$")

	/** Matches an entire line. */
	val Entire = Start +~ Chars.Any*<0 +~ End
}

/** Patterns related to words */
object Word {
	/** Matches a word boundary. */
	case object Boundary extends Matcher("\\b")

	/** Matches a nonword boundary, i.e. a position that is entirely within a word or
	 *entirely outside a word. */
	case object NonBoundary extends Matcher("\\B")

	/** Matches a "word" character; this is just an alias for Chars.Word */
	val Character = Chars.Word

	/** Matches at the start of a word, does not consume input. */
	val Start = Boundary +~ Character.>>

	/** Matches at the end of a word, does not consume input. */
	val End = Boundary.<< +~ Character

	/** Matches an entire word. This will never match "empty" words. */
	val Entire = Start +~ Character*<1 +~ End

}

object Input {
	/** Matches the start of an input string. */
	case object Start extends Matcher("\\A")

	/** Matches the end of an input string. */
	case object End extends Matcher("\\z")
	/** Matches the end of a line, except the  trailing newline will not be included. */

	case object BndryPreviousMatchEnd extends Matcher("\\G")
	case object BndryStringEndExceptTerminator extends Matcher("\\Z")
}

object Number {
	/** Greedily match as many digits as possible, at least one. */
	case object UnsignedInt extends Matcher("[0-9]+")

	/** Greedily match as many digits as possible (at least one), optionally preceded by a "+" or "-" */
	case object SignedInt extends Matcher("(?:\\+|-)?[0-9]+")

	/** Matches a series of digits followed by an optional period and 1 or more further digits.
	 * Note that numbers of the form "123." (i.e. no digits after decimal point) are not matched. */
	case object UnsignedFloat extends Matcher("[0-9]+(?:.[0-9]+)?")

	/** Like `UnsignedFloat`, but allows an optional "-" or "+" at the start of the match. */
	case object SignedFloat extends Matcher("(?:\\+|-)?[0-9]+(?:.[0-9]+)?")

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
case object Dot extends Matcher(".")

