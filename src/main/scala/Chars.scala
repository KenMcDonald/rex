package com.digitaldoodles.rex

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 6/14/11, 10:26 PM
 * License: LGPL
 */

object Chars {
	/** Matches any single uppercase character. */
	case object Uppercase extends RawCharClass("\\p{Upper}")

	/** Matches any single lowercase character. */
	case object Lowercase extends RawCharClass("\\p{Lower}")

	/** Matches any single ASCII character. */
	case object ASCII extends RawCharClass("\\p{ASCII}")

	/** Matches any single alphabetic character. */
	case object Alphabetic extends RawCharClass("\\p{Alpha}")

	/** Matches any single digit. */
	case object Digit extends RawCharClass("\\p{Digit}")

	/** Matches any single alphanumeric character. */
	case object Alphanumeric extends RawCharClass("\\p{Alnum}")

	/** Matches any single punctuation character. */
	case object Punctuation extends RawCharClass("\\p{Punct}")

	/** Matches any single graphical character. */
	case object Graphical extends RawCharClass("\\p{Graph}")

	/** Matches any single printing character. */
	case object Printable extends RawCharClass("\\p{Print}")

	/** Matches any single blank character. */
	case object Blank extends RawCharClass("\\p{Blank}")

	/** Matches any single control character. */
	case object Control extends RawCharClass("\\p{Cntrl}")

	/** Matches any single hexadecimal character. */
	case object Hex extends RawCharClass("\\p{XDigit}")

	/** Matches any single space character. */
	case object Space extends RawCharClass("\\p{Space}")

	/** Matches any single nondigit character. */
	case object NonDigit extends RawCharClass("\\D")

	/** Matches any single whitespace character. */
	case object Whitespace extends RawCharClass("\\s")

	/** Matches any single non-whitespace character. */
	case object NonWhitespace extends RawCharClass("\\S")

	/** Matches any single word character. */
	case object Word extends RawCharClass("\\w")

	/** Matches any single non-word character. */
	case object NonWord extends RawCharClass("\\W")

	/** Matches any character, including newlines. */
	case object Any extends RawCharClass("\\s\\S")
}
