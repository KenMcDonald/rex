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

	override val grouped = true
	override def anonGroup = this.pattern
}

/** Defines a character class that matches any of the characters in the provided string.

 The string is quoted internally, so no characters have special meanings to the regex engine. (However, you
 may need to backquote-escape characters that have special meanings in strings.

 */
case class CharSet(set: String) extends RawCharClass(Matcher.backQuoteCharClassSpecials(set)) {
	override val grouped = true
}

/** Defines a character class that matches a range of characters. Characters with special meanings are backquoted
 internally, so you do not need to backquote them */
case class CharRange(start:Char, end:Char) extends
	RawCharClass(Matcher.backQuoteCharClassSpecials(start.toString) +
		"-" + Matcher.backQuoteCharClassSpecials(end.toString)
	)

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
/** A RawCharClass does not perform escaping on the string passed to it.
 When used as a pattern, it simply wraps the string in "[]" */
private[rex] class RawCharClass(val characters: String, val negated: Boolean = false) extends
	Matcher("[" + (if (negated) "^" else "") + characters + "]") {

	def unary_! = new RawCharClass(characters, !negated)

	override def anonGroup = this.pattern

	/** Char class subtraction; things in this but not in 'notin' */
	def -(notin:RawCharClass):FinalCharClass = this /\  !notin
	def -(notin:String):FinalCharClass = this - CharSet(notin)

	/** Char class intersection.
	 *
	 * This produces a char class that matches
	 * characters that are in the intersection of 'this' and 'alsoIn'. For reasons
	 * too complex to go into, once this operation has been used to produce a char
	 * class, that char class cannot be used in other char class operations (but
	 * it can of course be used in normal rex operations.) */
	def /\(alsoIn: RawCharClass) = FinalCharClass(this, alsoIn)

	/** Char class union.
	 *
	 * Use this to get a char class that has "special" character symbols in it.
	 *
	 * @example CharSet("abc") extend CharSpace */
 	private def \/(orIn: RawCharClass) = extend(orIn)

	private def extend(orIn: RawCharClass) = new RawCharClass(this.characters + orIn.characters, negated)

	/**Produces a new char class which matches anything `this` matches, or anything in the range from the `start` char
	 * to the `end` char, inclusive
	 */
	def charRange(start: Char, end: Char) = this extend new RawCharClass(Matcher.backQuoteCharClassSpecials(start.toString) +
		"-" + Matcher.backQuoteCharClassSpecials(end.toString), negated)

	/** Produces a new char class which matches anything `this` matches, or anything in the string `set`. */
	def charSet(set: String) = this extend new RawCharClass(Matcher.backQuoteCharClassSpecials(set), negated)

	/** Produces a new char class which matches anything `this` matches, or any alphanumeric character. */
	def Alphanumeric = this extend Chars.Alphanumeric

	/** Produces a new char class which matches anything `this` matches, or any alphabetic character. */
	def Alphabetic = this extend Chars.Alphabetic

	/** Produces a new char class which matches anything `this` matches, or any ASCII character. */
	def ASCII = this extend Chars.ASCII

	/** Produces a new char class which matches anything `this` matches, or any digit. */
	def Digit = this extend Chars.Digit

	/** Produces a new char class which matches anything `this` matches, or any non-digit character. */
	def NonDigit = this extend Chars.NonDigit

	/** Produces a new char class which matches anything `this` matches, or any blank character. */
	def Blank = this extend Chars.Blank

	/** Produces a new char class which matches anything `this` matches, or any whitespace character. */
	def Whitespace = this extend Chars.Whitespace

	/** Produces a new char class which matches anything `this` matches, or any non-whitespace character. */
	def NonWhitespace = this extend Chars.NonWhitespace

	/** Produces a new char class which matches anything `this` matches, or any space character. */
	def Space = this extend Chars.Space

	/** Produces a new char class which matches anything `this` matches, or any punctuation character. */
	def Punctuation = this extend Chars.Punctuation

	/** Produces a new char class which matches anything `this` matches, or any lowercase character. */
	def Lowercase = this extend Chars.Lowercase

	/** Produces a new char class which matches anything `this` matches, or any uppercase character. */
	def Uppercase = this extend Chars.Uppercase

	/** Produces a new char class which matches anything `this` matches, or any control character. */
	def Control = this extend Chars.Control

	/** Produces a new char class which matches anything `this` matches, or any 'graphical' character. */
	def Graphical = this extend Chars.Graphical

	/** Produces a new char class which matches anything `this` matches, or any printable character. */
	def Printable = this extend Chars.Printable

	/** Produces a new char class which matches anything `this` matches, or any character which is not a 'word' character. */
	def Word = this extend Chars.Word

	/** Produces a new char class which matches anything `this` matches, or any character which is not a 'word' character. */
	def NonWord = this extend Chars.NonWord

	/** Produces a new char class which matches anything `this` matches, or any hexadecimal digit. */
	def Hex = this extend Chars.Hex
}



