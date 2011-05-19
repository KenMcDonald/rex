package com.digitaldoodles.rex

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 5/18/11, 11:16 AM
 * License: LGPL
 */

/** A final char class is typically of form [...&&[...]] or [...&&[^...]], and
 once formed, cannot be further combined with any other char classes. */
final protected case class FinalCharClass(set1:String, negated:Boolean, set2:String) extends
	Matcher(Matcher.assembleCharClass(set1, negated, set2)) {

	override val grouped = true
	override def anonGroup = this.pattern

	def this(set1:String) = this(set1, false, "")
}

/** Defines a character class that matches any of the characters in the provided string.
 The string is quoted internally, so no characters have special meanings to the regex engine. (However, you
 may need to backquote-escape characters that have special meanings in strings. */
case class CharSet(set:String) extends RawCharClass(Matcher.backQuoteCharClassSpecials(set)) {
	override val grouped = true
}

/** Defines a character class that matches a range of characters. Characters with special meanings are backquoted
 internally, so you do not need to backquote them */
case class CharRange(start:Char, end:Char) extends
	RawCharClass(Matcher.backQuoteCharClassSpecials(start.toString) +
		"-" + Matcher.backQuoteCharClassSpecials(end.toString)
	)

/** A RawCharClass does not perform escaping on the string passed to it.
 When used as a pattern, it simply wraps the string in "[]" */
protected class RawCharClass(val chars:String) extends Matcher("[" + chars + "]") {

	override def anonGroup = this.pattern

	/** Char class subtraction; things in this but not in 'notin' */
	def -(notin:RawCharClass):FinalCharClass = FinalCharClass(this.chars, true, notin.chars)
	def -(notin:String):FinalCharClass = this - CharSet(notin)

	/** Char class intersection.
	 *
	 * This produces a char class that matches
	 * chars that are in the intersection of 'this' and 'alsoIn'. For reasons
	 * too complex to go into, once this operation has been used to produce a char
	 * class, that char class cannot be used in other char class operations (but
	 * it can of course be used in normal rex operations.) */
	def /\(alsoIn: RawCharClass) = FinalCharClass(this.chars, false, alsoIn.chars)

	/** Char class union.
	 *
	 * Use this to get a char class that has "special" character symbols in it,
	 * by taking unions with `Char...` patterns from ykken.rex.patterns.
	 *
	 * @example CharSet("abc") extend CharSpace */
	@deprecated("Instead use one of the predefined methods that can do the same thing, only more concisely " +
			"and with better code assist: eg. 'CharSet(\"nesw\").digit.punctuation' to obtain a character class " +
			"that will match any of 'nesw', or a digit, or a punctuation character.")
 	private def \/(orIn: RawCharClass) = extend(orIn)

	private def extend(orIn: RawCharClass) = new RawCharClass(this.chars + orIn.chars)

	/** Produces a new char class which matches anything `this` matches, or any alphanumeric character. */
	def alphanumeric = this extend patterns.CharAlnum
	/** Produces a new char class which matches anything `this` matches, or any alphabetic character. */
	def alphabetic = this extend patterns.CharAlpha
	/** Produces a new char class which matches anything `this` matches, or any ASCII character. */
	def ascii = this extend patterns.CharASCII
	/** Produces a new char class which matches anything `this` matches, or any digit. */
	def digit = this extend patterns.CharDigit
	/** Produces a new char class which matches anything `this` matches, or any non-digit character. */
	def nondigit = this extend patterns.CharNonDigit

	/** Produces a new char class which matches anything `this` matches, or any blank character. */
	def blank = this extend patterns.CharBlank
	/** Produces a new char class which matches anything `this` matches, or any whitespace character. */
	def whitespace = this extend patterns.CharWhitespace
	/** Produces a new char class which matches anything `this` matches, or any non-whitespace character. */
	def nonwhitespace = this extend patterns.CharNonWhitespace
	/** Produces a new char class which matches anything `this` matches, or any space character. */
	def space = this extend patterns.CharSpace
	/** Produces a new char class which matches anything `this` matches, or any punctuation character. */
	def punctuation = this extend patterns.CharPunct

	/** Produces a new char class which matches anything `this` matches, or any lowercase character. */
	def lower = this extend patterns.CharLower
	/** Produces a new char class which matches anything `this` matches, or any uppercase character. */
	def upper = this extend patterns.CharUpper

	/** Produces a new char class which matches anything `this` matches, or any control character. */
	def control = this extend patterns.CharCntrl
	/** Produces a new char class which matches anything `this` matches, or any 'graphical' character. */
	def graphical = this extend patterns.CharGraph
	/** Produces a new char class which matches anything `this` matches, or any printable character. */
	def printable = this extend patterns.CharPrint

	/** Produces a new char class which matches anything `this` matches, or any character which is not a 'word' character. */
	def word = this extend patterns.CharWord
	/** Produces a new char class which matches anything `this` matches, or any character which is not a 'word' character. */
	def nonword = this extend patterns.CharNonWord
	/** Produces a new char class which matches anything `this` matches, or any hexadecimal digit. */
	def hex = this extend patterns.CharXDigit

}



