package com.digitaldoodles.rex

/**
 * Author(s): Kenneth Murray McDonald
 * Created: 6/14/11, 10:27 PM
 * License: LGPL
 */

/** A RawCharClass does not perform escaping on the string passed to it.
 * When used as a pattern, it simply wraps the string in "[]" */
private[rex] class RawCharClass(val characters: String, val negated: Boolean = false) extends Matcher {

	private[rex] def buildPattern(nameMap: Map[String, Int]) = "[" + (if (negated) "^" else "") + characters + "]"

	private[rex] val lowestPrecedenceInPattern = 0

	def unary_! = new RawCharClass(characters, !negated)

	override def anonGroup(nameMap: Map[String, Int]) = this.pattern

	/** Char class subtraction; things in this but not in 'notin' */
	def -(notin: RawCharClass): FinalCharClass = this /\  !notin
	def -(notin: String): FinalCharClass = this - CharSet(notin)

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
