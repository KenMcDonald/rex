package com.digitaldoodles.rex.patterns


/**
 * Author(s): Kenneth Murray McDonald
 * Created: 2011-01-19, 3:12 PM
 * License: LGPL
 */

import com.digitaldoodles.rex.{Matcher, RawCharClass}

/** Matches the start of a line. */
final case object BndryLineStart extends Matcher("^")
/** Matches the end of a line. */
final case object BndryLineEnd extends Matcher("$")
/** Matches a word boundary. */
final case object BndryWord extends Matcher("\\b")
/** Matches a nonword boundary, i.e. a position that is entirely within a word or
 *entirely outside a word. */
final case object BndryNonWord extends Matcher("\\B")
/** Matches the start of a string. */
final case object BndryStringStart extends Matcher("\\A")
/** Matches the end of a line, except the a trailing newline will not be included. */
final case object BndryStringEndExceptTerminator extends Matcher("\\Z")
/** Matches the end of a line. */
final case object BndryStringEnd extends Matcher("\\z")
final case object BndryPreviousMatchEnd extends Matcher("\\G")

/** Matches any single uppercase character. */
final case object CharUpper extends RawCharClass("\\p{Upper}")
/** Matches any single lowercase character. */
final case object CharLower extends RawCharClass("\\p{Lower}")
/** Matches any single ASCII character. */
final case object CharASCII extends RawCharClass("\\p{ASCII}")
/** Matches any single alphabetic character. */
final case object CharAlpha extends RawCharClass("\\p{Alpha}")
/** Matches any single digit. */
final case object CharDigit extends RawCharClass("\\p{Digit}")
/** Matches any single alphanumeric character. */
final case object CharAlnum extends RawCharClass("\\p{Alnum}")
/** Matches any single punctuation character. */
final case object CharPunct extends RawCharClass("\\p{Punct}")
/** Matches any single graphical character. */
final case object CharGraph extends RawCharClass("\\p{Graph}")
/** Matches any single printing character. */
final case object CharPrint extends RawCharClass("\\p{Print}")
/** Matches any single blank character. */
final case object CharBlank extends RawCharClass("\\p{Blank}")
/** Matches any single control character. */
final case object CharCntrl extends RawCharClass("\\p{Cntrl}")
/** Matches any single hexadecimal character. */
final case object CharXDigit extends RawCharClass("\\p{XDigit}")
/** Matches any single space character. */
final case object CharSpace extends RawCharClass("\\p{Space}")

/** Matches any single nondigit character. */
final case object CharNonDigit extends RawCharClass("\\D")
/** Matches any single whitespace character. */
final case object CharWhitespace extends RawCharClass("\\s")
/** Matches any single non-whitespace character. */
final case object CharNonWhitespace extends RawCharClass("\\S")
/** Matches any single word character. */
final case object CharWord extends RawCharClass("\\w")
/** Matches any single non-word character. */
final case object CharNonWord extends RawCharClass("\\W")
/** Matches any character, including newlines. */
final case object CharAny extends RawCharClass("\\s\\S")
/** Consume as many characters as possible. */
final case object PatMaxChars extends Matcher("[\\s\\S]*")
/** Consume as few characters as possible */
final case object PatMinChars extends Matcher("[\\s\\S]*?")
/** Consume as much whitespace as possible (possibly none). */
final case object PatMaxWhitespace extends Matcher("[\\s]*")
/** Consume as much whitespace as possible, ensuring there is at least some whitespace. */
final case object PatSomeWhitespace extends Matcher("[\\s]+")
/** Greedily match as many digits as possible, at least one. */
final case object PatPosInt extends Matcher("[0-9]+")
/** Greedily match as many digits as possible (at least one), optionally preceded by a "+" or "-" */
final case object PatSignedInt extends Matcher("(?:\\+|-)?[0-9]+")
/** Matches a series of digits followed by an optional period and 0 or more further digits. */
final case object PatPosFloat extends Matcher("[0-9]+(?:.[0-9]*)?")
/** Like `PatPosFloat`, but allows an optional "-" or "+" at the start of the match. */
final case object PatFloat extends Matcher("(?:\\+|-)?[0-9]+(?:.[0-9]*)?")

