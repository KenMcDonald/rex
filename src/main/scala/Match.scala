package com.digitaldoodles.rex

object Implicits {
	implicit def stringToLit(s:String): Matcher = Lit(s)
}

import Implicits._

final case class MatchingResult(input:String, start: Int, end: Int)

/**An instance of this class is thrown if the `Tokenizer` class encounters an error during its
 * attempt to process input.
 */
class TokenizerException(message: String) extends java.lang.RuntimeException(message)

final class Tokenizer[T](default: (MatchResult=>T), alternatives: (Matcher, MatchResult=>T)*) {
	var tokenizer: Matcher = alternatives(0)._1.internalName("$alt0")
	var processors = scala.collection.mutable.HashMap("$alt0" -> alternatives(0)._2)
	for (i <- 1 until alternatives.length) {
		tokenizer = tokenizer | alternatives(i)._1.internalName("$alt" + i)
		processors += ("$alt"+i) -> alternatives(i)._2
	}

	private def process(mr: MatchResult): T = {
		if (!mr.matched) default(mr)
		else {
			for (n <- processors.keys) {
				if (mr.group(n) != null) {
					return processors(n)(mr)
				}
			}
			throw new TokenizerException("Could not find a name associated with a tokenized substring. This indicates a bug in com.digitaldoodles.rex")
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

private object Lit {
	/** Add a backquote immediately before any characters that have a special meaning
	 * in regular expressions outside of character classes. */
	def backQuoteLiteralSpecials(s: String): String =
		(for (c <- s) yield (if ("[]^$\\.{,}|+*<".indexOf(c) >= 0) "\\" + c else c)).mkString("")
}

/** Define a literal pattern. This pattern is quoted internally, so no regular expression
 characters have special meanings in it. Of course, string special characters still have
 their special meanings in the string argument, if you use single-quoted strings.
 */
case class Lit(lit: String) extends Matcher {
	private[rex] def buildPattern(nameToGroupNumber: Map[String, Int]) = Lit.backQuoteLiteralSpecials(lit)

	private[rex] val lowestPrecedenceInPattern = if (lit.length <= 1) 0 else 2
}

private[rex] object BinopMatcher {
	val precedences = Map("" -> 2, "|" -> 3)
}

/** Used in constructing binary rex operations. */
private[rex] class BinopMatcher(val m1: Matcher, op: String, val m2: Matcher) extends Matcher {
	val names = m1.nameToGroupNumber.keySet intersect m2.nameToGroupNumber.keySet
	if (names.size > 0) {
		throw new NameException("Attempt to create pattern with duplicate subgroup name(s): %s" format names.mkString("'", "', '", "'"))
	}

	private[rex] def buildPattern(nameMap: Map[String, Int]): String = {
		val pattern1 = if (m1.lowestPrecedenceInPattern <= BinopMatcher.precedences(op)) m1.buildPattern(nameMap) else m1.anonGroup(nameMap)
		val pattern2 = if (m2.lowestPrecedenceInPattern <= BinopMatcher.precedences(op)) m2.buildPattern(nameMap) else m2.anonGroup(nameMap)
		pattern1 + op + pattern2
	}

	private[rex] val lowestPrecedenceInPattern = BinopMatcher.precedences(op)

	/** Number of non-anonymous (counting) groups in this regular expression. */
	override val groupCount = m1.groupCount + m2.groupCount

	/** Given a name name, return its corresponding name number.

		 @see Matcher.name(name)
	 */
	override val nameToGroupNumber = m1.nameToGroupNumber ++
			m2.nameToGroupNumber.map(x => (x._1, x._2 + m1.groupCount))
}

/** Responsible for numbering of named groups.*/
private[rex] class GroupMatcher(pat: Matcher, val name: String) extends Matcher {

	private[rex] def buildPattern(nameMap: Map[String, Int]) = "(" + pat.buildPattern(nameMap) + ")"

	private[rex] val lowestPrecedenceInPattern = 0

	/** Creates a new map of name names to left parenthesis numbers by mapping over the previously existing
	 * map. */
	override val nameToGroupNumber: Map[String, Int] = {
		if (pat.nameToGroupNumber.contains(name)) {
			throw new NameException("Attempt to create pattern with same name as existing subgroup name: '%s'" format name)
		}
		if (name.endsWith(".")) Map(name.substring(0, name.length-1) -> 1) ++ pat.nameToGroupNumber.map(x => (name + x._1, x._2 + 1))
		else Map(name -> 1) ++ pat.nameToGroupNumber.map(x => (x._1, x._2 + 1))
	}

	override val groupCount = 1 + pat.groupCount
}

/** Represents an instance of an anonymous (unnumbered, unnamed) name. */
private[rex] class AnonGroup(pattern: String) extends Matcher {

	private[rex] def buildPattern(nameMap: Map[String, Int]) = "(?" + pattern

	private[rex] val lowestPrecedenceInPattern = 0
}

class NameException(message: String) extends java.lang.RuntimeException(message)

case class SameAs(name: String) extends Matcher {
	private[rex] val lowestPrecedenceInPattern = 0

	private[rex] def buildPattern(nameMap: Map[String, Int]) = {
		"\\" + nameMap.getOrElse(name, throw new NameException("Reference to name '%s' which is not a valid group name." format name)).toString
	}
}

private[rex] class RepMatcher(m: Matcher, repOp: String) extends Matcher {

	private[rex] def buildPattern(nameMap: Map[String, Int]) = {
		val pattern = if (m.lowestPrecedenceInPattern > 1) m.anonGroup(nameMap) else m.buildPattern(nameMap)
		pattern + repOp
	}

	private[rex] val lowestPrecedenceInPattern = 1
}

