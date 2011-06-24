package com.digitaldoodles.rex

object Implicits {
	/** Converts a string to a literal pattern. */
	implicit def stringToLit(s:String): Matcher = Lit(s)
}

/**An instance of this class is thrown if the `Tokenizer` class encounters an error during its
 * attempt to process input.
 */
class TokenizerException(message: String) extends java.lang.RuntimeException(message)

/**Provides the ability to process matched subtrings in different ways, depending on which pattern they matched.
 * A `Tokenizer` is constructed with a number of patterns and corresponding functions of type (`MatchResult` => `T`),
 * and via the `tokenize` method, returns a Seq[T] obtained by, each time a pattern matches in the input, applying
 * the corresponding function to the input to get a result of `T`.
 *
 * @param default This function is applied to sections of the input that are not matched by any of the supplied Matchers.
 *
 * @param alternatives: A sequence of `Matcher -> (MatchResult => T)` tuples. The `Matcher` instances are combined
 * into a single pattern using the `|` operator, but the association between them and the function given with them
 * is maintained.
 */
final class Tokenizer[T](default: (MatchResult => T), alternatives: Seq[(Matcher, MatchResult => T)]) {
	private var tokenizer: Matcher = alternatives(0)._1.internalName("$alt0")
	private var processors = scala.collection.mutable.HashMap("$alt0" -> alternatives(0)._2)
	for (i <- 1 until alternatives.length) {
		tokenizer = tokenizer | alternatives(i)._1.internalName("$alt" + i)
		processors += ("$alt"+i) -> alternatives(i)._2
	}

	private def process(mr: MatchResult): T = {
		if (!mr.matched) default(mr)
		else {
			for (n <- processors.keys) {
				if (mr(n) != null) {
					return processors(n)(mr)
				}
			}
			throw new TokenizerException("Could not find a name associated with a tokenized substring. This indicates a bug in com.digitaldoodles.rex")
		}
	}

	/**Runs the `Tokenizer` over the given input string. The single pattern produced from the various alternative
	 * patterns given when the `Tokenizer` was created is used to iterate through the input.
	 * Whenever one of the subpatterns matches part of the input, its corresponding function is applied to
     * the `MatchResult` to obtain a result of `T` that will be incorporated into the final output. For sections
	 * of the input that fail to match any of the provided patterns, the `default` function provided when the
	 * `Tokenizer` was created is used to process that part of the string.
	 *
	 * @return A `Seq[T]` produced via the process described above.
	 */
	def tokenize(input: String): Seq[T] = new Matching(tokenizer, input).iterator.toSeq.map(process(_))
}

/** This class is experimental; it may disappear or undergo significant changes in future versions of Rex. */
final case class MatchingResult(input:String, start: Int, end: Int)

/**This class represents an instance of a regular expression (Matcher) associated with
 * an input string. It is intended for "heavy-duty" usage, for example when you have a Matcher
 * and want to do various things with it using the same input string.
 *
 * This class is experimental. It may disappear or be heavily modified in future versions of Rex.
 */
final class Matching(val matcher: Matcher, val input: String) extends Iterable[MatchResult] {
	private val jMatcher = matcher.regex.pattern.matcher(input)
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

object Lit {
	/** Add a backquote immediately before any characters that have a special meaning
	 * in regular expressions outside of character classes. */
	private[Lit] def backQuoteLiteralSpecials(s: String): String =
		(for (c <- s) yield (if ("[]^$\\.{,}|+*<".indexOf(c) >= 0) "\\" + c else c)).mkString("")
}

/** Defines a literal pattern.
 *
 * Characters with special meanings to the regex engine are backquoted
 * internally, so do not need to be backquoted when calling this constructor, but
 * characters with special meanings in strings may still need to be backquoted.
 * 
 * A literal pattern matches a substring of the input that is identical to the `lit` argument.
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
	private[rex] val names = m1.nameToGroupNumber.keySet intersect m2.nameToGroupNumber.keySet
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
	private[rex] override val groupCount = m1.groupCount + m2.groupCount

	/** Given a name name, return its corresponding name number.

		 @see Matcher.name(name)
	 */
	private[rex] override val nameToGroupNumber = m1.nameToGroupNumber ++
			m2.nameToGroupNumber.map(x => (x._1, x._2 + m1.groupCount))
}

/** Responsible for numbering of named groups.*/
private[rex] class GroupMatcher(pat: Matcher, val name: String) extends Matcher {

	private[rex] def buildPattern(nameMap: Map[String, Int]) = "(" + pat.buildPattern(nameMap) + ")"

	private[rex] val lowestPrecedenceInPattern = 0

	/** Creates a new map of names to left parenthesis numbers by mapping over the previously existing
	 * map. */
	private[rex] override val nameToGroupNumber: Map[String, Int] = {
		if (pat.nameToGroupNumber.contains(name)) {
			throw new NameException("Attempt to create pattern with same name as existing subgroup name: '%s'" format name)
		}
		if (name.endsWith(".")) Map(name.substring(0, name.length-1) -> 1) ++ pat.nameToGroupNumber.map(x => (name + x._1, x._2 + 1))
		else Map(name -> 1) ++ pat.nameToGroupNumber.map(x => (x._1, x._2 + 1))
	}

	private[rex] override val groupCount = 1 + pat.groupCount
}

/** Represents an instance of an anonymous (unnumbered, unnamed) name. */
private[rex] class AnonGroup(val anonType: String, val m: Matcher) extends Matcher {
	private[rex] def buildPattern(nameMap: Map[String, Int]) = "(?" + anonType + m.buildPattern(nameMap) + ")"
	private[rex] val lowestPrecedenceInPattern = 0
}

/**Instances of this class are thrown to indicate problems with named groups, such as duplicate names,
 * illegal names, and so on.
 */
class NameException(message: String) extends java.lang.RuntimeException(message)

/**Back-reference to a previously defined named group.
 * 
 * @param name The name of a group defined earlier in the pattern this back-reference is part of.
 *
 * The `SameAs` pattern will match a section of the input that is _exactly the same as_ the input
 * matched by the group named by `name`.
 *
 * Note that if when `SameAs` is processed, it refers to a group that has not yet been matched, the
 * result is, so far as I can tell, undefined. In the experiments I've tried, the match appears to
 * simply fail.
 *
 * @throws NameError if `name` is not the name of a group that exists elsewhere in the pattern.
 */
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

