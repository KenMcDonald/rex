package com.digitaldoodles.rex

import util.matching.Regex
import annotation.target

/** Represents the result of a match. This can either be a successful or unsuccessful match,
 which permits MatchResults to return the strings "between" the matches when iterating a
 Matcher over a target string. To determine if a MatchResult represents a successful
 or unsuccessful result, access the `matched` boolean attribute.

@param matched true if this was a successful match, false otherwise.
@see The testing code for simple examples.
 */
final class MatchResult(val matched: Boolean, private val m: Regex.Match, private val s: String, private val matcher: Matcher) {

	/** Retrieve a name by its name */
	def group(name:String) = m.group(matcher.nameToGroupNumber(name))

	/** Retrieve a name by its number. */
	def group(groupNum:Int) = {
		if (!matched && groupNum == 0) s
		else m.group(groupNum)
	}

	/** Return the entire matched string. */
	override def toString = this.group(0)

	/** Synonym for `toString`. */
	def string = toString
}

/**
 * Allows iteration over a series of MatchResult instances. Unlike in most
 * regular expression libraries, these instances give you access to not just which
 * substrings matched the regular expression, but also which substrings didn't.
 *
 * @see MatchResult
 */
final class MatchResultIterator(val input: String, val matches: Iterator[Regex.Match], val matcher: Matcher) extends Iterator[MatchResult] {
	private var nextStart = 0
	private var lastEnd = 0
	private var nextMatch: Regex.Match = null

	def hasNext = nextMatch != null || matches.hasNext || lastEnd < input.length

	private def substring(start:Int, end:Int) = {
		lastEnd = end
		new MatchResult(false, null, input.substring(start, end), matcher)
	}
	private def makeNextMatch = {
		lastEnd = nextMatch.end
		val tmp = nextMatch
		nextMatch = null
		new MatchResult(true, tmp, null, matcher)
	}

	def next = {
		if (nextMatch != null) {
			if (nextMatch.start > lastEnd) substring(lastEnd, input.length)
			else makeNextMatch
		} else if (!matches.hasNext) {
			// At this point, we know that lastEnd < target.length, otherwise
			// hasNext would have returned false and next would therefore not have
			// been called.
			substring(lastEnd, input.length)
		} else {
			nextMatch = matches.next
			if (lastEnd < nextMatch.start) substring(lastEnd, nextMatch.start)
			else makeNextMatch
		}
	}
}

