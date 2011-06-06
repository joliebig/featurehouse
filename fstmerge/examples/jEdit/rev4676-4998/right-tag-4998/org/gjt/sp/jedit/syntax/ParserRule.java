

package org.gjt.sp.jedit.syntax;

import gnu.regexp.*;


public class ParserRule
{
	
	
	
	public static final RESyntax RE_SYNTAX_JEDIT
		= new RESyntax(RESyntax.RE_SYNTAX_PERL5)
		.set(RESyntax.RE_CHAR_CLASSES)
		.setLineSeparator("\n");

	
	public static final int MAJOR_ACTIONS = 0x000000FF;
	public static final int SEQ = 0;
	public static final int SPAN = 1 << 1;
	public static final int MARK_PREVIOUS = 1 << 2;
	public static final int MARK_FOLLOWING = 1 << 3;
	public static final int EOL_SPAN = 1 << 4;
	

	
	public static final int ACTION_HINTS = 0x0000FF00;
	public static final int EXCLUDE_MATCH = 1 << 8;
	public static final int NO_LINE_BREAK = 1 << 9;
	public static final int NO_WORD_BREAK = 1 << 10;
	public static final int IS_ESCAPE = 1 << 11;
	public static final int NO_ESCAPE = 1 << 12;
	public static final int REGEXP = 1 << 13;
	

	
	public static final int AT_LINE_START = 1 << 1;
	public static final int AT_WHITESPACE_END = 1 << 2;
	public static final int AT_WORD_START = 1 << 3;
	

	
	public final char hashChar;
	public final int startPosMatch;
	public final char[] start;
	public final RE startRegexp;

	public final int endPosMatch;
	public final char[] end;

	public final int action;
	public final byte token;

	public ParserRuleSet delegate;

	public ParserRule next;
	

	
	public static final ParserRule createSequenceRule(
		int posMatch, String seq, ParserRuleSet delegate, byte id)
	{
		int ruleAction = SEQ;

		return new ParserRule(SEQ, seq.charAt(0),
			posMatch, seq.toCharArray(), null,
			0, null, delegate, id);
	} 

	
	public static final ParserRule createRegexpSequenceRule(
		char hashChar, int posMatch, String seq,
		ParserRuleSet delegate, byte id, boolean ignoreCase)
		throws REException
	{
		return new ParserRule(SEQ | REGEXP, hashChar, posMatch,
			null, new RE("\\A" + seq,(ignoreCase ? RE.REG_ICASE : 0),
			RE_SYNTAX_JEDIT), 0,
			null, delegate, id);
	} 

	
	public static final ParserRule createSpanRule(
		int startPosMatch, String start, int endPosMatch, String end,
		ParserRuleSet delegate, byte id, boolean excludeMatch,
		boolean noLineBreak, boolean noWordBreak, boolean noEscape)
	{
		int ruleAction = SPAN |
			((noLineBreak) ? NO_LINE_BREAK : 0) |
			((excludeMatch) ? EXCLUDE_MATCH : 0) |
			((noWordBreak) ? NO_WORD_BREAK : 0) |
			((noEscape) ? NO_ESCAPE : 0);

		return new ParserRule(ruleAction, start.charAt(0), startPosMatch,
			start.toCharArray(), null,
			endPosMatch, end.toCharArray(),
			delegate, id);
	} 

	
	public static final ParserRule createRegexpSpanRule(
		char hashChar, int startPosMatch, String start,
		int endPosMatch, String end, ParserRuleSet delegate, byte id,
		boolean excludeMatch, boolean noLineBreak, boolean noWordBreak,
		boolean ignoreCase, boolean noEscape)
		throws REException
	{
		int ruleAction = SPAN | REGEXP |
			((noLineBreak) ? NO_LINE_BREAK : 0) |
			((excludeMatch) ? EXCLUDE_MATCH : 0) |
			((noWordBreak) ? NO_WORD_BREAK : 0) |
			((noEscape) ? NO_ESCAPE : 0);

		return new ParserRule(ruleAction, hashChar, startPosMatch, null,
			new RE("\\A" + start,(ignoreCase ? RE.REG_ICASE : 0),
			RE_SYNTAX_JEDIT), endPosMatch,
			end.toCharArray(), delegate, id);
	} 

	
	public static final ParserRule createEOLSpanRule(
		int posMatch, String seq, ParserRuleSet delegate, byte id,
		boolean excludeMatch)
	{
		int ruleAction = EOL_SPAN |
			((excludeMatch) ? EXCLUDE_MATCH : 0)
			| NO_LINE_BREAK;

		return new ParserRule(ruleAction, seq.charAt(0), posMatch,
			seq.toCharArray(), null, 0, null,
			delegate, id);
	} 

	
	public static final ParserRule createRegexpEOLSpanRule(
		char hashChar, int posMatch, String seq, ParserRuleSet delegate,
		byte id, boolean excludeMatch, boolean ignoreCase)
		throws REException
	{
		int ruleAction = EOL_SPAN | REGEXP |
			((excludeMatch) ? EXCLUDE_MATCH : 0)
			| NO_LINE_BREAK;

		return new ParserRule(ruleAction, hashChar, posMatch,
			null, new RE("\\A" + seq,(ignoreCase ? RE.REG_ICASE : 0),
			RE_SYNTAX_JEDIT), 0, null,
			delegate, id);
	} 

	
	public static final ParserRule createMarkFollowingRule(
		int posMatch, String seq, byte id, boolean excludeMatch)
	{
		int ruleAction = MARK_FOLLOWING |
			((excludeMatch) ? EXCLUDE_MATCH : 0);

		return new ParserRule(ruleAction, seq.charAt(0), posMatch,
			seq.toCharArray(), null, 0, null, null, id);
	} 

	
	public static final ParserRule createMarkPreviousRule(
		int posMatch, String seq, byte id, boolean excludeMatch)
	{
		int ruleAction = MARK_PREVIOUS |
			((excludeMatch) ? EXCLUDE_MATCH : 0);

		return new ParserRule(ruleAction, seq.charAt(0), posMatch,
			seq.toCharArray(), null, 0, null, null, id);
	} 

	
	public static final ParserRule createEscapeRule(String seq)
	{
		int ruleAction = IS_ESCAPE;

		return new ParserRule(ruleAction, seq.charAt(0),
			0, seq.toCharArray(), null, 0, null,
			null, Token.NULL);
	} 

	
	private ParserRule(int action, char hashChar,
		int startPosMatch, char[] start, RE startRegexp,
		int endPosMatch, char[] end,
		ParserRuleSet delegate, byte token)
	{
		this.action = action;
		this.hashChar = hashChar;
		this.startPosMatch = startPosMatch;
		this.start = start;
		this.startRegexp = startRegexp;
		this.endPosMatch = endPosMatch;
		this.end = end;
		this.delegate = delegate;
		this.token = token;

		if(this.delegate == null)
		{
			if((action & MAJOR_ACTIONS) != SEQ)
			{
				this.delegate = ParserRuleSet.getStandardRuleSet(token);
			}
		}
	} 
}
