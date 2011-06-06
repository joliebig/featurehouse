

package org.gjt.sp.jedit.syntax;


import gnu.regexp.*;
import javax.swing.text.Segment;
import java.util.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.CharIndexedSegment;
import org.gjt.sp.util.Log;



public class TokenMarker
{
	
	public TokenMarker()
	{
		ruleSets = new Hashtable(64);
	} 

	
	public void addRuleSet(ParserRuleSet rules)
	{
		ruleSets.put(rules.getSetName(), rules);

		if (rules.getSetName().equals("MAIN"))
			mainRuleSet = rules;
	} 

	
	public ParserRuleSet getMainRuleSet()
	{
		return mainRuleSet;
	} 

	
	public ParserRuleSet getRuleSet(String setName)
	{
		return (ParserRuleSet) ruleSets.get(setName);
	} 

	
	
	public LineContext markTokens(LineContext prevContext,
		TokenHandler tokenHandler, Segment line)
	{
		
		
		
		this.tokenHandler = tokenHandler;
		this.line = line;

		lastOffset = line.offset;
		lineLength = line.count + line.offset;

		context = new LineContext();

		if(prevContext == null)
			context.rules = getMainRuleSet();
		else
		{
			context.parent = prevContext.parent;
			context.inRule = prevContext.inRule;
			context.rules = prevContext.rules;
			context.spanEndSubst = prevContext.spanEndSubst;
		}

		keywords = context.rules.getKeywords();
		escaped = false;

		seenWhitespaceEnd = false;
		whitespaceEnd = line.offset;
		

		
		ParserRule rule;
		int terminateChar = context.rules.getTerminateChar();
		boolean terminated = false;

main_loop:	for(pos = line.offset; pos < lineLength; pos++)
		{
			
			if(terminateChar >= 0 && pos - line.offset >= terminateChar
				&& !terminated)
			{
				terminated = true;
				context = new LineContext(ParserRuleSet
					.getStandardRuleSet(context.rules
					.getDefault()),context);
				keywords = context.rules.getKeywords();
			} 

			
			if(context.parent != null)
			{
				rule = context.parent.inRule;
				if(rule != null)
				{
					if(checkDelegateEnd(rule))
					{
						seenWhitespaceEnd = true;
						continue main_loop;
					}
				}
			} 

			
			char ch = line.array[pos];

			rule = context.rules.getRules(ch);
			while(rule != null)
			{
				
				if (handleRule(rule,false))
				{
					seenWhitespaceEnd = true;
					continue main_loop;
				}

				rule = rule.next;
			} 

			
			if(Character.isWhitespace(ch))
			{
				if(!seenWhitespaceEnd)
					whitespaceEnd = pos + 1;

				if(context.inRule != null)
					handleRule(context.inRule,true);

				handleNoWordBreak();

				markKeyword(false);

				if(lastOffset != pos)
				{
					tokenHandler.handleToken(
						context.rules.getDefault(),
						lastOffset - line.offset,
						pos - lastOffset,
						context);
				}

				tokenHandler.handleToken(context.rules.getDefault(),
					pos - line.offset,1,context);
				lastOffset = pos + 1;

				escaped = false;
			}
			else
			{
				if(keywords != null || context.rules.getRuleCount() != 0)
				{
					String noWordSep = context.rules.getNoWordSep();

					if(!Character.isLetterOrDigit(ch)
						&& noWordSep.indexOf(ch) == -1)
					{
						if(context.inRule != null)
							handleRule(context.inRule,true);

						handleNoWordBreak();

						markKeyword(true);

						tokenHandler.handleToken(
							context.rules.getDefault(),
							lastOffset - line.offset,1,
							context);
						lastOffset = pos + 1;
					}
				}

				seenWhitespaceEnd = true;
				escaped = false;
			} 
		} 

		
		pos = lineLength;

		if(context.inRule != null)
			handleRule(context.inRule,true);

		handleNoWordBreak();
		markKeyword(true);
		

		
unwind:		while(context.parent != null)
		{
			rule = context.parent.inRule;
			if((rule != null && (rule.action
				& ParserRule.NO_LINE_BREAK) == ParserRule.NO_LINE_BREAK)
				|| terminated)
			{
				context = context.parent;
				keywords = context.rules.getKeywords();
				context.inRule = null;
			}
			else
				break unwind;
		} 

		tokenHandler.handleToken(Token.END,pos - line.offset,0,context);

		return context.intern();
	} 

	

	
	private Hashtable ruleSets;
	private ParserRuleSet mainRuleSet;

	
	
	private TokenHandler tokenHandler;
	private Segment line;
	private LineContext context;
	private KeywordMap keywords;
	private Segment pattern = new Segment();
	private int lastOffset;
	private int lineLength;
	private int pos;
	private boolean escaped;

	private int whitespaceEnd;
	private boolean seenWhitespaceEnd;
	

	
	private boolean checkDelegateEnd(ParserRule rule)
	{
		if(rule.end == null)
			return false;

		LineContext tempContext = context;
		context = context.parent;
		keywords = context.rules.getKeywords();
		boolean tempEscaped = escaped;
		boolean b = handleRule(rule,true);
		context = tempContext;
		keywords = context.rules.getKeywords();

		if(b && !tempEscaped)
		{
			if(context.inRule != null)
				handleRule(context.inRule,true);

			markKeyword(true);

			context = (LineContext)context.parent.clone();

			tokenHandler.handleToken(
				(context.inRule.action & ParserRule.EXCLUDE_MATCH)
				== ParserRule.EXCLUDE_MATCH
				? context.rules.getDefault()
				: context.inRule.token,
				pos - line.offset,pattern.count,context);

			keywords = context.rules.getKeywords();
			context.inRule = null;
			lastOffset = pos + pattern.count;

			
			pos += (pattern.count - 1);

			return true;
		}

		
		if((rule.action & ParserRule.NO_ESCAPE) == 0)
		{
			ParserRule escape = context.parent.rules.getEscapeRule();
			if(escape != null && handleRule(escape,false))
				return true;
		}

		return false;
	} 

	
	
	private boolean handleRule(ParserRule checkRule, boolean end)
	{
		
		if(!end)
		{
			if(Character.toUpperCase(checkRule.hashChar)
				!= Character.toUpperCase(line.array[pos]))
			{
				return false;
			}
		}

		int offset = ((checkRule.action & ParserRule.MARK_PREVIOUS) != 0) ?
			lastOffset : pos;
		int posMatch = (end ? checkRule.endPosMatch : checkRule.startPosMatch);

		if((posMatch & ParserRule.AT_LINE_START)
			== ParserRule.AT_LINE_START)
		{
			if(offset != line.offset)
				return false;
		}
		else if((posMatch & ParserRule.AT_WHITESPACE_END)
			== ParserRule.AT_WHITESPACE_END)
		{
			if(offset != whitespaceEnd)
				return false;
		}
		else if((posMatch & ParserRule.AT_WORD_START)
			== ParserRule.AT_WORD_START)
		{
			if(offset != lastOffset)
				return false;
		} 

		int matchedChars = 1;
		CharIndexedSegment charIndexed = null;

		
		if(!end || (checkRule.action & ParserRule.MARK_FOLLOWING) == 0)
		{
			
			if((checkRule.action & ParserRule.REGEXP) == 0 || end)
			{
				if(end)
				{
					if(context.spanEndSubst != null)
						pattern.array = context.spanEndSubst.toCharArray();
					else
						pattern.array = checkRule.end;
				}
				else
					pattern.array = checkRule.start;
				pattern.offset = 0;
				pattern.count = pattern.array.length;
				matchedChars = pattern.count;

				if(!SyntaxUtilities.regionMatches(context.rules
					.getIgnoreCase(),line,pos,pattern.array))
				{
					return false;
				}
			}
			else
			{
				
				
				int matchStart = pos - line.offset;
				charIndexed = new CharIndexedSegment(line,matchStart);
				REMatch match = checkRule.startRegexp.getMatch(
					charIndexed,0,RE.REG_ANCHORINDEX);
				if(match == null)
					return false;
				else if(match.getStartIndex() != 0)
					throw new InternalError("Can't happen");
				else
					matchedChars = match.getEndIndex();
			}
		} 

		
		if((checkRule.action & ParserRule.IS_ESCAPE) == ParserRule.IS_ESCAPE)
		{
			if(context.inRule != null)
				handleRule(context.inRule,true);

			escaped = !escaped;
			pos += pattern.count - 1;
		}
		else if(escaped)
		{
			escaped = false;
			pos += pattern.count - 1;
		} 
		
		else if(!end)
		{
			if(context.inRule != null)
				handleRule(context.inRule,true);

			markKeyword((checkRule.action & ParserRule.MARK_PREVIOUS)
				!= ParserRule.MARK_PREVIOUS);

			switch(checkRule.action & ParserRule.MAJOR_ACTIONS)
			{
			
			case ParserRule.SEQ:
				context.spanEndSubst = null;

				if((checkRule.action & ParserRule.REGEXP) != 0)
				{
					handleTokenWithTabs(tokenHandler,
						checkRule.token,
						pos - line.offset,
						matchedChars,
						context);
				}
				else
				{
					tokenHandler.handleToken(checkRule.token,
						pos - line.offset,matchedChars,context);
				}

				
				
				if(checkRule.delegate != null)
				{
					context = new LineContext(
						checkRule.delegate,
						context.parent);
					keywords = context.rules.getKeywords();
				}
				break;
			
			
			case ParserRule.SPAN:
			case ParserRule.EOL_SPAN:
				context.inRule = checkRule;

				byte tokenType = ((checkRule.action & ParserRule.EXCLUDE_MATCH)
					== ParserRule.EXCLUDE_MATCH
					? context.rules.getDefault() : checkRule.token);

				if((checkRule.action & ParserRule.REGEXP) != 0)
				{
					handleTokenWithTabs(tokenHandler,
						tokenType,
						pos - line.offset,
						matchedChars,
						context);
				}
				else
				{
					tokenHandler.handleToken(tokenType,
						pos - line.offset,matchedChars,context);
				}

				String spanEndSubst = null;
				
				if(charIndexed != null)
				{
					spanEndSubst = checkRule.startRegexp.substitute(
						charIndexed,new String(checkRule.end));
				}

				context.spanEndSubst = spanEndSubst;
				context = new LineContext(
					checkRule.delegate,
					context);
				keywords = context.rules.getKeywords();

				break;
			
			
			case ParserRule.MARK_FOLLOWING:
				tokenHandler.handleToken((checkRule.action
					& ParserRule.EXCLUDE_MATCH)
					== ParserRule.EXCLUDE_MATCH ?
					context.rules.getDefault()
					: checkRule.token,pos - line.offset,
					pattern.count,context);

				context.spanEndSubst = null;
				context.inRule = checkRule;
				break;
			
			
			case ParserRule.MARK_PREVIOUS:
				context.spanEndSubst = null;

				if ((checkRule.action & ParserRule.EXCLUDE_MATCH)
					== ParserRule.EXCLUDE_MATCH)
				{
					if(pos != lastOffset)
					{
						tokenHandler.handleToken(
							checkRule.token,
							lastOffset - line.offset,
							pos - lastOffset,
							context);
					}

					tokenHandler.handleToken(
						context.rules.getDefault(),
						pos - line.offset,pattern.count,
						context);
				}
				else
				{
					tokenHandler.handleToken(checkRule.token,
						lastOffset - line.offset,
						pos - lastOffset + pattern.count,
						context);
				}

				break;
			
			default:
				throw new InternalError("Unhandled major action");
			}

			
			pos += (matchedChars - 1);
			lastOffset = pos + 1;

			
		} 
		
		else if((context.inRule.action & ParserRule.MARK_FOLLOWING) != 0)
		{
			if(pos != lastOffset)
			{
				tokenHandler.handleToken(
					context.inRule.token,
					lastOffset - line.offset,
					pos - lastOffset,context);
			}

			lastOffset = pos;
			context.inRule = null;
		} 

		return true;
	} 

	
	private void handleNoWordBreak()
	{
		if(context.parent != null)
		{
			ParserRule rule = context.parent.inRule;
			if(rule != null && (context.parent.inRule.action
				& ParserRule.NO_WORD_BREAK) != 0)
			{
				if(pos != lastOffset)
				{
					tokenHandler.handleToken(rule.token,
						lastOffset - line.offset,
						pos - lastOffset,context);
				}

				lastOffset = pos;
				context = context.parent;
				keywords = context.rules.getKeywords();
				context.inRule = null;
			}
		}
	} 

	
	private void handleTokenWithTabs(TokenHandler tokenHandler, byte tokenType,
		int start, int len, LineContext context)
	{
		int last = start;
		int end = start + len;

		for(int i = start; i < end; i++)
		{
			if(line.array[i] == '\t')
			{
				if(last != i)
					tokenHandler.handleToken(tokenType,last,i - last,context);
				tokenHandler.handleToken(tokenType,i,1,context);
				last = i + 1;
			}
		}

		if(last != end)
			tokenHandler.handleToken(tokenType,last,end - last,context);
	} 

	
	private void markKeyword(boolean addRemaining)
	{
		int len = pos - lastOffset;
		if(len == 0)
			return;

		
		if(context.rules.getHighlightDigits())
		{
			boolean digit = false;
			boolean mixed = false;

			for(int i = lastOffset; i < pos; i++)
			{
				char ch = line.array[i];
				if(Character.isDigit(ch))
					digit = true;
				else
					mixed = true;
			}

			if(mixed)
			{
				RE digitRE = context.rules.getDigitRegexp();

				
				
				if(digit)
				{ 
					if(digitRE == null)
					{
						
						
						
						digit = false;
					}
					else
					{
						CharIndexedSegment seg = new CharIndexedSegment(
							line,false);
						int oldCount = line.count;
						int oldOffset = line.offset;
						line.offset = lastOffset;
						line.count = len;
						if(!digitRE.isMatch(seg))
							digit = false;
						line.offset = oldOffset;
						line.count = oldCount;
					}
				}
			}

			if(digit)
			{
				tokenHandler.handleToken(Token.DIGIT,
					lastOffset - line.offset,
					len,context);
				lastOffset = pos;

				return;
			}
		} 

		
		if(keywords != null)
		{
			byte id = keywords.lookup(line, lastOffset, len);

			if(id != Token.NULL)
			{
				tokenHandler.handleToken(id,
					lastOffset - line.offset,
					len,context);
				lastOffset = pos;
				return;
			}
		} 

		
		if(addRemaining)
		{
			tokenHandler.handleToken(context.rules.getDefault(),
				lastOffset - line.offset,len,context);
			lastOffset = pos;
		} 
	} 

	

	
	
	public static class LineContext
	{
		private static Hashtable intern = new Hashtable();

		public LineContext parent;
		public ParserRule inRule;
		public ParserRuleSet rules;
		
		public String spanEndSubst;

		
		public LineContext(ParserRuleSet rs, LineContext lc)
		{
			rules = rs;
			parent = (lc == null ? null : (LineContext)lc.clone());
		} 

		
		public LineContext()
		{
		} 

		
		public LineContext intern()
		{
			Object obj = intern.get(this);
			if(obj == null)
			{
				intern.put(this,this);
				return this;
			}
			else
				return (LineContext)obj;
		} 

		
		public int hashCode()
		{
			if(inRule != null)
				return inRule.hashCode();
			else if(rules != null)
				return rules.hashCode();
			else
				return 0;
		} 

		
		public boolean equals(Object obj)
		{
			if(obj instanceof LineContext)
			{
				LineContext lc = (LineContext)obj;
				return lc.inRule == inRule && lc.rules == rules
					&& MiscUtilities.objectsEqual(parent,lc.parent)
					&& MiscUtilities.objectsEqual(spanEndSubst,lc.spanEndSubst);
			}
			else
				return false;
		} 

		
		public Object clone()
		{
			LineContext lc = new LineContext();
			lc.inRule = inRule;
			lc.rules = rules;
			lc.parent = (parent == null) ? null : (LineContext) parent.clone();
			lc.spanEndSubst = spanEndSubst;

			return lc;
		} 
	} 
}
