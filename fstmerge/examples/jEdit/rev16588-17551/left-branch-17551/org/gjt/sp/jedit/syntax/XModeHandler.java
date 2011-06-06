

package org.gjt.sp.jedit.syntax;


import java.util.*;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.jedit.Mode;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;



public abstract class XModeHandler extends DefaultHandler
{
	
	public XModeHandler (String modeName)
	{
		this.modeName = modeName;
		marker = new TokenMarker();
		marker.addRuleSet(new ParserRuleSet(modeName,"MAIN"));
		stateStack = new Stack<TagDecl>();
	} 

	
	public InputSource resolveEntity(String publicId, String systemId)
	{
		return XMLUtilities.findEntity(systemId, "xmode.dtd", XModeHandler.class);
	} 

	
	public void characters(char[] c, int off, int len)
	{
		peekElement().setText(c, off, len);
	} 

	
	public void startElement(String uri, String localName,
				 String qName, Attributes attrs)
	{
		TagDecl tag = pushElement(qName, attrs);

		if (qName.equals("WHITESPACE"))
		{
			Log.log(Log.WARNING,this,modeName + ": WHITESPACE rule "
				+ "no longer needed");
		}
		else if (qName.equals("KEYWORDS"))
		{
			keywords = new KeywordMap(rules.getIgnoreCase());
		}
		else if (qName.equals("RULES"))
		{
			if(tag.lastSetName == null)
				tag.lastSetName = "MAIN";
			rules = marker.getRuleSet(tag.lastSetName);
			if(rules == null)
			{
				rules = new ParserRuleSet(modeName,tag.lastSetName);
				marker.addRuleSet(rules);
			}
			rules.setIgnoreCase(tag.lastIgnoreCase);
			rules.setHighlightDigits(tag.lastHighlightDigits);
			if(tag.lastDigitRE != null)
			{
				try
				{
					rules.setDigitRegexp(Pattern.compile(tag.lastDigitRE,
						tag.lastIgnoreCase
						? Pattern.CASE_INSENSITIVE : 0));
				}
				catch(PatternSyntaxException e)
				{
					error("regexp",e);
				}
			}

			if(tag.lastEscape != null)
				rules.setEscapeRule(ParserRule.createEscapeRule(tag.lastEscape));
			rules.setDefault(tag.lastDefaultID);
			rules.setNoWordSep(tag.lastNoWordSep);
		}
	} 

	
	public void endElement(String uri, String localName, String name)
	{
		TagDecl tag = popElement();
		if (name.equals(tag.tagName))
		{
			if(tag.lastDelegateSet != null
					&& ! tag.tagName.equals("IMPORT")
					&& ! tag.lastDelegateSet.getModeName().equals(modeName))
			{
				Mode mode = ModeProvider.instance.getMode(tag.lastDelegateSet.getModeName());
				if( ! reloadModes.contains(mode) )
				{
					reloadModes.add(mode);
				}
			}
			
			if (tag.tagName.equals("PROPERTY"))
			{
				props.put(propName,propValue);
			} 
			
			else if (tag.tagName.equals("PROPS"))
			{
				if(peekElement().tagName.equals("RULES"))
					rules.setProperties(props);
				else
					modeProps = props;

				props = new Hashtable<String, String>();
			} 
			
			else if (tag.tagName.equals("RULES"))
			{
				rules.setKeywords(keywords);
				keywords = null;
				rules = null;
			} 
			
			else if (tag.tagName.equals("IMPORT"))
			{
				
				if (!rules.equals(tag.lastDelegateSet))
				{
					rules.addRuleSet(tag.lastDelegateSet);
				}
			} 
			
			else if (tag.tagName.equals("TERMINATE"))
			{
				rules.setTerminateChar(tag.termChar);
			} 
			
			else if (tag.tagName.equals("SEQ"))
			{
				if(tag.lastStart == null)
				{
					error("empty-tag","SEQ");
					return;
				}

				rules.addRule(ParserRule.createSequenceRule(
					tag.lastStartPosMatch,tag.lastStart.toString(),
					tag.lastDelegateSet,tag.lastTokenID));
			} 
			
			else if (tag.tagName.equals("SEQ_REGEXP"))
			{
				if(tag.lastStart == null)
				{
					error("empty-tag","SEQ_REGEXP");
					return;
				}

				try
				{
					if (null != tag.lastHashChars)
					{
						rules.addRule(ParserRule.createRegexpSequenceRule(
							tag.lastStartPosMatch,tag.lastHashChars.toCharArray(),
							tag.lastStart.toString(),tag.lastDelegateSet,
							tag.lastTokenID,findParent("RULES").lastIgnoreCase));
					}
					else
					{
						rules.addRule(ParserRule.createRegexpSequenceRule(
							tag.lastHashChar,tag.lastStartPosMatch,
							tag.lastStart.toString(),tag.lastDelegateSet,
							tag.lastTokenID,findParent("RULES").lastIgnoreCase));
					}
				}
				catch(PatternSyntaxException re)
				{
					error("regexp",re);
				}
			} 
			
			else if (tag.tagName.equals("SPAN"))
			{
				if(tag.lastStart == null)
				{
					error("empty-tag","BEGIN");
					return;
				}

				if(tag.lastEnd == null)
				{
					error("empty-tag","END");
					return;
				}

				rules.addRule(ParserRule
					.createSpanRule(
					tag.lastStartPosMatch,tag.lastStart.toString(),
					tag.lastEndPosMatch,tag.lastEnd.toString(),
					tag.lastDelegateSet,
					tag.lastTokenID,tag.lastMatchType,
					tag.lastNoLineBreak,
					tag.lastNoWordBreak,
					tag.lastEscape));
			} 
			
			else if (tag.tagName.equals("SPAN_REGEXP"))
			{
				if(tag.lastStart == null)
				{
					error("empty-tag","BEGIN");
					return;
				}

				if(tag.lastEnd == null)
				{
					error("empty-tag","END");
					return;
				}

				try
				{
					if (null != tag.lastHashChars)
					{
						rules.addRule(ParserRule
							.createRegexpSpanRule(
							tag.lastStartPosMatch,tag.lastHashChars.toCharArray(),
							tag.lastStart.toString(),
							tag.lastEndPosMatch,tag.lastEnd.toString(),
							tag.lastDelegateSet,
							tag.lastTokenID,
							tag.lastMatchType,
							tag.lastNoLineBreak,
							tag.lastNoWordBreak,
							findParent("RULES").lastIgnoreCase,
							tag.lastEscape));
					}
					else
					{
						rules.addRule(ParserRule
							.createRegexpSpanRule(
							tag.lastHashChar,
							tag.lastStartPosMatch,tag.lastStart.toString(),
							tag.lastEndPosMatch,tag.lastEnd.toString(),
							tag.lastDelegateSet,
							tag.lastTokenID,
							tag.lastMatchType,
							tag.lastNoLineBreak,
							tag.lastNoWordBreak,
							findParent("RULES").lastIgnoreCase,
							tag.lastEscape));
					}
				}
				catch(PatternSyntaxException re)
				{
					error("regexp",re);
				}
			} 
			
			else if (tag.tagName.equals("EOL_SPAN"))
			{
				if(tag.lastStart == null)
				{
					error("empty-tag","EOL_SPAN");
					return;
				}

				rules.addRule(ParserRule.createEOLSpanRule(
					tag.lastStartPosMatch,tag.lastStart.toString(),
					tag.lastDelegateSet,tag.lastTokenID,
					tag.lastMatchType));
			} 
			
			else if (tag.tagName.equals("EOL_SPAN_REGEXP"))
			{
				if(tag.lastStart == null)
				{
					error("empty-tag","EOL_SPAN_REGEXP");
					return;
				}

				try
				{
					if (null != tag.lastHashChars)
					{
						rules.addRule(ParserRule.createRegexpEOLSpanRule(
							tag.lastStartPosMatch,tag.lastHashChars.toCharArray(),
							tag.lastStart.toString(),tag.lastDelegateSet,
							tag.lastTokenID,tag.lastMatchType,
							findParent("RULES").lastIgnoreCase));
					}
					else
					{
						rules.addRule(ParserRule.createRegexpEOLSpanRule(
							tag.lastHashChar,tag.lastStartPosMatch,
							tag.lastStart.toString(),tag.lastDelegateSet,
							tag.lastTokenID,tag.lastMatchType,
							findParent("RULES").lastIgnoreCase));
					}
				}
				catch(PatternSyntaxException re)
				{
					error("regexp",re);
				}
			} 
			
			else if (tag.tagName.equals("MARK_FOLLOWING"))
			{
				if(tag.lastStart == null)
				{
					error("empty-tag","MARK_FOLLOWING");
					return;
				}

				rules.addRule(ParserRule
					.createMarkFollowingRule(
					tag.lastStartPosMatch,tag.lastStart.toString(),
					tag.lastTokenID,tag.lastMatchType));
			} 
			
			else if (tag.tagName.equals("MARK_PREVIOUS"))
			{
				if(tag.lastStart == null)
				{
					error("empty-tag","MARK_PREVIOUS");
					return;
				}

				rules.addRule(ParserRule
					.createMarkPreviousRule(
					tag.lastStartPosMatch,tag.lastStart.toString(),
					tag.lastTokenID,tag.lastMatchType));
			} 
			
			else if (
				!tag.tagName.equals("END")
				&& !tag.tagName.equals("BEGIN")
				&& !tag.tagName.equals("KEYWORDS")
				&& !tag.tagName.equals("MODE"))
			{
				byte token = Token.stringToToken(tag.tagName);
				if(token != -1)
					addKeyword(tag.lastKeyword.toString(),token);
			} 
		}
		else
		{
			
			throw new InternalError();
		}
	} 

	
	public void startDocument()
	{
		props = new Hashtable<String, String>();
		pushElement(null, null);
		reloadModes = new Vector<Mode>();
	} 

	
	public void endDocument()
	{
		ParserRuleSet[] rulesets = marker.getRuleSets();
		for(int i = 0; i < rulesets.length; i++)
		{
			rulesets[i].resolveImports();
		}
		for(Mode mode : reloadModes)
		{
			mode.setTokenMarker(null);
			mode.loadIfNecessary();
		}
	} 

	
	
	public TokenMarker getTokenMarker()
	{
		return marker;
	} 

	
	public Hashtable<String, String> getModeProperties()
	{
		return modeProps;
	} 

	

	
	
	protected abstract void error(String msg, Object subst);
	

	
	
	protected abstract TokenMarker getTokenMarker(String mode);
	

	

	

	
	private String modeName;
	
	private final TokenMarker marker;
	private KeywordMap keywords;
	
	private Stack<TagDecl> stateStack;
	private String propName;
	private String propValue;
	private Hashtable<String, String> props;
	private Hashtable<String, String> modeProps;
	private ParserRuleSet rules;
	
	private Vector<Mode> reloadModes;
	

	
	private void addKeyword(String k, byte id)
	{
		if(k == null)
		{
			error("empty-keyword",null);
			return;
		}

		if (keywords == null) return;
		keywords.add(k,id);
	} 

	
	private TagDecl pushElement(String name, Attributes attrs)
	{
		if (name != null)
		{
			TagDecl tag = new TagDecl(name, attrs);
			stateStack.push(tag);
			return tag;
		}
		else
		{
			stateStack.push(null);
			return null;
		}
	} 

	
	private TagDecl peekElement()
	{
		return stateStack.peek();
	} 

	
	private TagDecl popElement()
	{
		return stateStack.pop();
	} 

	
	
	private TagDecl findParent(String tagName)
	{
		for (int idx = stateStack.size() - 1; idx >= 0; idx--)
		{
			TagDecl tag = stateStack.get(idx);
			if (tag.tagName.equals(tagName))
				return tag;
		}
		return null;
	} 

	

	
	private class TagDecl
	{

		public TagDecl(String tagName, Attributes attrs)
		{
			this.tagName = tagName;

			String tmp;

			propName = attrs.getValue("NAME");
			propValue = attrs.getValue("VALUE");

			tmp = attrs.getValue("TYPE");
			if (tmp != null)
			{
				lastTokenID = Token.stringToToken(tmp);
				if(lastTokenID == -1)
					error("token-invalid",tmp);
			}

			lastMatchType = ParserRule.MATCH_TYPE_RULE;
			
			
			tmp = attrs.getValue("EXCLUDE_MATCH");
			if (tmp != null)
			{
				Log.log(Log.WARNING, this, modeName + ": EXCLUDE_MATCH is deprecated");
				if ("TRUE".equalsIgnoreCase(tmp))
				{
					lastMatchType = ParserRule.MATCH_TYPE_CONTEXT;
				}
			}

			
			tmp = attrs.getValue("MATCH_TYPE");
			if (tmp != null)
			{
				if ("CONTEXT".equals(tmp))
				{
					lastMatchType = ParserRule.MATCH_TYPE_CONTEXT;
				}
				else if ("RULE".equals(tmp))
				{
					lastMatchType = ParserRule.MATCH_TYPE_RULE;
				}
				else
				{
					lastMatchType = Token.stringToToken(tmp);
					if(lastMatchType == -1)
						error("token-invalid",tmp);
				}
			}

			lastAtLineStart = "TRUE".equals(attrs.getValue("AT_LINE_START"));
			lastAtWhitespaceEnd = "TRUE".equals(attrs.getValue("AT_WHITESPACE_END"));
			lastAtWordStart = "TRUE".equals(attrs.getValue("AT_WORD_START"));
			lastNoLineBreak = "TRUE".equals(attrs.getValue("NO_LINE_BREAK"));
			lastNoWordBreak = "TRUE".equals(attrs.getValue("NO_WORD_BREAK"));
			lastIgnoreCase = (attrs.getValue("IGNORE_CASE") == null ||
					"TRUE".equals(attrs.getValue("IGNORE_CASE")));
			lastHighlightDigits = "TRUE".equals(attrs.getValue("HIGHLIGHT_DIGITS"));
			lastDigitRE = attrs.getValue("DIGIT_RE");

			tmp = attrs.getValue("NO_WORD_SEP");
			if (tmp != null)
				lastNoWordSep = tmp;

			tmp = attrs.getValue("AT_CHAR");
			if (tmp != null)
			{
				try
				{
					termChar = Integer.parseInt(tmp);
				}
				catch (NumberFormatException e)
				{
					error("termchar-invalid",tmp);
					termChar = -1;
				}
			}

			lastEscape = attrs.getValue("ESCAPE");
			lastSetName = attrs.getValue("SET");

			tmp = attrs.getValue("DELEGATE");
			if (tmp != null)
			{
				String delegateMode, delegateSetName;

				int index = tmp.indexOf("::");

				if(index != -1)
				{
					delegateMode = tmp.substring(0,index);
					delegateSetName = tmp.substring(index + 2);
				}
				else
				{
					delegateMode = modeName;
					delegateSetName = tmp;
				}

				TokenMarker delegateMarker = getTokenMarker(delegateMode);
				if(delegateMarker == null)
					error("delegate-invalid",tmp);
				else
				{
					lastDelegateSet = delegateMarker
						.getRuleSet(delegateSetName);
					if(delegateMarker == marker
						&& lastDelegateSet == null)
					{
						
						
						lastDelegateSet = new ParserRuleSet(
							delegateMode,
							delegateSetName);
						lastDelegateSet.setDefault(Token.INVALID);
						marker.addRuleSet(lastDelegateSet);
					}
					else if(lastDelegateSet == null)
						error("delegate-invalid",tmp);
				}
			}

			tmp = attrs.getValue("DEFAULT");
			if (tmp != null)
			{
				lastDefaultID = Token.stringToToken(tmp);
				if(lastDefaultID == -1)
				{
					error("token-invalid",tmp);
					lastDefaultID = Token.NULL;
				}
			}

			lastHashChar = attrs.getValue("HASH_CHAR");
			lastHashChars = attrs.getValue("HASH_CHARS");
			if ((null != lastHashChar) && (null != lastHashChars))
			{
				error("hash-char-and-hash-chars-mutually-exclusive",null);
				lastHashChars = null;
			}
		}

		public void setText(char[] c, int off, int len)
		{
			if (tagName.equals("EOL_SPAN") ||
				tagName.equals("EOL_SPAN_REGEXP") ||
				tagName.equals("MARK_PREVIOUS") ||
				tagName.equals("MARK_FOLLOWING") ||
				tagName.equals("SEQ") ||
				tagName.equals("SEQ_REGEXP") ||
				tagName.equals("BEGIN")
			)
			{
				TagDecl target = this;
				if (tagName.equals("BEGIN"))
					target = stateStack.get(stateStack.size() - 2);

				if (target.lastStart == null)
				{
					target.lastStart = new StringBuffer();
					target.lastStart.append(c, off, len);
					target.lastStartPosMatch = ((target.lastAtLineStart ? ParserRule.AT_LINE_START : 0)
						| (target.lastAtWhitespaceEnd ? ParserRule.AT_WHITESPACE_END : 0)
						| (target.lastAtWordStart ? ParserRule.AT_WORD_START : 0));
					target.lastAtLineStart = false;
					target.lastAtWordStart = false;
					target.lastAtWhitespaceEnd = false;
				}
				else
				{
					target.lastStart.append(c, off, len);
				}
			}
			else if (tagName.equals("END"))
			{
				TagDecl target = stateStack.get(stateStack.size() - 2);
				if (target.lastEnd == null)
				{
					target.lastEnd = new StringBuffer();
					target.lastEnd.append(c, off, len);
					target.lastEndPosMatch = ((this.lastAtLineStart ? ParserRule.AT_LINE_START : 0)
						| (this.lastAtWhitespaceEnd ? ParserRule.AT_WHITESPACE_END : 0)
						| (this.lastAtWordStart ? ParserRule.AT_WORD_START : 0));
					target.lastAtLineStart = false;
					target.lastAtWordStart = false;
					target.lastAtWhitespaceEnd = false;
				}
				else
				{
					target.lastEnd.append(c, off, len);
				}
			}
			else
			{
				if (lastKeyword == null)
					lastKeyword = new StringBuffer();
				lastKeyword.append(c, off, len);
			}
		}

		public String tagName;
		public StringBuffer lastStart;
		public StringBuffer lastEnd;
		public StringBuffer lastKeyword;
		public String lastSetName;
		public String lastEscape;
		public ParserRuleSet lastDelegateSet;
		public String lastNoWordSep = "_";
		public ParserRuleSet rules;
		public byte lastDefaultID = Token.NULL;
		public byte lastTokenID;
		public byte lastMatchType;
		public int termChar = -1;
		public boolean lastNoLineBreak;
		public boolean lastNoWordBreak;
		public boolean lastIgnoreCase = true;
		public boolean lastHighlightDigits;
		public boolean lastAtLineStart;
		public boolean lastAtWhitespaceEnd;
		public boolean lastAtWordStart;
		public int lastStartPosMatch;
		public int lastEndPosMatch;
		public String lastDigitRE;
		public String lastHashChar;
		public String lastHashChars;
	}
}

