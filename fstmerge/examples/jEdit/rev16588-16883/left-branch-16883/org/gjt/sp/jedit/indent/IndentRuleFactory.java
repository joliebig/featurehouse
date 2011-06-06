

package org.gjt.sp.jedit.indent;

import java.util.regex.PatternSyntaxException;

public class IndentRuleFactory
{
	public static IndentRule indentNextLines(String regexp)
		throws PatternSyntaxException
	{
		return new RegexpIndentRule(regexp,
			null,
			new IndentAction.Increase(),
			null,false);
	}

	public static IndentRule indentNextLine(String regexp)
		throws PatternSyntaxException
	{
		return new RegexpIndentRule(regexp,
			new IndentAction.Decrease(),
			new IndentAction.Increase(),
			null,true);
	}

	public static IndentRule unindentThisLine(String regexp)
		throws PatternSyntaxException
	{
		return new RegexpIndentRule(regexp,
			null,
			new IndentAction.Increase(),
			new IndentAction.Decrease(),
			false);
	}

	public static IndentRule unindentNextLines(String regexp)
		throws PatternSyntaxException
	{
		return new RegexpIndentRule(regexp,
			null,
			new IndentAction.Decrease(),
			null,
			false);
	}

	public static IndentRule indentOpenBracket(char bracket)
		throws PatternSyntaxException
	{
		return new OpenBracketIndentRule(bracket,true);
	}

	public static IndentRule indentCloseBracket(char bracket)
		throws PatternSyntaxException
	{
		return new CloseBracketIndentRule(bracket,true);
	}

	public static IndentRule unalignedOpenBracket(char bracket)
		throws PatternSyntaxException
	{
		return new OpenBracketIndentRule(bracket,false);
	}

	public static IndentRule unalignedCloseBracket(char bracket)
		throws PatternSyntaxException
	{
		return new CloseBracketIndentRule(bracket,false);
	}
}
