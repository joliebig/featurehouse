

package org.gjt.sp.jedit.indent;


public abstract class BracketIndentRule implements IndentRule
{

	public BracketIndentRule(char openBracket, char closeBracket)
	{
		this.openBracket = openBracket;
		this.closeBracket = closeBracket;
	}

	protected IndentContext.Brackets getBrackets(IndentContext ctx, int offset)
	{
		return ctx.getBrackets(offset, openBracket, closeBracket);
	}

	protected IndentContext.Brackets getBrackets(IndentContext ctx, CharSequence line)
	{
		return ctx.getBrackets(line, openBracket, closeBracket);
	}

	public String toString()
	{
		return getClass().getName() + "[" + openBracket + ","
			+ closeBracket + "]";
	}

	protected char openBracket, closeBracket;
}
