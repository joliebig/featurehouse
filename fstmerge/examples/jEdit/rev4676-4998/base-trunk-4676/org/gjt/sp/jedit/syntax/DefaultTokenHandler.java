

package org.gjt.sp.jedit.syntax;


public class DefaultTokenHandler implements TokenHandler
{
	
	
	public void init()
	{
		lastToken = firstToken = null;
	} 

	
	
	public Token getTokens()
	{
		return firstToken;
	} 

	
	
	public void handleToken(byte id, int offset, int length,
		TokenMarker.LineContext context)
	{
		Token token = createToken(id,offset,length,context);
		if(token != null)
			addToken(token,context);
	} 

	
	protected Token firstToken, lastToken;

	
	protected ParserRuleSet getParserRuleSet(TokenMarker.LineContext context)
	{
		while(context != null)
		{
			if(!context.rules.isBuiltIn())
				return context.rules;

			context = context.parent;
		}

		return null;
	} 

	
	protected Token createToken(byte id, int offset, int length,
		TokenMarker.LineContext context)
	{
		return new Token(id,offset,length,getParserRuleSet(context));
	} 

	
	protected void addToken(Token token, TokenMarker.LineContext context)
	{
		if(firstToken == null)
		{
			firstToken = lastToken = token;
		}
		else
		{
			lastToken.next = token;
			lastToken = lastToken.next;
		}
	} 

	
}
