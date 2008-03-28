package cide.languages;

import java.io.InputStream;

public interface ILanguageExtension {
	/**
	 * returns a parser for the specific language for the given input stream
	 * 
	 * @param inputStream
	 *            input for the parser
	 * @return the parser itself for the input stream
	 */
	ILanguageParser getParser(InputStream inputStream);

	/**
	 * returns a pretty printer for this language implemented as AST visitor.
	 * can only be used once.
	 * 
	 * @return the pretty printer as AST visitor
	 */
	ILanguagePrintVisitor getPrettyPrinter();

	/**
	 * returns the language validator. the validator is an interface that
	 * provides all required information about validating the specific language,
	 * including a resolver for references.
	 * 
	 * @return
	 */
	ILanguageValidator getValidator();
}
