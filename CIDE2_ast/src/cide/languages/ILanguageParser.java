package cide.languages;

import cide.gast.ISourceFile;
import cide.gparser.ParseException;

public interface ILanguageParser {
	ISourceFile getRoot() throws ParseException;
}
