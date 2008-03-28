package cide.languages;

import cide.gast.IASTVisitor;

public interface ILanguagePrintVisitor extends IASTVisitor {
	String getResult();
}
