package cide.greferences;

import cide.gast.ASTNode;
import cide.gast.ISourceFile;

public interface IResolver {

	/**
	 * resolves a reference. returns the target node of the reference or null if
	 * reference could not be resolved
	 * 
	 * @param type
	 * @param source
	 * @return
	 */
	public ASTNode getReferenceTarget(IReferenceType type, ASTNode source) throws ReferenceResolvingException;

	enum CacheRequirement {
		NoCache/* References can be resolved inside a single file */, CacheAll
		/* the whole project needs to be cached before resolving references */
	};

	public CacheRequirement getCacheRequirement();

	/**
	 * called by the framework after parsing a file to cache information
	 * required for resolving references
	 * 
	 * @param file
	 */
	public void cacheSourceFile(ISourceFile ast);

	/**
	 * called by the framework to clear the whole cache
	 */
	public void clearCache();
}
