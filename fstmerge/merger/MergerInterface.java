package merger;

import de.ovgu.cide.fstgen.ast.FSTTerminal;

public interface MergerInterface {
	void merge(FSTTerminal node) throws ContentMergeException;
}
