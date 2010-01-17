

package net.sf.jabref.groups;

import java.util.Map;

import javax.swing.undo.AbstractUndoableEdit;

import net.sf.jabref.BibtexDatabase;
import net.sf.jabref.BibtexEntry;
import net.sf.jabref.SearchRule;


public abstract class AbstractGroup {
	
	protected String m_name;

	
	protected int m_context = INDEPENDENT;

	public AbstractGroup(String name, int context) {
		m_name = name;
		setHierarchicalContext(context);
	}

	
	public static final int INDEPENDENT = 0;
	
	public static final int REFINING = 1;
	
	public static final int INCLUDING = 2;

	
	protected static final char QUOTE_CHAR = '\\';

	
	protected static final String SEPARATOR = ";";

	
	public abstract SearchRule getSearchRule();

	
	public static AbstractGroup fromString(String s, BibtexDatabase db,
			int version) throws Exception {
		if (s.startsWith(KeywordGroup.ID))
			return KeywordGroup.fromString(s, db, version);
		if (s.startsWith(AllEntriesGroup.ID))
			return AllEntriesGroup.fromString(s, db, version);
		if (s.startsWith(SearchGroup.ID))
			return SearchGroup.fromString(s, db, version);
		if (s.startsWith(ExplicitGroup.ID))
			return ExplicitGroup.fromString(s, db, version);
		return null; 
	}

	
	public final String getName() {
		return m_name;
	}

	
	public final void setName(String name) {
		m_name = name;
	}

	
	public abstract boolean supportsAdd();

	
	public abstract boolean supportsRemove();

	
	public abstract AbstractUndoableEdit add(BibtexEntry[] entries);

	
	public abstract AbstractUndoableEdit remove(BibtexEntry[] entries);

	
	public abstract boolean contains(Map<String, String> searchOptions, BibtexEntry entry);

	
	public abstract boolean contains(BibtexEntry entry);

	
	public boolean containsAny(BibtexEntry[] entries) {
		for (int i = 0; i < entries.length; ++i)
			if (contains(entries[i]))
				return true;
		return false;
	}

	
	public boolean containsAll(BibtexEntry[] entries) {
		for (int i = 0; i < entries.length; ++i)
			if (!contains(entries[i]))
				return false;
		return true;
	}

	
	public abstract boolean isDynamic();

	
	public void setHierarchicalContext(int context) {
		if (context != INDEPENDENT && context != REFINING
				&& context != INCLUDING)
			return;
		m_context = context;
	}
	
	
	public int getHierarchicalContext() {
		return m_context;
	}
	
	
	public abstract String getDescription();

	
	public abstract AbstractGroup deepCopy();

	
	public abstract String getShortDescription();

	
	
	

	
        
        
        public void refreshForNewDatabase(BibtexDatabase db) {
            
            
        }
}
