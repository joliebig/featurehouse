package net.sf.jabref.gui; 

import java.util.HashMap; 
import java.util.Hashtable; 
import java.util.Vector; 

import net.sf.jabref.AuthorList; 
import net.sf.jabref.BasePanel; 
import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.BibtexFields; 
import net.sf.jabref.GUIGlobals; 
import net.sf.jabref.Globals; 
import net.sf.jabref.SearchRuleSet; 
import net.sf.jabref.Util; 
import ca.odell.glazedlists.gui.TableFormat; 
import ca.odell.glazedlists.matchers.Matcher; 


public  class  MainTableFormat implements  TableFormat<BibtexEntry> {
	

    
    
    
    public static final String COL_DEFINITION_FIELD_SEPARATOR = "/";

	

    public static final String[]
            PDF = {"pdf", "ps"}
    ,
    URL_ = {"url", "doi"}
    ,
    CITESEER = {"citeseerurl"},
    FILE = {GUIGlobals.FILE_FIELD};

	

    BasePanel panel;

	

    private String[][] columns;

	 
    public int padleft = -1;

	 
    
    private HashMap<Integer, String[]> iconCols = new HashMap<Integer, String[]>();

	
    int[][] nameCols = null;

	
    boolean namesAsIs, abbr_names, namesNatbib, namesFf, namesLf, namesLastOnly, showShort;

	

    public MainTableFormat(BasePanel panel) {
        this.panel = panel;
    }


	

    public int getColumnCount() {
        return padleft + columns.length;
    }


	

    public String getColumnName(int col) {
        if (col == 0) {
            return GUIGlobals.NUMBER_COL;
        } else if (getIconTypeForColumn(col) != null) {
            return "";
        }
        else 
        {
            String[] fld = columns[col - padleft];
            StringBuilder sb = new StringBuilder();
            for (int i=0; i<fld.length; i++) {
                if (i > 0)
                    sb.append('/');
                String disName = BibtexFields.getFieldDisplayName(fld[i]);
                if (disName != null)
                    sb.append(disName);
                else
                    sb.append(Util.nCase(fld[i]));
            }
            return sb.toString();
          
        }
        
    }


	

    
    public String[] getIconTypeForColumn(int col) {
        Object o = iconCols.get(new Integer(col));
        if (o != null)
            return (String[]) o;
        else
            return null;
    }


	

    
    public int getColumnIndex(String colName) {
        for (int i=0; i<columns.length; i++) {
            
            if (columns[i][0].equalsIgnoreCase(colName))
                return i+padleft;
        }
        return -1;
    }


	

    public Object getColumnValue(BibtexEntry be, int col) {
        Object o = null;
        String[] iconType = getIconTypeForColumn(col); 
        if (col == 0) {
            o = "#";
        }

        else if (iconType != null) {
            int hasField = -1;
            for (int i = iconType.length - 1; i >= 0; i--)
                if (hasField(be, iconType[i]))
                    hasField = i;
            if (hasField < 0)
                return null;

            
            if (iconType[hasField].equals(GUIGlobals.FILE_FIELD)) {
                o = FileListTableModel.getFirstLabel(be.getField(GUIGlobals.FILE_FIELD));
            } else
                o = GUIGlobals.getTableIcon(iconType[hasField]);
        } else {
            String[] fld = columns[col - padleft];
            
            int j = 0;
            for (int i = 0; i < fld.length; i++) {
                if (fld[i].equals(GUIGlobals.TYPE_HEADER))
                    o = be.getType().getName();
                else
                    o = be.getField(fld[i]);
                if (o != null) {
                    j = i;
                    break;
                }
            }

            for (int i = 0; i < nameCols.length; i++) {
                if ((col - padleft == nameCols[i][0]) && (nameCols[i][1] == j)) {
                    return formatName(o);
                }
            }


        }

        return o;
    }


	

    
    public Object formatName(Object o) {
        if (o == null) {
            return null;
        }
        if (namesAsIs) return o;
        if (namesNatbib) o = AuthorList.fixAuthor_Natbib((String) o);
        else if (namesLastOnly) o = AuthorList.fixAuthor_lastNameOnlyCommas((String) o, false);
        else if (namesFf) o = AuthorList.fixAuthor_firstNameFirstCommas((String) o, abbr_names, false);
        else if (namesLf) o = AuthorList.fixAuthor_lastNameFirstCommas((String) o, abbr_names, false);
        return o;
    }


	

    public boolean hasField(BibtexEntry be, String field) {
        
        
        return ((be != null) && (be.getField(field) != null));
    }


	

    public void updateTableFormat() {

        
        String[] colSettings = Globals.prefs.getStringArray("columnNames");
        columns = new String[colSettings.length][];
        for (int i=0; i<colSettings.length; i++) {
            String[] fields = colSettings[i].split(COL_DEFINITION_FIELD_SEPARATOR);
            columns[i] = new String[fields.length];
            for (int j = 0; j < fields.length; j++) {
                columns[i][j] = fields[j];
            }
        }
        
        
        showShort = Globals.prefs.getBoolean("showShort");        
        namesNatbib = Globals.prefs.getBoolean("namesNatbib");    
        namesLastOnly = Globals.prefs.getBoolean("namesLastOnly");
        namesAsIs = Globals.prefs.getBoolean("namesAsIs");
        abbr_names = Globals.prefs.getBoolean("abbrAuthorNames"); 
        namesFf = Globals.prefs.getBoolean("namesFf");
        namesLf = !(namesAsIs || namesFf || namesNatbib || namesLastOnly); 

        
        
        iconCols.clear();
        int coln = 1;
        if (Globals.prefs.getBoolean("fileColumn"))
            iconCols.put(coln++, FILE);
        if (Globals.prefs.getBoolean("pdfColumn"))
            iconCols.put(coln++, PDF);
        if (Globals.prefs.getBoolean("urlColumn"))
            iconCols.put(coln++, URL_);
        if (Globals.prefs.getBoolean("citeseerColumn"))
            iconCols.put(coln++, CITESEER);

        
        padleft = 1 + iconCols.size();

        
        
        
        
        
        Vector<int[]> tmp = new Vector<int[]>(2, 1);
        for (int i = 0; i < columns.length; i++) {
            for (int j = 0; j < columns[i].length; j++) {
                if (columns[i][j].equals("author")
                    || columns[i][j].equals("editor")) {
                    tmp.add(new int[] {i, j});
                }
            }
        }
        nameCols = new int[tmp.size()][];
        for (int i = 0; i < nameCols.length; i++) {
            nameCols[i] = tmp.elementAt(i);
        }
    }


	

    public boolean isIconColumn(int col) {
        return (getIconTypeForColumn(col) != null);
    }


	



    static  class  NoSearchMatcher implements  Matcher<BibtexEntry> {
		
        public boolean matches(BibtexEntry object) {
            return true;
        }



	}

	

    static  class  SearchMatcher implements  Matcher<BibtexEntry> {
		
        private SearchRuleSet ruleSet;

		
        private Hashtable<String, String> searchOptions;

		

        public SearchMatcher(SearchRuleSet ruleSet, Hashtable<String, String> searchOptions) {
            this.ruleSet = ruleSet;
            this.searchOptions = searchOptions;
        }


		
        public boolean matches(BibtexEntry entry) {
            int result = ruleSet.applyRule(searchOptions, entry);
            return result > 0;
        }



	}


}
