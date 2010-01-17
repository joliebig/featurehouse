package net.sf.jabref.export.layout.format; 

import net.sf.jabref.export.layout.AbstractParamLayoutFormatter; 
import net.sf.jabref.gui.FileListTableModel; 
import net.sf.jabref.gui.FileListEntry; 
import net.sf.jabref.Globals; 
import net.sf.jabref.GUIGlobals; 
import net.sf.jabref.Util; 

import java.util.*; 
import java.io.File; 
import java.io.IOException; 


public  class  WrapFileLinks  extends AbstractParamLayoutFormatter {
	


    private String fileType = null;

	
    private List<FormatEntry> format = null;

	
    private Map<String, String> replacements = new HashMap<String, String>();

	

    public void setArgument(String arg) {
        String[] parts = parseArgument(arg);
        format = parseFormatString(parts[0]);
        if ((parts.length > 1) && (parts[1].trim().length() > 0))
            fileType = parts[1];
        if (parts.length > 2) {
            for (int i = 2; i < parts.length-1; i+=2) {
                replacements.put(parts[i], parts[i+1]);
            }
        }
    }


	

    public String format(String field) {
        StringBuilder sb = new StringBuilder();

        
        FileListTableModel tableModel = new FileListTableModel();
        if (field == null)
            return "";
        tableModel.setContent(field);

        int piv = 1; 
        for (int i = 0; i < tableModel.getRowCount(); i++) {
            FileListEntry flEntry = tableModel.getEntry(i);
            
            if ((fileType == null) || flEntry.getType().getName().toLowerCase().equals(fileType)) {

                for (FormatEntry entry : format) {
                    switch (entry.getType()) {
                        case STRING:
                            sb.append(entry.getString());
                            break;
                        case ITERATION_COUNT:
                            sb.append(String.valueOf(piv));
                            break;
                        case FILE_PATH:
                            if (flEntry.getLink() == null)
                                break;

                            String dir;
                            
                            
                            
                            
                            if (Globals.prefs.fileDirForDatabase != null)
                                dir = Globals.prefs.fileDirForDatabase;
                            else
                                dir = Globals.prefs.get(GUIGlobals.FILE_FIELD + "Directory");

                            File f = Util.expandFilename(flEntry.getLink(), new String[]{dir});
                            
                            if (f != null) {
                                try {
                                    sb.append(replaceStrings(f.getCanonicalPath()));
                                } catch (IOException ex) {
                                    ex.printStackTrace();
                                    sb.append(replaceStrings(f.getPath()));
                                }
                            } else {
                                sb.append(replaceStrings(flEntry.getLink()));
                            }

                            break;
                        case RELATIVE_FILE_PATH:
                            if (flEntry.getLink() == null)
                                break;

                            
                            sb.append(replaceStrings(flEntry.getLink()));

                            break;
                        case FILE_EXTENSION:
                            if (flEntry.getLink() == null)
                                break;
                            int index = flEntry.getLink().lastIndexOf('.');
                            if ((index >= 0) && (index < flEntry.getLink().length() - 1))
                                sb.append(replaceStrings(flEntry.getLink().substring(index + 1)));
                            break;
                        case FILE_TYPE:
                            sb.append(replaceStrings(flEntry.getType().getName()));
                            break;
                        case FILE_DESCRIPTION:
                            sb.append(replaceStrings(flEntry.getDescription()));
                            break;
                    }
                }

                piv++; 
            }
        }

        return sb.toString();
    }


	


    protected String replaceStrings(String text) {
        for (Iterator<String> i=replacements.keySet().iterator(); i.hasNext();) {
            String from = i.next();
            String to = replacements.get(from);
            text = text.replaceAll(from, to);
        }
        return text;
        
    }


	


    
    

	

    
    final static Map<Character, Integer> ESCAPE_SEQ = new HashMap<Character, Integer>();

	

    static {
        ESCAPE_SEQ.put('i', ITERATION_COUNT);
        ESCAPE_SEQ.put('p', FILE_PATH);
        ESCAPE_SEQ.put('f', FILE_TYPE);
        ESCAPE_SEQ.put('x', FILE_EXTENSION);
        ESCAPE_SEQ.put('d', FILE_DESCRIPTION);
    }

	

    
    public List<FormatEntry> parseFormatString(String format) {
        List<FormatEntry> l = new ArrayList<FormatEntry>();
        StringBuilder sb = new StringBuilder();
        boolean escaped = false;
        for (int i = 0; i < format.length(); i++) {
            char c = format.charAt(i);
            if (!escaped) {
                
                if (c == '\\')
                    escaped = true;
                else
                    sb.append(c);
            } else {
                escaped = false; 
                
                if (c == '\\') {
                    
                    sb.append(c);
                } else if (ESCAPE_SEQ.containsKey(c)) {
                    
                    
                    if (sb.length() > 0) {
                        l.add(new FormatEntry(sb.toString()));
                        
                        sb = new StringBuilder();
                    }
                    l.add(new FormatEntry(ESCAPE_SEQ.get(c)));
                } else {
                    
                    sb.append('\\');
                    sb.append(c);
                    
                }
            }
        }
        
        if (sb.length() > 0) {
            l.add(new FormatEntry(sb.toString()));
        }

        return l;
    }


	


    
    protected  class  FormatEntry {
		

        private int type;

		
        private String string = null;

		

        public FormatEntry(int type) {
            this.type = type;
        }


		

        public FormatEntry(String value) {
            this.type = STRING;
            this.string = value;
        }


		

        public int getType() {
            return type;
        }


		

        public String getString() {
            return string;
        }



	}

	


    
    public static final int STRING = 0, ITERATION_COUNT = 1, FILE_PATH = 2, FILE_TYPE = 3,
            FILE_EXTENSION = 4, FILE_DESCRIPTION = 5, RELATIVE_FILE_PATH = 6;

	

    static {
        ESCAPE_SEQ.put('i', ITERATION_COUNT);
        ESCAPE_SEQ.put('p', FILE_PATH);
        ESCAPE_SEQ.put('r', RELATIVE_FILE_PATH);
        ESCAPE_SEQ.put('f', FILE_TYPE);
        ESCAPE_SEQ.put('x', FILE_EXTENSION);
        ESCAPE_SEQ.put('d', FILE_DESCRIPTION);
    }


}
