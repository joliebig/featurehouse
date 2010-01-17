
package net.sf.jabref; 

import java.io.*; 
import java.util.*; 

import java.util.Iterator; 
import java.util.TreeMap; 

public abstract  class  BibtexEntryType implements  Comparable ,  Comparable<BibtexEntryType> {
	

    public static final BibtexEntryType OTHER =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Other";
            }

            public String[] getOptionalFields()
            {
                return new String[0];
            }

            public String[] getRequiredFields()
            {
                return new String[0];
            }


            public String describeRequiredFields()
            {
                return "";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return true;
            }
        };

	


    public static final BibtexEntryType ARTICLE =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Article";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "number", "month", "eid", "note"
                };
            }

            public String[] getRequiredFields()
            {
                return new String[]
                {
                    "author", "title", "journal", "year", "volume", "pages"
                };
            }

            public String describeRequiredFields()
            {
                return "AUTHOR, TITLE, JOURNAL and YEAR";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return entry.allFieldsPresent(new String[]
                    {
                        "author", "title", "journal", "year", "bibtexkey", "volume", "pages"
                    });
            }
        };

	

    public static final BibtexEntryType BOOKLET =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Booklet";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "author", "howpublished", "lastchecked", "address", "month", "year", "note"
                };
            }

            public String[] getRequiredFields()
            {
                return new String[]
                {
                    "title"
                };
            }

            public String describeRequiredFields()
            {
                return "TITLE";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return entry.allFieldsPresent(new String[]
                    {
                        "title", "bibtexkey"
                    });
            }
        };

	


   public static final BibtexEntryType INBOOK =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Inbook";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "volume", "number", "pages", "series", "type", "address", "edition",
		    "month", "note"
                };
            }

            public String[] getRequiredFields()
            {
                return new String[]
                {
                    "chapter", "pages", "title", "publisher", "year", "editor",
		    "author"
                };
            }

            public String describeRequiredFields()
            {
                return "TITLE, CHAPTER and/or PAGES, PUBLISHER, YEAR, and an "
		    +"EDITOR and/or AUTHOR";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return entry.allFieldsPresent(new String[]
                    {
                        "title", "publisher", "year", "bibtexkey"
                    }) &&
		    (((entry.getField("author") != null) ||
		      (entry.getField("editor") != null)) &&
		     ((entry.getField("chapter") != null) ||
		      (entry.getField("pages") != null)));
            }
        };

	

    public static final BibtexEntryType BOOK =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Book";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "volume", "number", "pages", "series", "address", "edition", "month",
                    "note"
                };
            }

            public String[] getRequiredFields()
            {
                return new String[]
                {
                    "title", "publisher", "year", "editor", "author"
                };
            }

            public String describeRequiredFields()
            {
                return "TITLE, PUBLISHER, YEAR, and an EDITOR and/or AUTHOR";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return entry.allFieldsPresent(new String[]
                    {
                        "title", "publisher", "year", "bibtexkey"
                    }) &&
                ((entry.getField("author") != null) ||
                (entry.getField("editor") != null));
            }
        };

	


    public static final BibtexEntryType INCOLLECTION =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Incollection";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "editor", "volume", "number", "series", "type", "chapter",
		    "pages", "address", "edition", "month", "note"
                };
            }

            public String[] getRequiredFields()
            {
                return new String[]
                {
                    "author", "title", "booktitle", "publisher", "year"
                };
            }

            public String describeRequiredFields()
            {
                return "AUTHOR, TITLE, BOOKTITLE, PUBLISHER and YEAR";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return entry.allFieldsPresent(new String[]
                    {
			"author", "title", "booktitle", "publisher", "year",
			"bibtexkey"

                    });
            }
        };

	

    public static final BibtexEntryType CONFERENCE =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Conference";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "editor", "volume", "number", "series", "pages",
		    "address", "month", "organization", "publisher", "note"
                };
            }

            public String[] getRequiredFields()
            {
                return new String[]
                {
                    "author", "title", "booktitle", "year"
                };
            }

            public String describeRequiredFields()
            {
                return "AUTHOR, TITLE, BOOKTITLE and YEAR";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return entry.allFieldsPresent(new String[]
                    {
			"author", "title", "booktitle", "year" , "bibtexkey"
                    });
            }
        };

	

    public static final BibtexEntryType INPROCEEDINGS =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Inproceedings";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "editor", "volume", "number", "series", "pages",
		    "address", "month", "organization", "publisher", "note"
                };
            }

            public String[] getRequiredFields()
            {
                return new String[]
                {
                    "author", "title", "booktitle", "year"
                };
            }

            public String describeRequiredFields()
            {
                return "AUTHOR, TITLE, BOOKTITLE and YEAR";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return entry.allFieldsPresent(new String[]
                    {
			"author", "title", "booktitle", "year" , "bibtexkey"
                    });
            }
        };

	

    public static final BibtexEntryType PROCEEDINGS =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Proceedings";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "editor", "volume", "number", "series", "address",
		    "publisher", "note", "month", "organization"
                };
            }

            public String[] getRequiredFields()
            {
                return new String[]
                {
                    "title", "year"
                };
            }

            public String describeRequiredFields()
            {
                return "TITLE and YEAR";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return entry.allFieldsPresent(new String[]
                    {
			"title", "year", "bibtexkey"
                    });
            }
        };

	


    public static final BibtexEntryType MANUAL =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Manual";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "author", "organization", "address", "edition",
		    "month", "year", "note"
                };
            }

            public String[] getRequiredFields()
            {
                return new String[]
                {
                    "title"
                };
            }

            public String describeRequiredFields()
            {
                return "TITLE";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return entry.allFieldsPresent(new String[]
                    {
                        "title", "bibtexkey"
                    });
            }
        };

	

    public static final BibtexEntryType TECHREPORT =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Techreport";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "type", "number", "address", "month", "note"
                };
            }

            public String[] getRequiredFields()
            {
                return new String[]
                {
                    "author", "title", "institution", "year"
                };
            }

            public String describeRequiredFields()
            {
                return "AUTHOR, TITLE, INSTITUTION and YEAR";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return entry.allFieldsPresent(new String[]
                    {
			"author", "title", "institution", "year",
			"bibtexkey"
                    });
            }
        };

	


    public static final BibtexEntryType MASTERSTHESIS =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Mastersthesis";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "type", "address", "month", "note"
                };
            }

            public String[] getRequiredFields()
            {
                return new String[]
                {
                    "author", "title", "school", "year"
                };
            }

            public String describeRequiredFields()
            {
                return "AUTHOR, TITLE, SCHOOL and YEAR";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return entry.allFieldsPresent(new String[]
                    {
                        "author", "title", "school", "year", "bibtexkey"
                    });
            }
        };

	


    public static final BibtexEntryType PHDTHESIS =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Phdthesis";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "type", "address", "month", "note"
                };
            }

            public String[] getRequiredFields()
            {
                return new String[]
                {
                    "author", "title", "school", "year"
                };
            }

            public String describeRequiredFields()
            {
                return "AUTHOR, TITLE, SCHOOL and YEAR";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return entry.allFieldsPresent(new String[]
                    {
                        "author", "title", "school", "year", "bibtexkey"
                    });
            }
        };

	

    public static final BibtexEntryType UNPUBLISHED =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Unpublished";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "month", "year"
                };
            }

            public String[] getRequiredFields()
            {
                return new String[]
                {
                    "author", "title", "note"
                };
            }

            public String describeRequiredFields()
            {
                return "AUTHOR, TITLE and NOTE";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
                return entry.allFieldsPresent(new String[]
                    {
			"author", "title", "note", "bibtexkey"
                    });
            }
        };

	


    public static final BibtexEntryType MISC =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Misc";
            }

            public String[] getOptionalFields()
            {
                return new String[]
                {
                    "author", "title", "howpublished", "month", "year", "note"
                };
            }

            public String[] getRequiredFields()
            {
                return null;
            }

            public String describeRequiredFields()
            {
                return "None";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
		return entry.allFieldsPresent(new String[]
                    {
			"bibtexkey"
                    });
            }
        };

	

    
    public static final BibtexEntryType TYPELESS =
        new BibtexEntryType()
        {
            public String getName()
            {
                return "Typeless";
            }

            public String[] getOptionalFields()
            {
                return null;
            }

            public String[] getRequiredFields()
            {
                return null;
            }

            public String describeRequiredFields()
            {
                return "None";
            }

            public boolean hasAllRequiredFields(BibtexEntry entry)
            {
		return false;
           }
        };

	


    public abstract String getName();


	

    


	

    public abstract String[] getOptionalFields();


	

    public abstract String[] getRequiredFields();


	

    public String[] getGeneralFields() {
        return new String[]
	    {"crossref", "keywords", "doi", "url",
	     "citeseerurl", "pdf", "abstract", "comment"};
    }


	

    public abstract String describeRequiredFields();


	

    public abstract boolean hasAllRequiredFields(BibtexEntry entry);


	


    public String[] getUtilityFields(){
        return new String[] {"search" } ;
    }


	


    public boolean isRequired(String field) {
	String[] req = getRequiredFields();
	if (req == null) return false;
	for (int i=0; i<req.length; i++)
	    if (req[i].equals(field)) return true;
	return false;
    }


	

    public boolean isOptional(String field) {
	String[] opt = getOptionalFields();
	if (opt == null) return false;
	for (int i=0; i<opt.length; i++)
	    if (opt[i].equals(field)) return true;
	return false;
    }


	

    

	
    

	
    static {
	
	ALL_TYPES.put("article", ARTICLE);
	ALL_TYPES.put("inbook", INBOOK);
	ALL_TYPES.put("book", BOOK);
	ALL_TYPES.put("booklet", BOOKLET);
	ALL_TYPES.put("incollection", INCOLLECTION);
        ALL_TYPES.put("conference", CONFERENCE);
	ALL_TYPES.put("inproceedings", INPROCEEDINGS);
	ALL_TYPES.put("proceedings", PROCEEDINGS);
	ALL_TYPES.put("manual", MANUAL);
	ALL_TYPES.put("mastersthesis", MASTERSTHESIS);
	ALL_TYPES.put("phdthesis", PHDTHESIS);
	ALL_TYPES.put("techreport", TECHREPORT);
	ALL_TYPES.put("unpublished", UNPUBLISHED);
        ALL_TYPES.put("misc", MISC);
        ALL_TYPES.put("other", OTHER);

	
	
	STANDARD_TYPES = (TreeMap)ALL_TYPES.clone();
    }

	

    
    public static BibtexEntryType getType(String name) {
	
	Object o = ALL_TYPES.get(name.toLowerCase());
	if (o == null)
	    return null;
	else return (BibtexEntryType)o;
    }


	

    
    public static BibtexEntryType getStandardType(String name) {
	
	Object o = STANDARD_TYPES.get(name.toLowerCase());
	if (o == null)
	    return null;
	else return (BibtexEntryType)o;
    }


	

    
    public static void removeType(String name) {
	
	String nm = name.toLowerCase();
        
	ALL_TYPES.remove(nm);
        
	if (STANDARD_TYPES.get(nm) != null) {
	    
	    
	    ALL_TYPES.put(nm, STANDARD_TYPES.get(nm));
	}

    }


	

    
    public static void loadCustomEntryTypes(JabRefPreferences prefs) {
	int number = 0;
	CustomEntryType type;
	while ((type = prefs.getCustomEntryType(number)) != null) {
	    ALL_TYPES.put(type.getName().toLowerCase(), type);
	    number++;
	}
    }


	

    
    public static void saveCustomEntryTypes(JabRefPreferences prefs) {
	Iterator<String> i=ALL_TYPES.keySet().iterator();
	int number = 0;
	
	while (i.hasNext()) {
	    Object o=ALL_TYPES.get(i.next());
	    if (o instanceof CustomEntryType) {
		
		prefs.storeCustomEntryType((CustomEntryType)o, number);
		number++;
	    }
	}
	
	
	
	prefs.purgeCustomEntryTypes(number);
    }


	

    public int compareTo(BibtexEntryType o) {
	return getName().compareTo(o.getName());
    }

	

    public static TreeMap<String, BibtexEntryType> ALL_TYPES = new TreeMap<String, BibtexEntryType>();

	
    public static TreeMap<String, BibtexEntryType> STANDARD_TYPES = new TreeMap<String, BibtexEntryType>();

	
    static {
	
	ALL_TYPES.put("article", ARTICLE);
	ALL_TYPES.put("inbook", INBOOK);
	ALL_TYPES.put("book", BOOK);
	ALL_TYPES.put("booklet", BOOKLET);
	ALL_TYPES.put("incollection", INCOLLECTION);
        ALL_TYPES.put("conference", CONFERENCE);
	ALL_TYPES.put("inproceedings", INPROCEEDINGS);
	ALL_TYPES.put("proceedings", PROCEEDINGS);
	ALL_TYPES.put("manual", MANUAL);
	ALL_TYPES.put("mastersthesis", MASTERSTHESIS);
	ALL_TYPES.put("phdthesis", PHDTHESIS);
	ALL_TYPES.put("techreport", TECHREPORT);
	ALL_TYPES.put("unpublished", UNPUBLISHED);
        ALL_TYPES.put("misc", MISC);
        ALL_TYPES.put("other", OTHER);

	
	
	STANDARD_TYPES = new TreeMap<String, BibtexEntryType>(ALL_TYPES);
    }


}
