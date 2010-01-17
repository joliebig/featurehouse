package net.sf.jabref;

import java.io.Writer;


public class UnknownEntryType extends BibtexEntryType {

    private String name;
    private String[] fields = new String[0];

    public UnknownEntryType(String name_) {
	name = name_;
    }

    public String getName() {
	return name;
    }

    public String[] getOptionalFields() {
	return fields;
    }
    public String[] getRequiredFields() {
	return fields;
    }


    public String describeRequiredFields() {
	return "unknown";
    }

    public String describeOptionalFields() {
	return "unknown";
    }

    public boolean hasAllRequiredFields(BibtexEntry entry) {
	return true;
    }

    public void save(Writer out) {
    }

}
