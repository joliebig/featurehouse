package net.sf.jabref;

import net.sf.jabref.gui.MainTableFormat;

import java.util.Comparator;


public class FieldComparator implements Comparator<BibtexEntry> {

	private String[] field;
    private String fieldName;

    boolean isNameField, isTypeHeader, isYearField, isMonthField, isNumeric;

	int multiplier;

	public FieldComparator(String field) {
		this(field, false);
	}

	public FieldComparator(String field, boolean reversed) {
        this.fieldName = field;
        this.field = field.split(MainTableFormat.COL_DEFINITION_FIELD_SEPARATOR);
		multiplier = reversed ? -1 : 1;
		isTypeHeader = this.field[0].equals(GUIGlobals.TYPE_HEADER);
        isNameField = (this.field[0].equals("author")
                || this.field[0].equals("editor"));
		isYearField = this.field[0].equals("year");
		isMonthField = this.field[0].equals("month");
        isNumeric = BibtexFields.isNumeric(this.field[0]);
    }

	public int compare(BibtexEntry e1, BibtexEntry e2) {
		Object f1, f2;

		if (isTypeHeader) {
			
			f1 = e1.getType().getName();
			f2 = e2.getType().getName();
		} else {

			
			
			f1 = getField(e1);
			f2 = getField(e2);
		}

		
		int localMultiplier = multiplier;
		if (isMonthField)
			localMultiplier = -localMultiplier;
		
		
		if (f1 == null)
			return f2 == null ? 0 : localMultiplier;

		if (f2 == null)
			return -localMultiplier;

		
		if (isNameField) {
			f1 = AuthorList.fixAuthorForAlphabetization((String) f1);
			f2 = AuthorList.fixAuthorForAlphabetization((String) f2);
		} else if (isYearField) {
			
			f1 = Util.toFourDigitYear((String) f1);
			f2 = Util.toFourDigitYear((String) f2);
		} else if (isMonthField) {
			
			f1 = new Integer(Util.getMonthNumber((String)f1));			
			f2 = new Integer(Util.getMonthNumber((String)f2));
		}

        if (isNumeric) {
            Integer i1 = null, i2 = null;
            try {
                i1 = Integer.parseInt((String)f1);
            } catch (NumberFormatException ex) {
                
            }

            try {
                i2 = Integer.parseInt((String)f2);
            } catch (NumberFormatException ex) {
                
            }

            if (i2 != null && i1 != null) {
                
                f1 = i1;
                f2 = i2;
            } else if (i1 != null) {
                
                
                f1 = i1;
                f2 = new Integer(i1.intValue()+1);
            } else if (i2 != null) {
                
                
                f2 = i2;
                f1 = new Integer(i2.intValue()+1);
            }
            
        }

        int result = 0;
		if ((f1 instanceof Integer) && (f2 instanceof Integer)) {
			result = (((Integer) f1).compareTo((Integer) f2));
		} else if (f2 instanceof Integer) {
			Integer f1AsInteger = new Integer(f1.toString());
			result = -((f1AsInteger).compareTo((Integer) f2));
		} else if (f1 instanceof Integer) {
			Integer f2AsInteger = new Integer(f2.toString());
			result = -(((Integer) f1).compareTo(f2AsInteger));
		} else {
			String ours = ((String) f1).toLowerCase(), theirs = ((String) f2).toLowerCase();
            result = ours.compareTo(theirs);
		}

		return result * localMultiplier;
	}

    private Object getField(BibtexEntry entry) {
        for (int i = 0; i < field.length; i++) {
            Object o = entry.getField(field[i]);
            if (o != null)
                return o;
        }
        return null;
    }

    
	public String getFieldName() {
		return fieldName;
	}
}
