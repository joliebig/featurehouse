package net.sf.jabref;

import java.util.Comparator;


public class FieldComparator implements Comparator<BibtexEntry> {

	String field;

	boolean isNameField, isTypeHeader, isYearField, isMonthField;

	int multiplier;

	public FieldComparator(String field) {
		this(field, false);
	}

	public FieldComparator(String field, boolean reversed) {
		this.field = field;
		multiplier = reversed ? -1 : 1;
		isTypeHeader = field.equals(GUIGlobals.TYPE_HEADER);

		isNameField = (field.equals("author") || field.equals("editor"));
		isYearField = field.equals("year");
		isMonthField = field.equals("month");
	}

	public int compare(BibtexEntry e1, BibtexEntry e2) {
		Object f1, f2;

		if (isTypeHeader) {
			
			f1 = e1.getType().getName();
			f2 = e2.getType().getName();
		} else {

			
			
			f1 = e1.getField(field);
			f2 = e2.getField(field);
		}

		
		int localMultiplier = multiplier;
		if (isMonthField)
			localMultiplier = -localMultiplier;
		
		
		if (f1 == null)
			return f2 == null ? 0 : localMultiplier;

		if (f2 == null)
			return -localMultiplier;

		
		if (isNameField) {
			if (f1 != null)
				f1 = AuthorList.fixAuthorForAlphabetization((String) f1);
			if (f2 != null)
				f2 = AuthorList.fixAuthorForAlphabetization((String) f2);
		} else if (isYearField) {
			
			f1 = Util.toFourDigitYear((String) f1);
			f2 = Util.toFourDigitYear((String) f2);
		} else if (isMonthField) {
			
			f1 = new Integer(Util.getMonthNumber((String)f1));			
			f2 = new Integer(Util.getMonthNumber((String)f2));
		}

		int result = 0;
		if ((f1 instanceof Integer) && (f2 instanceof Integer)) {
			result = -(((Integer) f1).compareTo((Integer) f2));
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

	
	public String getFieldName() {
		return field;
	}
}
