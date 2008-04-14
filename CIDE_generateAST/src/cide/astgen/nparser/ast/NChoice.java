package cide.astgen.nparser.ast;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import cide.astgen.nparser.visitor.NVisitor;

public class NChoice {
	public final List<NAbstractValue> units;
	private final List<NAnnotation> annotations = new ArrayList<NAnnotation>();

	NProduction production;

	public String optName = null;

	public NChoice(NProduction production, List<NAbstractValue> units) {
		this.production = production;
		this.units = units;
	}

	public NChoice(NProduction production) {
		this(production, new ArrayList<NAbstractValue>());
	}

	public void accept(NVisitor visitor) {
		if (visitor.visit(this))
			for (NAbstractValue p : units)
				p.accept(visitor);
		visitor.postVisit(this);
	}

	public String genClassname() {
		if (production.choices.size() <= 1)
			return production.getName();

		if (optName != null && optName.length() > 0)
			return optName.toString();

		return production.getName() + (getChoiceIdx() + 1);
	}

	public String genSuperclass() {
		if (production.choices.size() > 1)
			return production.getName();
		return null;
	}

	public NProduction getParent() {
		return production;
	}

	public List<NAbstractValue> getUnits() {
		return Collections.unmodifiableList(units);
	}

	/**
	 * returns the position of this choice inside the production
	 */
	public int getChoiceIdx() {
		return production.choices.indexOf(this);
	}

	/**
	 * only used for the List annotation &LI specifies whether any child uses
	 * this annotation
	 */
	public boolean isList() {
		for (NAbstractValue u : units)
			if (u.isListElement())
				return true;
		return false;
	}

	/**
	 * only used for the List annotation &LI
	 * 
	 * @return type of the list. error if does not contain consistent type
	 */
	public String getListType() {
		String result = null;
		for (NAbstractValue u : units)
			if (u.isListElement()) {
				assert result == null
						|| result.equals(u.genVariablePlainType());
				result = u.genVariablePlainType();
			}
		return result;
	}

	public String getListAccessMethod() {
		for (NAbstractValue u : units)
			if (u.isListElement()) {
				return u.genAccessMethod();
			}
		return null;
	}

	public List<String> collectAnnotationValues(String string) {
		ArrayList<String> result = new ArrayList<String>();
		for (NAbstractValue unit : units) {
			result.addAll(unit.getAnnotationValues(string));
		}
		return result;
	}

	public void addAnnotation(NAnnotation annotation) {
		annotations.add(annotation);
	}

	public NAnnotation findAnnotation(String annotationName) {
		for (NAnnotation a : annotations)
			if (a.getName().equals(annotationName))
				return a;
		for (NAnnotation a : production.getAnnotations())
			if (a.getName().equals(annotationName))
				return a;
		return null;
	}

	public String findAnnotationValue(String annotationName, String valueKey) {
		NAnnotation annoation = findAnnotation(annotationName);
		if (annoation != null)
			return annoation.values.get(valueKey);
		return null;
	}

	public List<NAnnotation> getAnnotations() {
		ArrayList<NAnnotation> allAnnotations = new ArrayList<NAnnotation>(
				annotations);
		allAnnotations.addAll(production.getAnnotations());
		return Collections.unmodifiableList(allAnnotations);
	}

	@Override
	public String toString() {
		String r = "";
		for (NAbstractValue u : getUnits())
			r += u.getName() + " ";
		r += ":: " + genClassname();
		return r;
	}

}
