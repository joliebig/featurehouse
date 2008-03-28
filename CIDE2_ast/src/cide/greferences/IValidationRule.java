package cide.greferences;

import java.util.HashMap;
import java.util.Set;

import cide.features.IASTColorProvider;

public interface IValidationRule {
	final static int Ignore = -1; // during handling only
	final static int Warning = 0; // during handling only
	final static int Error = 1;

	int getErrorSeverity();

	String getErrorMessage();

	IReferenceType[] getRequiredReferences();

	void validate(IASTColorProvider colorProvider,
			HashMap<IReferenceType, Set<IReference>> references, IValidationErrorCallback errorCallback);

	String getName();
}
