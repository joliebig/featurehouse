package cide.greferences;

public class UnsupportedReferenceTypeException extends
		ReferenceResolvingException {

	public UnsupportedReferenceTypeException(IReferenceType type) {
		super("Unsupported Reference Type: " + type);
	}

}
