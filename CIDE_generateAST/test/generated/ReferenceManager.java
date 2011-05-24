package generated;

import java.util.*;
import cide.greferences.*;

import cide.gast.*;

public class ReferenceManager implements IReferenceManager {
	public final static ReferenceType production = new ReferenceType("Production", new Class[] { NonTerminal.class }, new Class[] { Production.class });
	
	public ReferenceType[] getReferenceTypes() {
		return new ReferenceType[] { production };
	}
}
