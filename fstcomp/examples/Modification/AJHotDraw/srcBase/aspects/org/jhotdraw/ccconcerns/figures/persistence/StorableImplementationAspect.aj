package org.jhotdraw.ccconcerns.figures.persistence;

import org.jhotdraw.util.Storable;
import org.jhotdraw.util.StorableInput;
import org.jhotdraw.util.StorableOutput;

/*
 * This aspect forces implementations of the Storable.read and
 * Storable.write methods inside the persistence package
 */
public aspect StorableImplementationAspect {
	pointcut storableMethodImplementation():
		(execution(public void Storable.read(StorableInput))
		||execution(public void Storable.write(StorableOutput)))
		&& !execution(* Object.*(..));

	declare warning: storableMethodImplementation()
	&& !within(org.jhotdraw.ccconcerns.figures.persistence.*):
		"All methods declared in Storable should be implemented in the org.jhotdraw.ccconcerns.figures.persistence package";

	declare precedence: *, storableMethodImplementation*;
}
