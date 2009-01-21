package org.jhotdraw.ccconcerns;

/** 
 * Aspect precedence.
 * @author Gijs Peek
 */
public aspect Precedence {
	// Contract enforcement has precedence over all other concerns
	declare precedence: *..*Contract*, *;
}
