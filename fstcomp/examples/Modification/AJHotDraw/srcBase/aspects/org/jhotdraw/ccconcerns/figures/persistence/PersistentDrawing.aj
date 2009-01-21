package org.jhotdraw.ccconcerns.figures.persistence;

import org.jhotdraw.util.Storable;
import org.jhotdraw.framework.Drawing;

/**
 * Aspect to add the persistence concern to the Drawing
 * classes.
 * 
 * @author Marius M.
 *
 */
public aspect PersistentDrawing {

    declare parents: Drawing extends Storable;
    
}
