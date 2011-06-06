

package org.gjt.sp.jedit.bsh;


public interface NameSource
{
    public String [] getAllNames();
    public void addNameSourceListener( NameSource.Listener listener );

    public static interface Listener {
        public void nameSourceChanged( NameSource src );
        
    }
}
