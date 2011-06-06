

package bsh;
import java.util.*;


public interface NameSource 
{
	public String [] getAllNames();
	public void addNameSourceListener( NameSource.Listener listener );

	public static interface Listener {
		public void nameSourceChanged( NameSource src );
		
	}
}
