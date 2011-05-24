

public class ThisPre {

   // refines "this" to be " ( ThisName ) this "

   public void reduce2java( AstProperties props ) {
      String ThisName = (String) props.getProperty( "ThisName" );
		if (ThisName == null)
		   AstNode.fatalError( "ThisName property not set" );
		props.print( getComment() + "((" + ThisName + ") this)");
   }
}
