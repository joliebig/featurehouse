

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

//---------   Constructor Declaration and constructor rewrites -----------

/* this layer deals with the propagation of constructors for programs
   produced by Mixin.  Recall that Java doesn't support the inheritance
   of constructors.  Well, for refinement chains, we have to propagate
   constructors, and in fact, in the general case, we have to propagate
   contructors both UP and DOWN the refinement chain.  Here's the
   essential algorithm.  We will create a list of classes, or whatever
   that we have seen so far.  Call it previousTypeDecls.  We will use
   this list to propagate constructors UP the refinement chain.

      note: this happens in the following circumstance.  suppose there
      are 2 classes in a super-class-subclass relationship.  A layer
      extends both classes with a constructor, where the constructor
      of the subclass invokes "super" to call its superclass constructor.
      well, for Mixin implementations, we have to propagate this
      constructor upwards so that the super call works properly.

   This must be a 2-pass algorithm.  The first pass collects 
   constructors to be propagated.  The second pass does the usual
   reduction AND propagates these constructors.

   The first-pass algorithm (harvestConstructors) is:

   (0) set previousTypeDecls = empty;
       set inheritedCons = empty; (this is the list of constructors we have
                                   seen so far in a Mixin-produced file)
   foreach Mixin M:
       (1) set M.inheritedCons = inheritedCons;
       (2) add constructors of M into inheritedCons;
       (3) if there are SUPER invocations, create a constructor for it
           and propagate it up the refinement hierarchy, adding it
           to the P.inheritedCons for each ancestor of M

   The second-pass algorithm (reduce2java)

   foreach mixin M do:

      add M.inheritedCons to M (making sure that the constructor
      names are correct).
*/
 
public class conTable extends Hashtable {

    public void add( String key,  ConDecl c,  AstTokenInterface t ) {
        if ( kernelConstants.globals().j2jbase.SourceDeclSeen && containsKey( key ) )
            AstNode.error( t, "multiple constructors have the same signature: (" 
                        + key + ")" );
        put( key, c );
    }

    public void union( String key,  ConDecl c ) {
        put( key, c );
    }
}
