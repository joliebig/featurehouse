

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// stateInfo is instantiated once per state declaration

class stateInfo extends contInfo {
    public String          name; // name of state
    public boolean         nested_state; // is state nested?
    public String          nested_var; // nested variable name
    public  AST_Stmt  exit_action_ast; // exit action
    public  AST_Stmt  enter_action_ast; // enter action
    public  AST_Stmt  prepare_action_ast; // prepare action
    public String          branches_mth_name; // branches method name
    public String          delivery_action; // delivery action
    public  AST_Stmt  otherwise_action_ast; // ast of action
    public  AstNode   alloc_expr_ast; // for nested SMs

    public stateInfo( String name, boolean nested ) {
        inherited            = false;
        this.name            = name;
        nested_state         = nested;
        nested_var           = "nested_" + name;

        String nested_stuff = nested ? 
                nested_var + ".delivery(" +  kernelConstants.globals().sm4vars.Sm.argdecl +
                "); if (!" + nested_var + ".atStop()) return; " 
                : "";
         
        branches_mth_name    = name + "_branches";

        exit_action_ast      =  AST_Stmt.MakeAST( ";" );
        enter_action_ast     =  AST_Stmt.MakeAST( ";" );
        prepare_action_ast   =  AST_Stmt.MakeAST( ";" );
        delivery_action      = "\n" +
              "      if (current_state == " + name + ") { " + nested_stuff +
              name + "_prepare(" +  kernelConstants.globals().sm4vars.Sm.argdecl + "); " +
              branches_mth_name + "(" +  kernelConstants.globals().sm4vars.Sm.argdecl + "); return; }";
        otherwise_action_ast =  AST_Stmt.MakeAST( "otherwise_Default(" +  kernelConstants.globals().sm4vars.Sm.argdecl +
                              ");" );
        alloc_expr_ast = null;
    }

    // used for serialization - throw away anything that is not needed.
    // this object is serialized, and is read in during the processing
    // of parent state machines in inheritance hierarchies.  Not all
    // information is needed -- most is thrown away.
    // REMEMBER: if an AST is to be saved/serialized, it must be
    // Detached from the tree

    public void truncate() {
        inherited = true;
        // leave name
        exit_action_ast = null;
        enter_action_ast = null;
        prepare_action_ast = null;
        // leave branches_mth_name
        // leave delivery_action
        otherwise_action_ast = null;
        alloc_expr_ast = null;
    // leave nested_state 
    // leave nested_var
    }

    public boolean equals( Object x ) {
        if ( x instanceof  stateInfo )
            return ( ( stateInfo ) x ).name.equals( name );
        return false;
    }

    static  stateInfo verifyStateName( String sname, String which,
              AstTokenInterface t ) {
        stateInfo s;

        SmIterator i =  kernelConstants.globals().sm4vars.Sm.StateCont.iterator();
        for ( s = ( stateInfo ) i.firstObj(); 
               s != null; 
               s = ( stateInfo ) i.nextObj() ) {
            if ( s.name.equals( sname ) )
                return s;
        }
        
        AstNode.fatalError( t, Utility.SourceName()+ 
                         "Unrecognized state " + sname + " in " + which );
        return /* should never get here */ null;
    }
 
    // only for debugging

    public void print() {
        System.out.println( "   inherited: " + inherited );
        System.out.println( "   State Name " + name + " ast: " + name );
        System.out.println( "   Exit action: " + exit_action_ast );
        System.out.println( "   Enter action: " + enter_action_ast );
        System.out.println( "   Prepare action: " + prepare_action_ast );
        System.out.println( "   Branches method name " + branches_mth_name );
        System.out.println( "   delivery action: " + delivery_action );
        System.out.println( "   otherwise action: " + otherwise_action_ast );
        System.out.println( "   allocation expr: " + alloc_expr_ast );
        System.out.println( "   nested_state: " + nested_state );
        System.out.println( "   nested_var: " + nested_var );
        System.out.println();
    }

    public String branches_method() {
        transInfo  e;
        String result = "";
        String s = "";
        String terminate = "";

        SmIterator i =  kernelConstants.globals().sm4vars.Sm.TransCont.iterator();
        for ( e = ( transInfo ) i.firstObj(); 
              e != null; 
              e = ( transInfo ) i.nextObj() ) {

            // if an transition begins at the given state...

            if ( e.start.equals( name ) && e.branches!=null )
                s = s + e.branches;
        }

		  String args = kernelConstants.globals().sm4vars.Sm.argdecl;
        if ( kernelConstants.globals().sm4vars.Sm.superSm_name == null || !inherited ) 
		      terminate = "if (starBranches( " + args + " ) ) " + 
                        name + "_otherwise( " +  args + " );";
        else
            terminate = "super." + branches_mth_name + 
                       "( " + args + ");";

        // return null if the state isn't new (i.e., it is inherited)
        // and no new branches have been defined

        if ( inherited && s.equals( "" ) )
            return "";

        return
"\n\n   // methods for state " + name + "\n\n" +
"   void " + branches_mth_name + "( " +  kernelConstants.globals().sm4vars.Sm.pardecl + " ) {\n" + s +
"      " + terminate + "\n" +
"   }";
    }
}
