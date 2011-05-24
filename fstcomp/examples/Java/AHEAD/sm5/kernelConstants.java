

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// Note: this is the extended version of the sm3 layer.  

// Something that I haven't done before
// is that it is easy to formulate generated code as a string, and
// then convert it into an AST, rather than using code constructors
// mth{..}mth exclusively.  Hopefully, this will turn out to be a
// good decision.  However, be aware that this might pose some problems
// in the future, when constructs from other DSLs are being used as
// statements for transition actions and tests.  If the ASTs for these
// actions have already been annotated, and these annotations will 
// be needed for subsequent transformations, there may be a problem.
// Converting ASTs into strings will lose the annotations.  If we're
// careful that pre-annotating an AST is avoided, we should have no
// problems.

// global information collected during the parse of a state_machine 
// specification is stored in instances of the following classes: 
//
//           sdInfo - one instance per state_machine
//           transInfo - one instance per transition defined
//           stateInfo - one instance per state defined

// during a parse, we're only looking at one state machine at a time.
// the following static members contain the current parse information 
// ($TEqn.kernelConstants.globals().sm4vars.Sm) from which the 
// "inherited" parse information can be derived

// global information includes:
//    Sm                    -- parse info on current state machine
//    ser_directory         -- directory to read .jak and store .ser files 
//    serCache              -- an optimization to avoid reading .ser files
//                          -- more than once.

public class kernelConstants {
    public  sm4data   sm4vars = new  sm4data();
}
