// adds and sets option for equation file

class Main {
  static boolean equationFormat = false;

  static void marqueeAdditions() {
     original();
     System.out.println( "                -e equation file format" );
  }

  static boolean processOptions( char o ) {
     if ( o == 'e' ) {
	     equationFormat = true;
             Gui.equations = "equation";
             return true;
	  }
	  return original(o);
  }

  static void setEquationFormat( boolean v ) {
     equationFormat = v;
     if (v) Gui.equations="equation";
     else   Gui.equations="equations";
  }
}
