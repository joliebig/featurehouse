 class  Store {
	
  int value = 0;

	
  private int  read__wrappee__Switch  () { return value; }

	
  private int read() { 
    if(state == State.ON) { return read__wrappee__Switch(); } 
    else { return 0; }
  }

	
  private void  set__wrappee__Switch  (int v) { value = v; }

	
  void set(int v) { 
    if(state == State.ON) { set__wrappee__Switch(v); }
  }

	
   enum  State { ON ,  OFF}

	
  State state = State.OFF;

	
  void flip() { 
	  state = (state == State.ON ? State.OFF : State.ON);
  }


}
