class Store {
  enum State { ON, OFF }
  State state = State.OFF;
  void flip() { 
	  state = (state == State.ON ? State.OFF : State.ON);
  }
}