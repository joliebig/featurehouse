class Store {
  int read() { 
    if(state == State.ON) { return original(); } 
    else { return 0; }
  }
  void set(int v) { 
    if(state == State.ON) { original(v); }
  }
}WE