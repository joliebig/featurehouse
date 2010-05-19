package tmp;

public class Neg implements Exp {
  Exp x;
  Neg(  Exp x){
    this.x=x;
  }
  public String toString(){
    return " -(" + x + ") ";
  }
}
