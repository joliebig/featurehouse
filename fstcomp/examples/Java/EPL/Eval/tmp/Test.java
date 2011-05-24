package tmp;

public class Test {
  static void evaltest(){
    e=new Num(1);
    System.out.println("eval(1) = " + e.eval());
    e=new Neg(new Num(1));
    System.out.println("eval(Neg(1)) = " + e.eval());
    e=new Plus(new Num(1),new Num(2));
    System.out.println("eval(1+2)=" + e.eval());
    e=new Neg(new Plus(new Num(1),new Num(2)));
    System.out.println("eval(-(1+2))=" + e.eval());
  }
  public static void main(  String args[]){
    original(args);
    Test.evaltest();
  }
}
