package tmp;

public class Test {
  static void printtest(){
    e=new Num(3);
    System.out.println("print(3) = " + e);
    e=new Neg(new Num(5));
    System.out.println("print(Neg(5)) = " + e);
    e=new Plus(new Num(5),new Num(7));
    System.out.println("print(5+7) = " + e);
  }
  public static void main(  String args[]){
    Test.printtest();
    original(args);
  }
}
