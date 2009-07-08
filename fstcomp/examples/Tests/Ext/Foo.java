public class Foo {
	public void access() {
		//int a = this.a;
		int b = this.b;
		int c = this.c;
		int d = this.d;
	}
	public int bar() {
		return foo(new B());
	}
}