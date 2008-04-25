package examples;

class Foo extends X implements B, X, Y {
	abstract static protected int i() { before(); original(); after(); }
	final protected int j() {}
	
	int a;
	int b = 1;
	int c;
	int d = 0;
	int e, t = 9;
	int f = 1, tt = 99;
	int g, ttt = 999;
	int h = 0, tttt = 9999;
}
