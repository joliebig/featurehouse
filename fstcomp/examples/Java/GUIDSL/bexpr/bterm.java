public class bterm extends node {
    public String name;

    public     bterm( String n ) {
        name = n;
    }

    public     node klone() {
        return new bterm( name );
    }

    public node simplify() {
        return this;
    }
    public String toString() {
        return name;
    }

    public String cnf2String() {
        return name;
    }

    public     node cnf() {
        return this;
    }
}
