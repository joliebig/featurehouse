/**
 *
 *
 */
public class Structure extends AbstractEnum {
    public static final Structure ROW = new Structure("ROW");
    public static final Structure COL = new Structure("COL");
    public static final Structure BOX = new Structure("BOX");

    public static Structure[] values() {
        return (Structure[]) values0(Structure.class,
                                     new Structure[count(Structure.class)]);
    }

    public static Structure valueOf(final String name) {
        return (Structure) valueOf0(Structure.class, name);
    }

    private Structure(final String name) {
        super(name);
    }
}