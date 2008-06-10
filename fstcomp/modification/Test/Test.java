package modification.Test;
import java.util.regex.Pattern;

public class Test {
    public static void main(String args[]) {
	System.out.println(Pattern.matches("a(.)*", "aaaaab"));
    }
}
