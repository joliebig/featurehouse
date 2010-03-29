// representation of a clause in the propositional formula
public class clause
{
    node n = null;            // node representation of clause
    String value = null;      // status of clause
    String originalStr = null;   // original, non-simplified clause
    int unknowns = 0;         // used to determine unit-open vs non-unit-open

    public clause(node n, String value, String originalStr)
    {
        this.n = n;
        this.value = value;
        this.originalStr = originalStr;
    }
}