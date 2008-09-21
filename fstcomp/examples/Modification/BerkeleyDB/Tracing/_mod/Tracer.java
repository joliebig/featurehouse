import java.util.List;
import java.util.LinkedList;

public class Tracer {
    
    
    private List<String> log = new LinkedList<String>();
    
    
    public Tracer() {

    }
    
    public List<String> getLog{
	return log;
    }
    
    public in(String s){
	log.add("<IN> " + s);
    }

    public out(String s){
	log.add("<OUT> " + s);
    }
}
