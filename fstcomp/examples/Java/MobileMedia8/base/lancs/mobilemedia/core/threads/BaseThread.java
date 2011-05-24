package lancs.mobilemedia.core.threads;
import de.ovgu.cide.jakutil.*;
/** 
 * Base Thread class. Most commands will execute within a thread (to avoid screen
 * contention and hardware interrupts). 
 * At the moment, there is no useful functionality beyond what Runnable already provides.
 * But we may wish to add some parent features in later.
 */
public class BaseThread implements Runnable {
  /** 
 * Constructor
 */
  public BaseThread(){
    System.out.println("BaseThread:: 0 Param Constructor used: Using default values");
  }
  /** 
 * Start the thread running
 */
  public void run(){
    System.out.println("Starting BaseThread::run()");
    System.out.println("Finishing Baseathread::run()");
  }
}
