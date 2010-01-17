
package net.sf.jabref;

public class KeyCollisionException extends RuntimeException
{
    public KeyCollisionException()
    {
        super();
    }

    public KeyCollisionException(String msg)
    {
        super(msg);
    }

    public KeyCollisionException(String msg, Throwable exception)
    {
        super(msg, exception);
    }

    public KeyCollisionException(Throwable exception)
    {
        super(exception);
    }
}
