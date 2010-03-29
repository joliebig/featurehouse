package org.prevayler;
import java.util.Date;

/** 
 * The same as TransactionWithQuery except it does not throw Exception when executed.
 * @see TransactionWithQuery
 */
public interface SureTransactionWithQuery extends TransactionWithQuery {
  /** 
 * The same as TransactionWithQuery.execute(Object, Date) except it does not throw Exception when executed.
 * @see TransactionWithQuery#executeAndQuery(Object,Date)
 */
  public Object executeAndQuery(  Object prevalentSystem,  Date executionTime);
}
