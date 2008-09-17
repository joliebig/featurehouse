package com.sleepycat.je.dbi;

public class EnvironmentImpl {
    @MethodObject
    static class EnvironmentImpl_checkLeaks {
	EnvironmentImpl_checkLeaks(EnvironmentImpl _this) {
	    this._this = _this;
	}

	protected EnvironmentImpl _this;

	protected boolean clean;

	protected StatsConfig statsConfig;

	protected LockStats lockStat;

	protected TransactionStats txnStat;

	protected TransactionStats.Active[] active;

	void execute__wrappee__CheckLeaks() throws DatabaseException {
	    if (!_this.configManager
		    .getBoolean(EnvironmentParams.ENV_CHECK_LEAKS)) {
		return;
	    }
	    clean = true;
	    this.hook313();
	    this.hook312();
	    assert clean : "Lock, transaction, or latch left behind at environment close";
	}

	void execute() throws DatabaseException {
	    t.in(Thread.currentThread().getStackTrace()[1].toString());
	    execute__wrappee__CheckLeaks();
	    t.out(Thread.currentThread().getStackTrace()[1].toString());
	}

	protected void hook312__wrappee__CheckLeaks() throws DatabaseException {
	}

	protected void hook312() throws DatabaseException {
	    t.in(Thread.currentThread().getStackTrace()[1].toString());
	    hook312__wrappee__CheckLeaks();
	    t.out(Thread.currentThread().getStackTrace()[1].toString());
	}

	protected void hook313__wrappee__CheckLeaks() throws DatabaseException {
	}

	protected void hook313() throws DatabaseException {
	    t.in(Thread.currentThread().getStackTrace()[1].toString());
	    hook313__wrappee__CheckLeaks();
	    t.out(Thread.currentThread().getStackTrace()[1].toString());
	}

    }

    private void checkLeaks__wrappee__CheckLeaks() throws DatabaseException {
	new EnvironmentImpl_checkLeaks(this).execute();
    }

    private void checkLeaks() throws DatabaseException {
	t.in(Thread.currentThread().getStackTrace()[1].toString());
	checkLeaks__wrappee__CheckLeaks();
	t.out(Thread.currentThread().getStackTrace()[1].toString());
    }

    protected void hook311__wrappee__CheckLeaks() throws DatabaseException {
    }

    protected void hook311() throws DatabaseException {
	t.in(Thread.currentThread().getStackTrace()[1].toString());
	hook311__wrappee__CheckLeaks();
	t.out(Thread.currentThread().getStackTrace()[1].toString());
    }

    protected void hook325__wrappee__CheckLeaks(StringBuffer errors)
	    throws DatabaseException {
	try {
	    checkLeaks();
	    this.hook311();
	} catch (DatabaseException DBE) {
	    errors.append("\nException performing validity checks: ");
	    errors.append(DBE.toString()).append("\n");
	}
	original(errors);
    }

    protected void hook325(StringBuffer errors) throws DatabaseException {
	t.in(Thread.currentThread().getStackTrace()[1].toString());
	hook325__wrappee__CheckLeaks();
	t.out(Thread.currentThread().getStackTrace()[1].toString());
    }

    private Tracer t = new Tracer();

    public Tracer getTracer() {
	return t;
    }

}
