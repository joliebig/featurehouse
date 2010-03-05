package org.prevayler.implementation.snapshot;
import java.io.IOException;

public class NullSnapshotManager extends GenericSnapshotManager {
  private final String _snapshotAttemptErrorMessage;
  public NullSnapshotManager(  Object newPrevalentSystem,  String snapshotAttemptErrorMessage){
    super(newPrevalentSystem);
    _snapshotAttemptErrorMessage=snapshotAttemptErrorMessage;
  }
  public void writeSnapshot(  Object prevalentSystem,  long version) throws IOException {
    throw new IOException(_snapshotAttemptErrorMessage);
  }
}
