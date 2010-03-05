package org.prevayler.implementation;
import org.prevayler.implementation.snapshot.GenericSnapshotManager;
public class PrevalentSystemGuard {
  public void takeSnapshot(  GenericSnapshotManager snapshotManager) throws IOException {
synchronized (this) {
      if (_prevalentSystem == null) {
        throw new Error("Prevayler is no longer allowing snapshots due to an Error thrown from an earlier transaction.");
      }
synchronized (_prevalentSystem) {
        snapshotManager.writeSnapshot(_prevalentSystem,_systemVersion);
      }
    }
  }
}
