"""Package for interacting with VM folders.
Design notes go here.
Use ZODB to store training data and classifier.
The spam and ham data are culled from sets of folders.  The actual
tokenized messages are stored in a training database.  When the folder
changes, the training data is updated.
- Updates are incremental.
- Changes to a folder are detected based on mtime and folder size.
- The contents of the folder are keyed on message-id.
- If a message is removed from a folder, it is removed from training data.
"""
