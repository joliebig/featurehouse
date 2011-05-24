import sys
import traceback
from win32com.mapi import mapi
def been_trained_as_ham(msg):
    if msg.t is None:
        return False
    return msg.t == False
def been_trained_as_spam(msg):
    if msg.t is None:
        return False
    return msg.t == True
def train_message(msg, is_spam, cdata):
    from spambayes.tokenizer import tokenize
    cdata.message_db.load_msg(msg)
    was_spam = msg.t
    if was_spam == is_spam:
        return False    
    stream = msg.GetEmailPackageObject()
    if was_spam is not None:
        cdata.bayes.unlearn(tokenize(stream), was_spam)
    cdata.bayes.learn(tokenize(stream), is_spam)
    msg.t = is_spam
    cdata.message_db.store_msg(msg)
    cdata.dirty = True
    return True
def untrain_message(msg, cdata):
    from spambayes.tokenizer import tokenize
    stream = msg.GetEmailPackageObject()
    cdata.message_db.load_msg(msg)
    if been_trained_as_spam(msg):
        assert not been_trained_as_ham(msg), "Can't have been both!"
        cdata.bayes.unlearn(tokenize(stream), True)
        cdata.message_db.remove_msg(msg)
        cdata.dirty = True
        return True
    if been_trained_as_ham(msg):
        assert not been_trained_as_spam(msg), "Can't have been both!"
        cdata.bayes.unlearn(tokenize(stream), False)
        cdata.message_db.remove_msg(msg)
        cdata.dirty = True
        return False
    return None
def train_folder(f, isspam, cdata, progress):
    num = num_added = 0
    for message in f.GetMessageGenerator():
        if progress.stop_requested():
            break
        progress.tick()
        try:
            if train_message(message, isspam, cdata):
                num_added += 1
        except:
            print("Error training message '%s'" % (message,))
            traceback.print_exc()
        num += 1
    print("Checked", num, "in folder", f.name, "-", num_added, "new entries found.")
def real_trainer(classifier_data, config, message_store, progress):
    progress.set_status(_("Counting messages"))
    num_msgs = 0
    for f in message_store.GetFolderGenerator(config.training.ham_folder_ids, config.training.ham_include_sub):
        num_msgs += f.count
    for f in message_store.GetFolderGenerator(config.training.spam_folder_ids, config.training.spam_include_sub):
        num_msgs += f.count
    progress.set_max_ticks(num_msgs+3)
    for f in message_store.GetFolderGenerator(config.training.ham_folder_ids, config.training.ham_include_sub):
        progress.set_status(_("Processing good folder '%s'") % (f.name,))
        train_folder(f, 0, classifier_data, progress)
        if progress.stop_requested():
            return
    for f in message_store.GetFolderGenerator(config.training.spam_folder_ids, config.training.spam_include_sub):
        progress.set_status(_("Processing spam folder '%s'") % (f.name,))
        train_folder(f, 1, classifier_data, progress)
        if progress.stop_requested():
            return
    progress.tick()
    if progress.stop_requested():
        return
    progress.set_max_ticks(1)
    progress.set_status(_("Writing the database..."))
    classifier_data.Save()
def trainer(mgr, config, progress):
    rebuild = config.training.rebuild
    rescore = config.training.rescore
    if not config.training.ham_folder_ids and not config.training.spam_folder_ids:
        progress.error(_("You must specify at least one spam or one good folder"))
        return
    if rebuild:
        import os, manager
        bayes_base = os.path.join(mgr.data_directory, "$sbtemp$default_bayes_database")
        mdb_base = os.path.join(mgr.data_directory, "$sbtemp$default_message_database")
        ManagerClass = manager.GetStorageManagerClass()
        db_manager = ManagerClass(bayes_base, mdb_base)
        classifier_data = manager.ClassifierData(db_manager, mgr)
        classifier_data.InitNew()
    else:
        classifier_data = mgr.classifier_data
    if rescore:
        stages = (_("Training"), .3), (_("Saving"), .1), (_("Scoring"), .6)
    else:
        stages = (_("Training"), .9), (_("Saving"), .1)
    progress.set_stages(stages)
    real_trainer(classifier_data, config, mgr.message_store, progress)
    if progress.stop_requested():
        return
    if rebuild:
        assert mgr.classifier_data is not classifier_data
        mgr.AdoptClassifierData(classifier_data)
        classifier_data = mgr.classifier_data
        mgr.LogDebug(1, "Session:" + "\r\n".join(\
            mgr.stats.GetStats(session_only=True)))
        mgr.LogDebug(1, "Total:" + "\r\n".join(mgr.stats.GetStats()))
        mgr.stats.Reset()
        mgr.stats.ResetTotal(permanently=True)
    progress.tick()
    if rescore:
        config = mgr.config.filter_now
        config.only_unread = False
        config.only_unseen = False
        config.action_all = False
        config.folder_ids = mgr.config.training.ham_folder_ids + mgr.config.training.spam_folder_ids
        config.include_sub = mgr.config.training.ham_include_sub or mgr.config.training.spam_include_sub
        import filter
        filter.filterer(mgr, mgr.config, progress)
    bayes = classifier_data.bayes
    progress.set_status(_("Completed training with %d spam and %d good messages") % (bayes.nspam, bayes.nham))
def main():
    print("Sorry - we don't do anything here any more")
if __name__ == "__main__":
    main()
