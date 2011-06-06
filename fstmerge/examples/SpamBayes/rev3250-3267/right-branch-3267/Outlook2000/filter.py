ACTION_MOVE, ACTION_COPY, ACTION_NONE = None, None, None
def filter_message(msg, mgr, all_actions=True):
    config = mgr.config.filter
    prob = mgr.score(msg)
    prob_perc = prob * 100
    if prob_perc >= config.spam_threshold:
        disposition = "Yes"
        attr_prefix = "spam"
        if all_actions:
            msg.c = mgr.bayes_message.PERSISTENT_SPAM_STRING
    elif prob_perc >= config.unsure_threshold:
        disposition = "Unsure"
        attr_prefix = "unsure"
        if all_actions:
            msg.c = mgr.bayes_message.PERSISTENT_UNSURE_STRING
    else:
        disposition = "No"
        attr_prefix = "ham"
        if all_actions:
            msg.c = mgr.bayes_message.PERSISTENT_HAM_STRING
    ms = mgr.message_store
    try:
        global ACTION_NONE, ACTION_COPY, ACTION_MOVE
        if ACTION_NONE is None: ACTION_NONE = _("Untouched").lower()
        if ACTION_COPY is None: ACTION_COPY = _("Copied").lower()
        if ACTION_MOVE is None: ACTION_MOVE = _("Moved").lower()
        try:
            if config.save_spam_info:
                for i in range(3):
                    try:
                        msg.SetField(mgr.config.general.field_score_name, prob)
                        if all_actions:
                            msg.RememberMessageCurrentFolder()
                        msg.Save()
                        break
                    except ms.ObjectChangedException:
                        mgr.LogDebug(1, "Got ObjectChanged changed - " \
                                        "trying again...")
                        msg.dirty = False
                        msg.mapi_object = None # cause it to be re-fetched.
                else:
                    mgr.LogDebug(0, "Got ObjectChanged 3 times in a row - " \
                                    "giving up!")
                    msg.dirty = False
        except ms.ReadOnlyException:
            mgr.LogDebug(1, "Message is read-only - could not save Spam score")
            msg.dirty = False
        except ms.MsgStoreException as details:
            print("Unexpected MAPI error saving the spam score for", msg)
            print(details)
            msg.dirty = False
        if all_actions and attr_prefix is not None:
            folder_id = getattr(config, attr_prefix + "_folder_id")
            action = getattr(config, attr_prefix + "_action").lower()
            mark_as_read = getattr(config, attr_prefix + "_mark_as_read")
            if mark_as_read:
                msg.SetReadState(True)
            if action == ACTION_NONE:
                mgr.LogDebug(1, "Not touching message '%s'" % msg.subject)
            elif action == ACTION_COPY:
                try:
                    dest_folder = ms.GetFolder(folder_id)
                except ms.MsgStoreException:
                    print("ERROR: Unable to open the folder to Copy the " \
                          "message - this message was not copied")
                else:
                    msg.CopyToReportingError(mgr, dest_folder)
                    mgr.LogDebug(1, "Copied message '%s' to folder '%s'" \
                                 % (msg.subject, dest_folder.GetFQName()))
            elif action == ACTION_MOVE:
                try:
                    dest_folder = ms.GetFolder(folder_id)
                except ms.MsgStoreException:
                    print("ERROR: Unable to open the folder to Move the " \
                          "message - this message was not moved")
                else:
                    msg.MoveToReportingError(mgr, dest_folder)
                    mgr.LogDebug(1, "Moved message '%s' to folder '%s'" \
                                 % (msg.subject, dest_folder.GetFQName()))
            else:
                raise RuntimeError("Eeek - bad action '%r'" % (action,))
        if all_actions:
            mgr.stats.RecordClassification(prob)
            mgr.classifier_data.message_db.store_msg(msg)
            mgr.classifier_data.dirty = True
            mgr.classifier_data.SavePostIncrementalTrain()
        return disposition
    except:
        print("Failed filtering message!", msg)
        import traceback
        traceback.print_exc()
        return "Failed"
def filter_folder(f, mgr, config, progress):
    only_unread = config.only_unread
    only_unseen = config.only_unseen
    all_actions = config.action_all
    dispositions = {}
    field_name = mgr.config.general.field_score_name
    for message in f.GetMessageGenerator():
        if progress.stop_requested():
            break
        progress.tick()
        if only_unread and message.GetReadState() or \
           only_unseen and message.GetField(field_name) is not None:
            continue
        try:
            disposition = filter_message(message, mgr, all_actions)
        except:
            import traceback
            print("Error filtering message '%s'" % (message,))
            traceback.print_exc()
            disposition = "Error"
        dispositions[disposition] = dispositions.get(disposition, 0) + 1
    return dispositions
def filterer(mgr, config, progress):
    config = config.filter_now
    if not config.folder_ids:
        progress.error(_("You must specify at least one folder"))
        return
    progress.set_status(_("Counting messages"))
    num_msgs = 0
    for f in mgr.message_store.GetFolderGenerator(config.folder_ids, config.include_sub):
        num_msgs += f.count
    progress.set_max_ticks(num_msgs+3)
    dispositions = {}
    for f in mgr.message_store.GetFolderGenerator(config.folder_ids, config.include_sub):
        progress.set_status(_("Filtering folder '%s'") % (f.name))
        this_dispositions = filter_folder(f, mgr, config, progress)
        for key, val in list(this_dispositions.items()):
            dispositions[key] = dispositions.get(key, 0) + val
        if progress.stop_requested():
            return
    err_text = ""
    if "Error" in dispositions:
        err_text = _(" (%d errors)") % dispositions["Error"]
    dget = dispositions.get
    text = _("Found %d spam, %d unsure and %d good messages%s") % \
           (dget("Yes",0), dget("Unsure",0), dget("No",0), err_text)
    progress.set_status(text)
def main():
    print("Sorry - we don't do anything here any more")
if __name__ == "__main__":
    main()
