import sys, os
if __name__=='__main__':
    try:
        import spambayes.Version
    except ImportError:
        sys.path.append(os.path.abspath(os.path.join(os.path.dirname(sys.argv[0]), "..", "..")))
    try:
        import manager
    except ImportError:
        sys.path.append(os.path.abspath(os.path.join(os.path.dirname(sys.argv[0]), "..")))
    import manager, win32con
    mgr = manager.GetManager()
    from dialogs import ShowDialog, ShowWizard
    idd = "IDD_MANAGER"
    if len(sys.argv)>1:
        idd = sys.argv[1]
    if idd=='IDD_WIZARD':
        ShowWizard(0, mgr, idd)
    else:
        ShowDialog(0, mgr, mgr.config, idd)
    if "-d" in sys.argv:
        print("Dumping(but not saving) new manager configuration:")
        print(mgr.options.display())
        print("-- end of configuration --")
    mgr.Close()
