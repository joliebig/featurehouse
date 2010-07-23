































try:

    print 'Attempting to connect tray to daemon...'

    proxy_obj = bus.get_object('org.wicd.daemon', '/org/wicd/daemon')

    print 'Success.'

except Exception:

    print 'Daemon not running...'

    misc.PromptToStartDaemon()

    sys.exit(1)

























if __name__ == '__main__':

    if os.getuid() != 0:

        print "Root privileges are required to configure scripts.  Exiting."

        sys.exit(0)

    main(sys.argv)



try:

    print 'Attempting to connect tray to daemon...'

    proxy_obj = bus.get_object('org.wicd.daemon', '/org/wicd/daemon')

    print 'Success.'

except Exception:

    print 'Daemon not running...'

    misc.PromptToStartDaemon()

    sys.exit(1)



if __name__ == '__main__':

    if os.getuid() != 0:

        print "Root privileges are required to configure scripts.  Exiting."

        sys.exit(0)

    main(sys.argv)



