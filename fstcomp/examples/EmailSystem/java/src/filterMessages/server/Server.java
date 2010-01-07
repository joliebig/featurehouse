package server;

import common.Email;

public class Server {

    private String[] spamHosts;

    public void init(int port, String[] spamHosts) {
        init(port);
        this.spamHosts = spamHosts;
    }

    public void receiveMail(Email msg) {
        if (!filterCheck(msg))
            original(msg);
    }

    private boolean filterCheck(Email msg) {
        for (String s: spamHosts) {
            if (msg.getFrom().endsWith(s))
                return true;
        }
        return false;
    }
}
