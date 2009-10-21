package server;

import common.Email;

public class Server {

    public void receiveMail(Email msg) {
        original(deAnonymify(msg));
    }

    public void sendMail(Email msg) {
        original(anonymify(msg));
    }

    private Email anonymify(Email msg) {
        // TODO
        return msg;
    }

    private Email deAnonymify(Email msg) {
        // TODO
        return msg;
    }
}
