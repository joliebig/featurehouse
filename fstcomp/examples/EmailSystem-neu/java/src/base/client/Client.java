package client;

import common.Email;
import common.Net;

public class Client {

    private Net net;

    private String username;

    public Client(Net net, String username) {
        this.net = net;
        this.username = username;
    }

    public void sendMail(String hostname, Email msg) {
        net.getPop3Service(hostname).sendMail(msg);
    }

}
