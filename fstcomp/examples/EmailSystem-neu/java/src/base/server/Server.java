package server;

import common.Email;
import java.util.List;
import java.util.LinkedList;


public class Server {

    private final static String EMAIL_DOMAIN_SUFFIX_REGEX = "@.*";

    private String hostname;

    public void init() {
    }

    public void receiveMail(Email msg) {

    }

    public void start(int smtpPort, int pop3Port) {
        Thread pop3Worker = new Thread({
            public static void run
        ServerSocket smtpSocket = new ServerSocket(smtpPort);
        ServerSocket pop3Socket = new ServerSocket(pop3Port);
        Thread smtpWorker = new Thread({
        while (true) {
            Socket smtpClient = smtpSocket.accept();
            ObjectInputStream = new 
        });
    }

    public void sendMail(Email msg) {

    }

    public String getHostname() {
        return hostname;
    }
}
