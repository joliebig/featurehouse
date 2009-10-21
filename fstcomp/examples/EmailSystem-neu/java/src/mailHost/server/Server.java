package server;

import java.util.List;
import java.util.LinkedList;

public class Server {

    private List<String> accountants;
    private List<Email> receivedBuffer;

    public void init(int port) {
        original(port);
        receivedBuffer = new LinkedList<Email>();
        accountants = new LinkedList<String>();
    }

    public List<Email> fetchMail(String username) {
        List<Email> userMail = new LinkedList<Email>();
        for (Email email: receivedBuffer) {
            if (email.getTo().matches(username + EMAIL_DOMAIN_SUFFIX_REGEX)) {
                userMail.add(email);
            }
        }
        receivedBuffer.removeAll(userMail);
        return userMail;
    }

    public void receiveMail(Email msg) {
        for (String accountant: accountants) {
            if (msg.getTo().matches(accountant + EMAIL_DOMAIN_SUFFIX_REGEX)) {
                receivedBuffer.add(msg);
                return;
            }
        }
        sendMail(createNonDeliveryNotification(msg));
    }

    private Email createNonDeliveryNotification(Email msg) {
        return new Email("postmaster", msg.getFrom(), "", "user does not exist on host", "original message: \n" + msg.getSubject() + "\n" + msg.getBody());
    }

    public void createAccount(String username) {
        if (!accountants.contains(username))
            accountants.add(username);
    }

    public void deleteAccount(String username) {
        if (accountants.contains(username))
            accountants.remove(username);
    }
}
