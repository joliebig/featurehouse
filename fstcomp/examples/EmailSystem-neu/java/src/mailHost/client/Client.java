package client;

import java.util.List;

public class Client {

    public List<Email> fetchMail() {
        return net.getPop3Service(username).fetchMail(username);
    }

    public void createAccount(String hostname)  {
        net.getPop3Service(hostname).createAccount(username);
    }

    public void deleteAccount(String hostname)  {
        net.getPop3Service(hostname).deleteAccount(username);
    }
}
