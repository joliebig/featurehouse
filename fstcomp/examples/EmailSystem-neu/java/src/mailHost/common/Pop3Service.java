package common;

import java.util.List;

public interface Pop3Service {

    public void createAccount(String username);

    public void deleteAccount(String username);

    public List<Email> fetchMail(String username);

}
