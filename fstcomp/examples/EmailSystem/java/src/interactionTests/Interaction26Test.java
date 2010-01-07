package interactionTests;

import junit.framework.TestCase;

public class Interaction26Test extends TestCase {

    public static void test() throws Exception {
        client.common.Net net = new client.common.Net();
        client.client.Client client = new client.client.Client(net, "user");
        postOffice.server.Server hostA = new postOffice.server.Server();
        hostA.init((postOffice.common.Net) net, "hostA", new String[0]);
    }
}
