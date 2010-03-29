package org.prevayler.foundation.network;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

public class ObjectSocketImpl implements ObjectSocket {
  private final Socket _socket;
  private final ObjectOutputStream _outputStream;
  private final ObjectInputStream _inputStream;
  public ObjectSocketImpl(  String serverIpAddress,  int serverPort) throws IOException {
    this(new Socket(serverIpAddress,serverPort));
  }
  public ObjectSocketImpl(  Socket socket) throws IOException {
    _socket=socket;
    _outputStream=new ObjectOutputStream(_socket.getOutputStream());
    _inputStream=new ObjectInputStream(_socket.getInputStream());
  }
  public void writeObject(  Object object) throws IOException {
    _outputStream.writeObject(object);
    _outputStream.reset();
    _outputStream.flush();
  }
  public Object readObject() throws IOException, ClassNotFoundException {
    return _inputStream.readObject();
  }
  public void close() throws IOException {
    _outputStream.close();
    _inputStream.close();
    _socket.close();
  }
}
