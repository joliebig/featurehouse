import javax.persistence.Entity;
import javax.persistence.Column;
import java.io.Serializable;

@Entity
private class Book implements Serializable {
       
  @Column(updatable=false,nullable=false)
  private String getTitle() {
    return title;
  }
  
}