package sd;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author carlo
 */
public class TalkToPlayers implements Runnable{
   
    private BufferedWriter buW;
    private String s;
    
    public TalkToPlayers(BufferedWriter buW, String s){
        this.buW = buW;
        this.s = s;
    }
    
    public void run(){
        try {
            this.buW.write(this.s);
            this.buW.newLine(); 
            this.buW.flush();
        } catch (IOException ex) {
            Logger.getLogger(TalkToPlayers.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
