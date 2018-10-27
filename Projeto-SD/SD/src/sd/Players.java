package sd;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.locks.ReentrantLock;

/**
 *
 * @author carlo
 */
public class Players {
    HashMap<String, BufferedWriter> jogadores;
    HashMap<String, BufferedReader> jogadoresIn;
    ReentrantLock lock;
    
    public Players() {
        this.jogadores = new HashMap<String,BufferedWriter>();
        this.jogadoresIn = new HashMap<String,BufferedReader>();
        this.lock = new ReentrantLock();
    }

    public synchronized HashMap<String, BufferedWriter> getjogadores() {
        return this.jogadores;
    }
    
    public synchronized HashMap<String, BufferedReader> getJogadoresIn() {
        return jogadoresIn;
    }
    
    public synchronized ReentrantLock getLock() {
        return this.lock;
    }
    
    public synchronized void put(String username, BufferedWriter out){
        this.jogadores.put(username,out);
    }
    
    public synchronized void putIn(String username, BufferedReader in){
        this.jogadoresIn.put(username,in);
    }
    
    public synchronized BufferedWriter get(String username){
        return this.jogadores.get(username);
    }
    
    public synchronized HashMap<String,BufferedWriter> getBufferedWriters(ArrayList<Utilizador> players){
        HashMap<String,BufferedWriter> buffs = new HashMap<String,BufferedWriter>();
        
        for(Utilizador u : players){
            buffs.put(u.getUsername(),this.jogadores.get(u.getUsername()));
        }
        
        return buffs;
    }
    
    public synchronized HashMap<String, BufferedReader> getBufferedReaders(ArrayList<Utilizador> players){
        HashMap<String, BufferedReader> buffs = new HashMap<String,BufferedReader>();
        
        for(Utilizador u : players){
            buffs.put(u.getUsername(),this.jogadoresIn.get(u.getUsername()));
        }
        
        return buffs;
    }
}
