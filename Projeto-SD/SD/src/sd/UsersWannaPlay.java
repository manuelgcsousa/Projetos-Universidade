package sd;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.locks.ReentrantLock;

/**
 *
 * @author carlo
 */
public class UsersWannaPlay {
    HashMap<Integer, List<String>> usersToPlay;
    HashMap<String,Utilizador> users;
    ReentrantLock lock;
    
    public UsersWannaPlay(HashMap<String,Utilizador> users) {
        this.usersToPlay = new HashMap<Integer,List<String>>();
        this.users = users;
        this.lock = new ReentrantLock();
    }
    
    public HashMap<Integer, List<String>> getusersToPlay() {
        return this.usersToPlay;
    }

    public ReentrantLock getLock() {
        return this.lock;
    }
    
    public int getLength(int rank){
        if (rank == -1) return 11;
        return this.usersToPlay.get(rank).size();
    }
    
    public void addUserToPlay(int rank, String username){
        switch (rank) {
            case 0:
                usersToPlay.get(rank).add(username);
                break;
            case 9:
                usersToPlay.get(8).add(username);
                break;
            default:
                usersToPlay.get(rank - 1).add(username);
                usersToPlay.get(rank).add(username);
                break;
        }
    }
    
    public ArrayList<Utilizador> getUsers(int rank){
        ArrayList<Utilizador> users = new ArrayList<Utilizador>();
            
        for(String s : this.usersToPlay.get(rank)){
            users.add(this.users.get(s));
        }
            
        return users;
    }
    
    public void put(int rank, List<String> users){
        this.usersToPlay.put(rank, users);
    }
}
