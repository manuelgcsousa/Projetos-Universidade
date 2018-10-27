package sd;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

public class GameServer {

    private ServerSocket serverSocket;
    private int port;
    private HashMap<String, Utilizador> users;
    private Map<Integer, List<String>> usersSameRank;
    private ArrayList<Worker> workers;
    private UsersWannaPlay usersToPlay;
    private Players jogadores;
    private ReentrantLock lock;
    
    /**
     * GameServer => Construtor do Servidor.
     * - users => Estrutura de dados que guarda as informaçoes relativas aos jogadores guardados no sistema.
     * - usersSameRank => Estrutura de dados que guarda os jogadores com o rank semelhante.
     * - usersToPlay => Contem os jogadores que ja demonstraram a intencao de jogar com ranks semelhantes.
     * - jogadores => Contem os BufferedReader/Writer para todos os jogadores atualmente ligados ao servidor.
     * 
     * @param port
     * @throws IOException
     */
    public GameServer (int port) throws IOException {
        this.port = port;
        this.users = new HashMap<String, Utilizador>();
        this.usersSameRank = new HashMap<Integer, List<String>>();
        this.workers = new ArrayList<Worker>();
        this.usersToPlay = new UsersWannaPlay(users);
        this.jogadores = new Players();
        this.lock = new ReentrantLock();
                
        for(int i = 0; i < 10; i++){
            List<String> playersSameRank = new ArrayList<String>();
            List<String> playersToPlay = new ArrayList<String>();
            this.usersSameRank.put(i, playersSameRank);
            this.usersToPlay.put(i, playersToPlay);
        }
    }

    public UsersWannaPlay getUsersToPlay() {
        return this.usersToPlay;
    }

    public Players getJogadores() {
        return this.jogadores;
    }
        
    public Map<String, Utilizador> getUsers() {
        return this.users;
    }

    public Map<Integer, List<String>> getUsersSameRank() {
        return this.usersSameRank;
    }
        
    public ArrayList<Worker> getWorkers() {
        return this.workers;
    }
       
    public synchronized int getRank(String username){
        return this.users.get(username).getRank();
    }
        
    public synchronized int login (String username, String password) {
        Utilizador user = this.users.get(username);

        if (user != null) {
            String pass = user.getPassword();
            if (pass.equals(password)) {
                int rank = user.getRank();
                    switch (rank) {
                        case 0:
                            List<String> usersRankD = this.usersSameRank.get(rank);
                            usersRankD.add(username);
                            break;
                                    
                        case 9:
                            List<String> usersRankU = this.usersSameRank.get(8);
                            usersRankU.add(username);
                            break;
                        default:
                            List<String> usersRankDown = this.usersSameRank.get(rank);
                            List<String> usersRankUP = this.usersSameRank.get(rank + 1);
                            usersRankDown.add(username);
                            usersRankUP.add(username);
                            break;
                    }
                    return 1; //Utilizador existente e password correta.
            } else {
                return 0; //Utilizador existente e password errada.
            }
        }
        //Registar o Utilizador;
        return -1; //Utilizador inexistente.
    }
        
    public synchronized void registaUser(String username, String password) {
        Utilizador novo = new Utilizador(username, password, 0);
        this.users.put(username, novo);
    }

    public void startServer() {
        try {
            System.out.println("##### GAME SERVER #####");
            this.serverSocket = new ServerSocket(this.port);

            while (true) {
                System.out.println("ServerMain > Server is running waiting for a new connection...");
                Socket socket = serverSocket.accept();
                System.out.println("ServerMain > Connection received! Create worker thread to handle connection.");
                Thread t = new Thread(new Worker(this, socket));
                t.start();
            }
        } catch (IOException e) {
            System.out.println("Error accepting connection: " + e.getMessage());
        }
    }
            
    public static void main (String[] args) throws IOException {
        GameServer s = new GameServer(12345);
        for (int i = 0; i < 20; i++) {
            if (i <= 4) {
                int rank = 6;
                Utilizador user = new Utilizador("Jogador" + (i + 1), "pass" + (i + 1), rank);
                s.users.put("Jogador" + (i + 1), user);
            } else if (i > 4 && i <= 9) {
                int rank = 7;
                Utilizador user = new Utilizador("Jogador" + (i + 1), "pass" + (i + 1), rank);
                s.users.put("Jogador" + (i + 1), user);
            } else if (i > 9 && i <= 14) {
                int rank = 8;
                Utilizador user = new Utilizador("Jogador" + (i + 1), "pass" + (i + 1), rank);
                s.users.put("Jogador" + (i + 1), user);
            } else {
                int rank = 9;
                Utilizador user = new Utilizador("Jogador" + (i + 1), "pass" + (i + 1), rank);
                s.users.put("Jogador" + (i + 1), user);
            }
        }
        s.startServer();
    }
}
