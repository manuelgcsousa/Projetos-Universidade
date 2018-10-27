package sd;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class Worker implements Runnable {

    private String username;
    private int rank;
    private GameServer server;
    private Socket socket;
    private BufferedReader in;
    private BufferedWriter out;

    public Worker (GameServer server, Socket socket) throws IOException {
        this.server = server;
        this.socket = socket;
        this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        this.out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
    }

    public String cutUsername (String info) {
        info = info + "\n";
        return info.substring(info.indexOf("@") + 1, info.indexOf("\n"));
    }

    public String cutPassword (String info) {
        info = info + "\n";
        return info.substring(info.indexOf("$") + 1, info.indexOf("\n"));
    }

    @Override
    public void run() {
        try {
            String info = "";
            
            while ((info = this.in.readLine()).charAt(0) != '@') {
                out.write("Username inválido.");
                out.newLine();
                out.flush();
            }
            
            out.write("ok");
            out.newLine();
            out.flush();
            
            this.username = cutUsername(info);
            
            info = "";
            while ((info = this.in.readLine()).charAt(0) != '$') {
                out.write("Password inválida.");
                out.newLine();
                out.flush();
            }
            
            out.write("ok");
            out.newLine();
            out.flush();
            
            String password = cutPassword(info);
                        
            int auth = this.server.login(username, password);

            if (auth == -1) {
                out.write("User não encontrado!");
                out.newLine();
                out.flush();
                
                this.server.registaUser(this.username, password);
                out.write("User registado com sucesso!");
                
                /* Faz o Login do novo user após registo. */
                auth = this.server.login(username, password);
            }

            while (auth == 0) {
                out.write("Dados Inválidos!\nIntroduza Password.");
                out.newLine();
                out.flush();
                out.write("Password: ");
                out.flush();
                password = this.in.readLine();
                auth = this.server.login(this.username, password);
            }
            
            if (auth == 1) {        
                this.rank = this.server.getRank(this.username);
                
                this.server.getJogadores().putIn(this.username, this.in);
                this.server.getJogadores().put(this.username, this.out);
                
                out.write("\n");
                out.flush();
                out.write("Utilizador autenticado com Sucesso!\nBem-Vindo!\n");
                out.newLine();
                out.flush();
                
                out.write("Username: " + this.username + "\n" +
                          "Rank: " + this.server.getUsers().get(username).getRank() + "\n");
                out.newLine();
                out.flush();
                
                String menu = "###### Menu ######\n" //Username e Rank
                            + "1) Começar Partida\n"
                            + "2) Sair\n"
                            + "##################\n"
                            + "\n"
                            + "-> Opção: ";
                out.write(menu);
                out.flush();
                 
                String s = "";
                boolean b = false;
                boolean c = false;
                boolean aJogar = false;
                ArrayList<Utilizador> usersToPlay = new ArrayList<>();

                while( !(s.equals("2")) ) {
                    
                    if (aJogar == false) {
                        s = in.readLine(); 
                    }
                    
                    if (s.equals("1")) {
                        s = "";
                        aJogar = true;
                        this.out.write("Deseja Jogar");
                        this.out.newLine();
                        this.out.flush();
                        
                        /* Se o map de jogadores que querem jogar n tiver jogadores suficientes, ele tem de meter no Map
                        que tem os jogadores em Espera e dizer que está à espera de jogadores. */
                        
                        this.server.getUsersToPlay().getLock().lock();
                        
                        if (this.server.getUsersToPlay().getLength(rank - 1) < 10 && this.server.getUsersToPlay().getLength(rank) < 10) {

                            this.server.getUsersToPlay().addUserToPlay(rank, username);
                            
                            this.out.write("Infelizmente não existem jogadores suficientes para iniciar uma partida.\n" + 
                                           "Aguarde...");
                            this.out.newLine();
                            this.out.flush();
                        }
                        
                        /* Se o map de jogadores tiver jogadores suficientes faz o jogo.*/
                        if (((b = (this.server.getUsersToPlay().getLength(rank - 1) == 10)) || (c = (this.server.getUsersToPlay().getLength(rank) == 10)))) {

                            if (b) {
                                usersToPlay = this.server.getUsersToPlay().getUsers(rank - 1);
                                this.rank = this.rank - 1;
                            } else if (c) {
                                usersToPlay = this.server.getUsersToPlay().getUsers(rank);
                            }
                            
                            //Fazer clears do nível superior e inferior.
                            for (Utilizador u : usersToPlay) {
                                for (List<String> l : this.server.getUsersToPlay().getusersToPlay().values()) {
                                    if (l.contains(u.getUsername())) {
                                        l.remove(u.getUsername());
                                    }
                                }
                            }
                                                        
                            this.server.getJogadores().getLock().lock();
                            HashMap<String, BufferedReader> jogadoresIn = this.server.getJogadores().getBufferedReaders(usersToPlay);
                            HashMap<String, BufferedWriter> jogadores = this.server.getJogadores().getBufferedWriters(usersToPlay);
                            this.server.getJogadores().getLock().unlock();

                            Jogo j = new Jogo(usersToPlay, jogadores, jogadoresIn, rank);
                            Thread t = new Thread(j);
                            t.start();
                        }
                        this.server.getUsersToPlay().getLock().unlock();
                    }
                }
                
                out.write("Desconectado do Jogo. Volte sempre!");
                out.newLine();
                out.flush();
                
                out.close();
                socket.close();
                
                System.out.println("Conexão ao Cliente fechada.");
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
