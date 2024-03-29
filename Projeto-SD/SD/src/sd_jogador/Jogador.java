package sd_jogador;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.net.SocketException;
import java.net.UnknownHostException;
//import sd.Utilizador;

public class Jogador {
    
    private String user;
    private int port;

    public Jogador (String user, int port) {
	this.user = user;
        this.port = port;
    }

    public Socket socket;
    public BufferedReader in;
    public BufferedWriter out;
    public String nickname;
    public String password;

    public void clientStart() {
        try {	
            System.out.println("#### JOGADOR ####");
            
            //Tentativa de registo do nickname
            System.out.println("> Connecting to server...");
            socket = new Socket(this.user, this.port);
            System.out.println("> Connection accepted!");
			
            //criar canal de leitura do stdin
            BufferedReader systemIn = new BufferedReader(new InputStreamReader(System.in));
            in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
			
            System.out.println("Bem-Vindo ao Jogo!\n" + 
                               "=> Login:\n" + 
                               "Username: ");
            nickname = systemIn.readLine();
            out.write(nickname);
            out.newLine();
            out.flush();
            String response;
            while((response = in.readLine()) != null && !response.equals("ok")) {
		        System.out.println("Username inválido. Por favor, digite novamente.");
                nickname = systemIn.readLine();
                out.write(nickname);
                out.newLine();
                out.flush();				
            }
                        
            System.out.println("Password: ");
            password = systemIn.readLine();
            out.write(password);
            out.newLine();
            out.flush();
            response = "";
            while((response = in.readLine()) != null && !response.equals("ok")) {
                System.out.println("Password inválida. Por favor, digite novamente.");
		        password = systemIn.readLine();
		        out.write(password);
		        out.newLine();
		        out.flush();				
            }
                        
            //Criar listener thread para receber mensagens de outros utilizadores
            Thread listener = new Thread(new ClientListener());
            listener.start();

            String userInput; //string para ler o input do utilizador
            while((userInput = systemIn.readLine()) != null && !userInput.equals("quit")){
                out.write(userInput);
                out.newLine();
                out.flush();		
            }

            //fechar sockets
            systemIn.close();
            socket.shutdownOutput();
            socket.shutdownInput();
            socket.close();
        } catch (UnknownHostException e) {
            System.out.println("ERRO: Server doesn't exist!");
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
	
    public static void main(String[] args) {
        Jogador c = new Jogador("127.0.0.1", 12345);
	    c.clientStart();
    }
	
    public class ClientListener implements Runnable {
	    public ClientListener() {}
		
	    public void run() {
            String message;
            try {
		        while ((message = in.readLine()) != null) {
                    System.out.println(message);
		        }
            } catch (SocketException e) { } 
              catch (IOException e) {
	    	    e.printStackTrace();
            }
        }
    }
}
