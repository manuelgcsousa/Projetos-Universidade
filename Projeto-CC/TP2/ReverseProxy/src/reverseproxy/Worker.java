/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package reverseproxy;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.util.HashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author carlos
 */
public class Worker implements Runnable {
    private static int x = 0;
    private HashMap<String,Stats> tabelaEstado;
    private ReentrantLock lock;
    private Socket socket;
    private BufferedReader in;
    private BufferedWriter out;

    public Worker(HashMap<String, Stats> tabelaEstado, ReentrantLock lock, Socket socket) throws IOException {
        this.x = x + 1;
        this.tabelaEstado = tabelaEstado;
        this.lock = lock;
        this.socket = socket;
        this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        this.out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
    }
    
    private String serverToConnect() {
        String server = null;
        double status = 1000000;
        
        this.lock.lock();
        try {
            /* Calcular o valor mais pequeno */
            
            for(String s : this.tabelaEstado.keySet()){
                Stats st = this.tabelaEstado.get(s);
                
                if ( st.getLoad() < status ){
                    server = s;
                    status = st.getLoad();
                }
                
            }
        } finally {            
            this.lock.unlock();
        }
        
        return server;
    }
    
    @Override
    public void run() {
        try {
            System.out.println("Worker " + x + " created to handle connection");
            
            /* Escolher Servidor HTTP a conectar */
            String server = serverToConnect();
            //System.out.println("ServerToConnect = " + server);
            
            /* Conectar a esse servidor */
            Socket socketToHTTPServer = new Socket(server, 80);
            BufferedReader inHttpServer = new BufferedReader(new InputStreamReader(socketToHTTPServer.getInputStream()));
            BufferedWriter outHttpServer = new BufferedWriter(new OutputStreamWriter(socketToHTTPServer.getOutputStream()));
            System.out.println("Conectado ao Servidor HTTP");
                               
            /* Criar Thread para receber HTTP Get */ 
            
            ReceiverWorker rW = new ReceiverWorker(server, this.tabelaEstado, this.lock, inHttpServer, this.out);
            Thread t = new Thread(rW);
            t.start();
            
            /* Redirecionar GET recebido pelo ReverseProxy para Servidor HTTP */ 
            String info = "";
            int y = 0;
            
            while( (info = this.in.readLine()) != null && y < 2 ) {
                String s = info.concat("\r\n");
                outHttpServer.write(s);
                outHttpServer.flush();
                y++;
            }
            outHttpServer.write("\r\n\r\n");
            outHttpServer.flush();
        
        } catch (IOException ex) {
            Logger.getLogger(Worker.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
