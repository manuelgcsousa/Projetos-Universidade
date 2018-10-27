/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package reverseproxy;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.concurrent.locks.ReentrantLock;

/**
 *
 * @author carlos
 */
public class ReverseProxy {
    private ServerSocket serverSocket;
    private final int port;
    private final HashMap<String,Stats> tabelaEstado;
    ReentrantLock lock;
    
    public ReverseProxy(int port){
        this.port = port;
        this.tabelaEstado = new HashMap<>();
        this.lock = new ReentrantLock();
    }
    
    public void initServer(){
        
        /* Create Thread to run MonitorUDP as soon as the server starts */
        MonitorUDP mUDP = new MonitorUDP(this.tabelaEstado, this.lock);
        Thread i = new Thread(mUDP);
        i.start();
        
        
        /*  Create Threads to handle the HTTP requests  */
        try {
            System.out.println("##### REVERSE PROXY #####");
            this.serverSocket = new ServerSocket(this.port);

            while (true) {
                System.out.println("ServerMain > Server is running waiting for a new connection...");
                Socket socket = serverSocket.accept();
                System.out.println("ServerMain > Connection received! Create worker thread to handle connection.");
                
                /* Thread worker will determine the best server to connect and redirect the connection to it */
                Worker w = new Worker(this.tabelaEstado, this.lock, socket);
                Thread t = new Thread(w);
                t.start();
            }
        } catch (IOException e) {
            System.out.println("Error accepting connection: " + e.getMessage());
        }
    }

    public static void main(String[] args) {
        ReverseProxy reverseProxy = new ReverseProxy(80);
        reverseProxy.initServer();
    }    
}
