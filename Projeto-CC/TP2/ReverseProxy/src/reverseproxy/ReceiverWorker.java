/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package reverseproxy;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author carlos
 */
public class ReceiverWorker implements Runnable{
    String server;
    HashMap<String,Stats> tabelaEstado;
    ReentrantLock lock;            
    BufferedReader inHttpServer;
    BufferedWriter out;

    public ReceiverWorker(String server, HashMap<String,Stats> tabelaEstado, ReentrantLock lock, 
            BufferedReader inHttpServer, BufferedWriter out) {
        this.server = server;
        this.tabelaEstado = tabelaEstado;
        this.lock = lock;
        this.inHttpServer = inHttpServer;
        this.out = out;
    }
                
    @Override
    public void run() {
        try {
            String info = null;
            
            /* Receber Dados enviados pelo Servidor HTTP para a Socket e envia-los de volta para quem se conectou ao ReverseProxy */
            while( (info = inHttpServer.readLine()) != null ){
                this.lock.lock();
                try {
                    //System.out.println("Received = " + info);
                    double bWi = this.tabelaEstado.get(server).getbW();
                    this.tabelaEstado.get(server).setbW(bWi + info.length());
                } finally {
                    this.lock.unlock();
                }
                
                this.out.write(info);
                this.out.newLine();
                this.out.flush();
                
                this.lock.lock();
                try {
                    //System.out.println("Received = " + info);
                    double bWi = this.tabelaEstado.get(server).getbW();
                    this.tabelaEstado.get(server).setbW(bWi - info.length());
                } finally {
                    this.lock.unlock();
                }
            }
        } catch (IOException ex) {
            Logger.getLogger(ReceiverWorker.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
}
