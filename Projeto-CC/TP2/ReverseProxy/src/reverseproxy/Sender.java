/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package reverseproxy;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.HashMap;
import java.util.Random;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author carlos
 */
public class Sender implements Runnable {

    private HashMap<String,Stats> tabelaEstado;
    private ReentrantLock lock;
    
    public Sender(HashMap<String, Stats> tabelaEstado, ReentrantLock lock) {
        this.tabelaEstado = tabelaEstado;
        this.lock = lock;
    }
    
    public static int randInt(int min, int max) {
        Random rand = new Random();
    
        int randomNum = rand.nextInt((max - min) + 1) + min;

        return randomNum;
    }
    
    @Override
    public void run() {
        DatagramSocket socket = null;
        InetAddress group;
        
        try {
            /* De x em x segundos envia o pacote a pedir servidores */
            while(true){
                socket = new DatagramSocket();
                group = InetAddress.getByName("239.8.8.8");
                
                String security = "#@abcdefghijklmnopqrstuvwxyz@#";
                String time = Long.toString(System.currentTimeMillis());
                String msg = security + " " + time + " " + "Info Please";
                int sleep = randInt(1,10);
                
                //System.out.println(Arrays.toString(msg.getBytes()) + "lenght=" + msg.length());
                //System.out.println(sleep + " " + msg.length() + " time " + time);
                
                DatagramPacket hi = new DatagramPacket(msg.getBytes(), msg.length(),group, 8888);
                socket.send(hi);
                socket.close();
                
                lock.lock();
                try {
                    for(String s : this.tabelaEstado.keySet()){
                        int n = this.tabelaEstado.get(s).getN();
                        this.tabelaEstado.get(s).setN(n+1);
                    }
                } finally {
                    lock.unlock();
                }
                
                Thread.sleep(sleep * 1000);
            }
        } catch (IOException | InterruptedException ex) {
            Logger.getLogger(Receiver.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
}
                