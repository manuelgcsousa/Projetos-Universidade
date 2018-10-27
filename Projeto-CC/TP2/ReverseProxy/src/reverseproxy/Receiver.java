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
import java.net.SocketException;
import java.util.HashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author carlos
 */
public class Receiver implements Runnable {
    HashMap<String,Stats> tabelaEstado;
    ReentrantLock lock;
    
    public Receiver(HashMap<String, Stats> tabelaEstado, ReentrantLock lock) {
        this.tabelaEstado = tabelaEstado;
        this.lock = lock;
    }

    @Override
    public void run() {
        try {
            
            DatagramSocket dSocket = new DatagramSocket(8888);
            
            while(true){
                /* Cria Datagrama UDP e Socket e envia-o */
                byte[] data = new byte[100];
                DatagramPacket dPacket = new DatagramPacket(data, data.length);
                
                dSocket.receive(dPacket);
                
                String received = new String( dPacket.getData(), 0, dPacket.getLength() );
                
                
                String originalSecurity = "#@abcdefghijklmnopqrstuvwxyz@#";
                String security = received.substring(0,30);
                boolean igual = security.equals(originalSecurity);
                
                if ( igual ) {
                    InetAddress origin = dPacket.getAddress();
                    String originIP = origin.toString().substring(1);

                    //System.out.println("->" + originIP + "<-");
                    //System.out.println("MonitorUDP : Received = " + received);
                    
                    // Ainda n ta certo // 
                    int bCPU = received.indexOf("|") + 1;
                    int eCPU = received.indexOf("!");
                    int bRAM = received.indexOf("[") + 1;
                    int eRAM = received.indexOf("]");
                    int bNProcs = received.indexOf("$") + 1;
                    int eNProcs = received.indexOf("%");
                    int bRTT = received.indexOf("&") + 1;
                    
                    double cpuUsed = Double.parseDouble(received.substring(bCPU, eCPU));
                    double ramUsed = Double.parseDouble(received.substring(bRAM, eRAM));
                    int nProc = (int) Double.parseDouble(received.substring(bNProcs, eNProcs));
                    long time = System.currentTimeMillis();
                    double rtt = time - Double.parseDouble(received.substring(bRTT));

                    lock.lock();
                    
                    try {
                        
                        if(!this.tabelaEstado.containsKey(originIP)){
                            Stats stats = new Stats(originIP, 80, cpuUsed, ramUsed, nProc, rtt);
                            this.tabelaEstado.put(originIP, stats);
                        } else {
                            Stats stats = this.tabelaEstado.get(originIP);
                            stats.setN(0);
                            stats.setCpuUsed(cpuUsed);
                            stats.setRamUsed(ramUsed);
                            stats.setnProc(nProc);
                            stats.setRtt(rtt);
                        }
                        
                    } finally {
                        lock.unlock();
                    }
                    
                }
            }
        } catch (SocketException ex) {
            Logger.getLogger(Receiver.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(Receiver.class.getName()).log(Level.SEVERE, null, ex);
        } 
    }
    
}
