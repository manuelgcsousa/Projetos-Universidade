/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package AgenteUDP;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.management.ManagementFactory;
import java.lang.management.OperatingSystemMXBean;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.util.Random;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author carlos
 */
public class AgenteUDP {
    
    public static int randInt(int min, int max) {
        Random rand = new Random();
    
        int randomNum = rand.nextInt((max - min) + 1) + min;

        return randomNum;
    }
    
    /**
     *
     * @return
     */
    public static double getSystemLoad() {
        OperatingSystemMXBean opBean = ManagementFactory.getOperatingSystemMXBean();
        double systemLoad = opBean.getSystemLoadAverage();
        
        return systemLoad;
    }
    
    /**
     * 
     * @return 
     */
    private static double[] getStats() {
        
        double usedRam[] = new double[2];
        
        try {
            
            ProcessBuilder pb = new ProcessBuilder("../test");
            Process p = pb.start();
 
            BufferedReader in = new BufferedReader(new InputStreamReader(p.getInputStream()));
            
            String s;
            int i = 0;
            while( ((s = in.readLine()) != null) && i < 2){
                usedRam[i] = Double.parseDouble(s);
                i++;
            }
            
                        
        } catch (IOException ex) {
            Logger.getLogger(AgenteUDP.class.getName()).log(Level.SEVERE, null, ex);
        } 
        
        return usedRam;
    }
    
    /**
     *
     * @param args
     */
    public static void main(String[] args){
        System.out.println("AgenteUDP starting");
        
        int port = 8888;
        String multicastGroup = "239.8.8.8";
        MulticastSocket mSocket = null;
        InetAddress group = null;
        DatagramSocket dSocket = null;
        
        
        
        try {
            /* Cria nova socket e junta-se ao grupo multicast */
            mSocket = new MulticastSocket(port);
            group = InetAddress.getByName(multicastGroup);
            mSocket.joinGroup(group);
            
            System.out.println("Multicast Receiver running at:" + mSocket.getLocalSocketAddress());
 
            while(true) {
                byte[] data = new byte[56];
                DatagramPacket packet = new DatagramPacket(data, data.length);
                
                System.out.println("Waiting for a  multicast message...");
                
                mSocket.receive(packet);
                
                String received = new String( packet.getData(), 0, packet.getLength() );
                System.out.println("RECEIVED: " + received);
                
                // Validar a Mensagem atraves de HMAC;
                String originalSecurity = "#@abcdefghijklmnopqrstuvwxyz@#";
                String security = received.substring(0,30);
                boolean igual = security.equals(originalSecurity);
                
                if ( igual ) {
                    
                    dSocket = new DatagramSocket();
                    
                    InetAddress reverseProxyIP = packet.getAddress();
                    double systemLoad = getSystemLoad();
                    double systemRam[] = getStats();
                    String sLoad = Double.toString(systemLoad);
                    String sRam = Double.toString(systemRam[0]);
                    String nProcs = Double.toString(systemRam[1]);
                    String time = received.substring(31, 44);
                    String msg = originalSecurity + " SLoad =|" + sLoad + "! SRam =[" + sRam + "] nProcs =$" + nProcs + "% Time =&" + time;
                    
                    
                    DatagramPacket agentUDP = new DatagramPacket(msg.getBytes(), msg.length(), reverseProxyIP, 8888);
                    
                    int delay = randInt(1, 15);
                    
                    Thread.sleep(delay);
                    
                    dSocket.send(agentUDP);
                } 
            } 
        } catch(Exception e){
            e.printStackTrace();
        }
    }

}
