/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package reverseproxy;

import java.util.HashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author carlos
 */
public class MonitorUDP implements Runnable {
    private HashMap<String,Stats> tabelaEstado;
    private ReentrantLock lock;
    private Receiver receiver;
    private Sender sender;
    
    public MonitorUDP(HashMap<String,Stats> tabelaEstado, ReentrantLock lock){
        this.tabelaEstado = tabelaEstado;
        this.lock = lock;
        this.receiver = new Receiver(this.tabelaEstado, this.lock);
        this.sender = new Sender(this.tabelaEstado, this.lock);
    }

    @Override
    public void run() {
        Thread receiver = new Thread(this.receiver);
        Thread sender = new Thread(this.sender);
        receiver.start();
        sender.start();
        
        while(true){
            
            try {
                
                lock.lock();
                
                try {
                    
                    /**
                     * 
                     * De 15 em 15 segundos o MonitorUDP verifica se existe algum servidor que nao
                     * tenha respondido a 3 pedidos consecutivos e se existir retira-o do HashMap
                     * 
                     * */
                    System.out.println("Imprimir Tabela de Estado");
                    for( String s: this.tabelaEstado.keySet() ){
                        Stats sts = this.tabelaEstado.get(s);
                        System.out.println(sts.toString());
                        
                        if ( sts.getN() >= 3 ){
                            Stats remove = this.tabelaEstado.remove(sts);
                        }
                        
                    }
                    
                } finally {
                    lock.unlock();
                }
                
                Thread.sleep(15000);
                
            }   catch (InterruptedException ex) {
                Logger.getLogger(MonitorUDP.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }
}
