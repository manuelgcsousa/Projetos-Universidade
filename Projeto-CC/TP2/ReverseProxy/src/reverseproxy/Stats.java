/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package reverseproxy;

/**
 *
 * @author carlos
 */
public class Stats {
    private String ip;              // IP do servidor HTTP;
    private int port;               // Porta do servidor (Se necessario);
    private int n;                  // Numero de Vezes Seguidas que o Servidor n respondeu ao Pedido UDP;
    private double cpuUsed;         // CPU usado pelo servidor;
    private double ramUsed;         // Ram usada pelo servidor;
    private int nProc;              // Numero de Processos ativos no servidor
    private double rtt;             // Round Trip Time
    private double bW;              // Largura de Banda Usado Pelo Servidor
    /**
     * Ver mais recursos que podemos por para medir a carga do servidor
     */
    
    public Stats(String ip, int port, double cpuUsed, double ramUsed, int nProc, double rtt){
        this.ip = ip;
        this.port = port;
        this.n = 0;
        this.cpuUsed = cpuUsed;
        this.ramUsed = ramUsed;
        this.nProc = nProc;
        this.bW = 0;
    }

    public String getIp() {
        return ip;
    }

    public void setIp(String ip) {
        this.ip = ip;
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public int getN() {
        return n;
    }

    public void setN(int n) {
        this.n = n;
    }

    public double getCpuUsed() {
        return cpuUsed;
    }

    public void setCpuUsed(double cpuUsed) {
        this.cpuUsed = cpuUsed;
    }

    public double getRamUsed() {
        return ramUsed;
    }

    public void setRamUsed(double ramUsed) {
        this.ramUsed = ramUsed;
    }

    public int getnProc() {
        return nProc;
    }

    public void setnProc(int nProc) {
        this.nProc = nProc;
    }
    
    public double getRtt() {
        return rtt;
    }

    public void setRtt(double rtt) {
        this.rtt = rtt;
    }
    
    public double getbW() {
        return bW;
    }

    public void setbW(double bW) {
        this.bW = bW;
    }
    
    @Override
    public String toString(){
        String s = "IP = " + this.ip + " Port = " + this.port + " cpuUsed = " + this.cpuUsed + " ramUsed = " + this.ramUsed 
                           + " nProcs = " + this.nProc + " rtt " + this.rtt;
        return s;
    }
    
    /**
     * Método que retorna a carga do servidor;
     */
    public double getLoad() {
        double load = 0.5 * this.cpuUsed + 0.4 * this.ramUsed + 0.05 * this.nProc + 0.05 * this.rtt;
        
        return load;
    }
    
}
