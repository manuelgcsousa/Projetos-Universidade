/**
 * Classe InfoReservas.
 * 
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */

import java.io.*;

public class InfoReservas implements Serializable {
    /** Variáveis de Instância */
    private Cliente cliente;
    private Taxi taxi;
    private Ponto2D coordsDestinoCliente;
    private int indexTaxi; //Indice do Taxi no ArrayList de Taxistas na classe Umer
    private int indexMotorista;
    private boolean taxiProximo;
    
    /** 
     * Construtor vazio.
     */
    public InfoReservas() {
        this.cliente = new Cliente();
        this.taxi = new Taxi();
        this.coordsDestinoCliente = new Ponto2D();
        this.indexTaxi = -1;
        this.indexMotorista = -1;
        this.taxiProximo = false;
    }
    
    /** 
     * Construtor por cópia.
     * @param info
     */
    public InfoReservas (InfoReservas info) {
        this.cliente = info.getCliente();
        this.taxi = info.getTaxi();
        this.coordsDestinoCliente = info.getCoords();
        this.indexTaxi = info.getIndexTaxi();
        this.indexMotorista = info.getIndexMotorista();
        this.taxiProximo = info.getBoolProx();
    }
    
    /** 
     * Construtor por parâmetro.
     * @param c
     * @param t
     * @param destino
     * @param indexT
     * @param indexM
     * @param prox
     */
    public InfoReservas (Cliente c, Taxi t, Ponto2D destino, int indexT, int indexM, boolean prox) {
        this.cliente = c.clone();
        this.taxi = t.clone();
        this.coordsDestinoCliente = destino.clone();
        this.indexTaxi = indexT;
        this.indexMotorista = indexM;
        this.taxiProximo = prox;
    }
    
    /**
     * Getters e Setters da classe InfoReservas.
     */
    public Cliente getCliente() {
        return this.cliente.clone();
    }
    
    public Taxi getTaxi() {
        return this.taxi.clone();
    }
    
    public Ponto2D getCoords() {
        return this.coordsDestinoCliente.clone();
    }
    
    public int getIndexTaxi() {
        return this.indexTaxi;
    }
    
    public int getIndexMotorista() {
        return this.indexMotorista;
    }
    
    public boolean getBoolProx() {
        return this.taxiProximo;
    }
    
    public void setCliente (Cliente c) {
        this.cliente = c.clone();
    }
    
    public void setTaxi (Taxi t) {
        this.taxi = t.clone();
    }
    
    public void setCoords (Ponto2D p) {
        this.coordsDestinoCliente = p.clone();
    }
    
    public void setIndexTaxi (int t) {
        this.indexTaxi = t;
    }
    
    public void setIndexMotorista (int m) {
        this.indexMotorista = m;
    }
    
    public void setBoolProx (boolean b) {
        this.taxiProximo = b;
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return
     */
    public String toString() {
        return ("Informação sobre a reserva: \n" + this.cliente.toString() + "\n" + "Taxi: " + this.taxi.getMotorista().getEmail() + "\n"
                + this.coordsDestinoCliente.toString() + "\n" + "Taxi proximo? " + taxiProximo);
    }

    /**
     * Compara igualdade com outro objeto.
     * @param o
     * @return
     */
    public boolean equals (Object o) {
        if (this == o)
            return true;
        
        if (o == null || (this.getClass() != o.getClass()))
            return false;
        
        InfoReservas info = (InfoReservas) o;
        return (this.cliente.equals(info.getCliente()) && this.taxi.equals(info.getTaxi()) &&
                this.coordsDestinoCliente.equals(info.getCoords()) && this.indexTaxi == info.getIndexTaxi() && this.taxiProximo == info.getBoolProx());
    }
    
    /**
     * Retorna uma cópia da instância.
     * @return
     */
    public InfoReservas clone() {
        return new InfoReservas(this);
    }
}
