/**
 * Classe Cliente (subclasse de Actor).
 * 
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */

import java.io.*;
import java.util.Calendar;
import java.util.ArrayList;
import java.util.List;

public class Cliente extends Actor implements Serializable {
    /** Variáveis de Instância */
    private Ponto2D coords; 
    private Taxi taxiEscolhido;
    private ArrayList<Viagem> viagensEfetuadas;
    private boolean especifico;
    
    /** 
     * Construtor vazio.
     */
    public Cliente() {
        super();
        this.coords = new Ponto2D();
        this.taxiEscolhido = new Taxi();
        this.viagensEfetuadas = new ArrayList<Viagem>();
        this.especifico = false;
    }
    
    /** 
     * Construtor por cópia.
     * @param c
     */
    public Cliente(Cliente c) {
        super(c);
        this.coords = c.getCoords();
        this.taxiEscolhido = c.getTaxiEscolhido();
        this.viagensEfetuadas = c.getViagensEfetuadas();
        this.especifico = c.getEspecifico();
    }
    
    /** 
     * Construtor por parâmetro.
     * @param email
     * @param nome
     * @param password
     * @param morada
     * @param dataDeNascimento
     * @param coords
     * @param taxiEscolhido
     * @param viagensEfetuadas
     * @param especifico
     */
    public Cliente(String email, String nome, String password, String morada, Calendar dateDeNascimento, Ponto2D coords, Taxi taxiEscolhido, 
                   ArrayList<Viagem> viagensEfetuadas, boolean especifico) {
        super(email,nome,password,morada,dateDeNascimento);
        this.coords=coords;
        this.taxiEscolhido = taxiEscolhido;
        this.viagensEfetuadas = viagensEfetuadas;
        this.especifico = especifico;
    }
    
    /**
     * Getters e Setters da classe Cliente.
     */
    public Ponto2D getCoords() {
        return this.coords.clone();
    }
    
    public Taxi getTaxiEscolhido() {
        return this.taxiEscolhido.clone();
    }
    
    public ArrayList<Viagem> getViagensEfetuadas() {
        ArrayList<Viagem> novo = new ArrayList<Viagem>();
        
        for(Viagem v : viagensEfetuadas) {
            novo.add(v.clone());
        }
        return novo;
    }
    
    public boolean getEspecifico() {
        return this.especifico;
    }
    
    public void setEspecifico(boolean especifico) {
        this.especifico = especifico;
    }
    
    public void setCoords(Ponto2D coords) {
        this.coords = coords.clone();
    }
    
    public void setTaxiEscolhido(Taxi taxiEscolhido) {
        this.taxiEscolhido = taxiEscolhido;
    }
    
    public void setViagensEfetuadas(ArrayList<Viagem> viagensEfetuadas) {
        this.viagensEfetuadas.clear();
        for(Viagem v: viagensEfetuadas ) {
            this.viagensEfetuadas.add(v.clone());
        }
    }
    
    /**
     * Retorna a despesa total do Cliente.
     * @return
     */
    public double despesaTotal() {
        double despTotal = 0;
        for (Viagem v : this.viagensEfetuadas){
            despTotal += v.getPrecoReal();
        }
        return despTotal;
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return
     */
    public String toString() {
        return new String("Coordenadas: " + "\n" + this.coords.toString() + "Taxi :" + this.taxiEscolhido.toString() +
                          "Viagens Efetuadas: " + this.viagensEfetuadas.toString() + "\n" + super.toString() + "É especifico?" + this.especifico);
    }
    
    /**
     * Compara igualdade com outro objeto.
     * @param umCliente
     * @return
     */
    public boolean equals(Object umCliente) {
        if (this == umCliente) 
            return true;
            
        if ((umCliente == null || (this.getClass() != umCliente.getClass())))
            return false;
            
        Cliente c = (Cliente) umCliente;
        return(super.equals(c) && this.coords.equals(c.getCoords()) && this.viagensEfetuadas.equals(c.getViagensEfetuadas()) &&
               this.taxiEscolhido.equals(c.getTaxiEscolhido()) && this.especifico == c.getEspecifico());
    }
    
    /**
     * Retorna uma cópia da instância.
     * @return
     */
    public Cliente clone() {
        return new Cliente(this);
    }
}
