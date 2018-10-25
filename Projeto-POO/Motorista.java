/**
 * Classe Motorista (subclasse de Actor).
 * 
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */

import java.util.Calendar;
import java.util.ArrayList;
import java.io.*;

public class Motorista extends Actor implements Serializable {
    /** Variáveis de Instância */
    private double cumpHorario;
    private int classMotorista;
    private double kmsRealizados;
    private boolean disponivel;
    private ArrayList<Viagem> viagensEfetuadas;
    private int nViagensEfetuadas;
    
    /** 
     * Construtor vazio.
     */
    public Motorista() {
        super();
        this.cumpHorario = 0;
        this.classMotorista = 0;
        this.kmsRealizados = 0;
        this.disponivel = true;
        this.viagensEfetuadas = new ArrayList<Viagem>();
        this.nViagensEfetuadas = 0;
    }
    
    /** 
     * Construtor por cópia.
     * @param m
     */
    public Motorista(Motorista m) {
        super(m);      
        this.cumpHorario = m.getCumpHorario();
        this.classMotorista = m.getClassMotorista();
        this.kmsRealizados = m.getKmsRealizados();
        this.disponivel = m.getDisponivel();
        this.viagensEfetuadas = m.getViagensEfetuadas();
        this.nViagensEfetuadas = m.getNViagensEfetuadas();    
    }
    
    /** 
     * Construtor por parâmetro.
     * @param email
     * @param nome
     * @param password
     * @param morada
     * @param dataDeNascimento
     * @param cumpHorario
     * @param classMotorista
     * @param kmsRealizados
     * @param disponivel
     * @param viagensEfetuadas
     * @param nViagensEfetuadas
     */  
    public Motorista(String email, String nome, String password, String morada, Calendar dateDeNascimento, double cumpHorario, int classMotorista, 
                     double kmsRealizados, boolean disponivel, ArrayList<Viagem> viagensEfetuadas, int nViagensEfetuadas) {
        super(email,nome,password,morada,dateDeNascimento);
        this.cumpHorario = cumpHorario;
        this.classMotorista = classMotorista;
        this.kmsRealizados = kmsRealizados;
        this.disponivel = disponivel;
        this.viagensEfetuadas = viagensEfetuadas;
        this.nViagensEfetuadas = nViagensEfetuadas; 
    }
    
    /**
     * Getters e Setters da classe Motorista.
     */
    public ArrayList<Viagem> getViagensEfetuadas() {
        ArrayList<Viagem> novo = new ArrayList<Viagem>();
          
        for (Viagem v : viagensEfetuadas) {
            novo.add(v.clone());
        }
        return novo;
    }
      
    public double getCumpHorario() {
        return this.cumpHorario;
    }
      
    public int getClassMotorista() {
        return this.classMotorista;
    }
     
    public double getKmsRealizados() {
        return this.kmsRealizados;
    }
     
    public boolean getDisponivel() {
        return this.disponivel;
    }
     
    public int getNViagensEfetuadas() {
        return this.nViagensEfetuadas;
    }
    
    public void setCumpHorario(double cumpHorario) {
        this.cumpHorario = cumpHorario;
    }
     
    public void setClassMotorista(int classMotorista) {
        this.classMotorista = classMotorista;
    }
     
    public void setKmsRealizados(double kmsRealizados) {
        this.kmsRealizados = kmsRealizados;
    }
     
    public void setDisponivel (boolean disponibilidade) {
        this.disponivel = disponibilidade;
    }
     
    public void setNViagensEfetuadas (int nViagensEfetuadas) {
        this.nViagensEfetuadas = nViagensEfetuadas;
    }
      
    public void setViagensEfetuadas(ArrayList<Viagem> viagensEfetuadas) {
       this.viagensEfetuadas.clear();
       for (Viagem v: viagensEfetuadas ) {
           this.viagensEfetuadas.add(v.clone());
       }
    }
    
    /**
     * Total de desvios do Motorista.
     * @return
     */
    public int contaDesvios() {
        int contaDesvios = 0;
        double desvio = 0;

        for (Viagem v : this.viagensEfetuadas) {
            desvio = v.getPrecoReal() - v.getPrecoEstimado();
            if (desvio != 0) contaDesvios++;
        }

        return contaDesvios;
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return
     */
    public String toString() {
        return new String ("-> Motorista: \n" + "Cumprimento do Horário: " + this.cumpHorario + "\n" +
                           "Classificação do Motorista " + this.classMotorista + "\n" + "Km's Realizados: " + this.kmsRealizados + "\n" +
                           "Está disponivel? " + this.disponivel + "Viagens Efetuadas: " + this.viagensEfetuadas.toString() +
                           "Número de Viagens Efetuadas: " + this.nViagensEfetuadas + super.toString());
    }
    
    /**
     * Compara igualdade com outro objeto.
     * @param umMotorista
     * @return
     */
    public boolean equals(Object umMotorista) {
        if (this == umMotorista) 
            return true;
           
        if ((umMotorista == null || (this.getClass() != umMotorista.getClass())))
            return false;
                   
        Motorista m = (Motorista) umMotorista;
        return(super.equals(m) && (this.cumpHorario == m.getCumpHorario()) && (this.classMotorista == m.getClassMotorista())
                  && (this.kmsRealizados == m.getKmsRealizados()) && (this.disponivel == m.getDisponivel()) && 
                     this.viagensEfetuadas.equals(m.getViagensEfetuadas()) && this.nViagensEfetuadas == m.getNViagensEfetuadas());
    }
    
    /**
     * Retorna uma cópia da instância.
     * @return
     */
    public Motorista clone() {
        return new Motorista(this);
    } 
} 
  