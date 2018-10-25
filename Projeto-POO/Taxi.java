/**
 * Classe Taxi
 * 
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */

import java.io.*;
import java.util.List;
import java.util.ArrayList; 
import java.util.GregorianCalendar;

public class Taxi implements Serializable {
    /** Variáveis de Instância */
    private String matricula;
    private Motorista condutor;
    private double velMedia;
    private double precoKm;
    private double factorFiabilidade;
    private double totalFaturado;
    private Ponto2D coords;
    private ArrayList<Viagem> viagensEfetuadas;

    /** 
     * Construtor vazio.
     */
    public Taxi() {
        this.matricula = "";
        this.condutor = new Motorista();
        this.velMedia = 0.0;
        this.precoKm = 0.0;
        this.factorFiabilidade = 0;
        this.totalFaturado = 0;
        this.coords = new Ponto2D();
        this.viagensEfetuadas = new ArrayList<Viagem>();
    }
    
    /** 
     * Construtor por cópia.
     * @param t
     */
    public Taxi(Taxi t) {
        this.matricula = t.getMatricula();
        this.condutor = t.getMotorista();
        this.velMedia = t.getVelMedia();
        this.precoKm = t.getPrecoKm();
        this.factorFiabilidade = t.getFactor();
        this.totalFaturado = t.getTotalFaturado();
        this.coords = t.getCoords();
        this.viagensEfetuadas = t.getViagensEfetuadas();
    }
    
    /** 
     * Construtor por parâmetro.
     * @param matricula
     * @param condutor
     * @param velMedia
     * @param precoKm
     * @param factorFiabilidade
     * @param totalFaturado
     * @param coords
     * @param viagensEfetuadas
     */
    public Taxi(String matricula, Motorista condutor, double velMedia, double precoKm, double factorFiabilidade, 
                double totalFaturado, Ponto2D coords, ArrayList<Viagem> viagensEfetuadas) {
        this.matricula = matricula;
        this.condutor = condutor;
        this.velMedia = velMedia;
        this.precoKm = precoKm;
        this.factorFiabilidade = factorFiabilidade;
        this.totalFaturado = totalFaturado;
        this.coords = coords;
        for (Viagem v : viagensEfetuadas) {
            this.viagensEfetuadas.add(v.clone());
        }
    }
    
    /**
     * Getters e Setters da classe Taxi.
     */
    public String getMatricula() {
        return this.matricula;
    }
    
    public Motorista getMotorista() {
        return this.condutor.clone();
    }
    
    public double getVelMedia() {
        return this.velMedia;
    }
    
    public double getPrecoKm() {
        return this.precoKm;
    }
    
    public double getFactor() {
        return this.factorFiabilidade;
    }
    
    public double getTotalFaturado() {
        return this.totalFaturado;
    }
    
    public Ponto2D getCoords() {
        return this.coords.clone();
    }
    
    public ArrayList<Viagem> getViagensEfetuadas() {
        ArrayList<Viagem> novo = new ArrayList<Viagem>();
        if (this.viagensEfetuadas == null) return novo;
        for (Viagem v : this.viagensEfetuadas) {
            novo.add(v.clone());
        }
        return novo;
    }
    
    public void setMatricula(String m) {
        this.matricula = m;
    }
    
    public void setMotorista(Motorista m) {
        this.condutor = m.clone();
    }
    
    public void setVelMedia(double velMedia) {
        this.velMedia = velMedia;
    }
    
    public void setPrecoKm(double precoKm) {
        this.precoKm = precoKm;
    }
    
    public void setFactor(double factor) {
        this.factorFiabilidade = factor;
    }
    
    public void setTotalFaturado(double totalFaturado) {
        this.totalFaturado = totalFaturado;
    }
    
    public void setCoords(Ponto2D coords) {
        this.coords = coords.clone();
    }
    
    public void setViagensEfetuadas(ArrayList<Viagem> viagensEfetuadas) {
        this.viagensEfetuadas.clear();
        for (Viagem v : viagensEfetuadas) {
            this.viagensEfetuadas.add(v.clone());
        }
    }

    /**
     * Aumenta o totalFaturado no objeto.
     * @param custoViagem
     */
    public void aumentarTotalFaturado(double custoViagem) {
        this.totalFaturado += custoViagem;
    }
    
    /**
     * Devolve o totalFaturado num determinado período.
     * @param dataInicial
     * @param dataFinal
     * @return
     */
    public double getTotalFaturadoData (GregorianCalendar dataInicial, GregorianCalendar dataFinal) {
        double total = 0;
        
        for (Viagem v : this.viagensEfetuadas) {
            GregorianCalendar aux = v.getCalendar();
            if (aux.before(dataFinal) && aux.after(dataInicial)) total += v.getPrecoReal();
        }
        
        return total;
    }
    
    /**
     * Compara igualdade com outro objeto.
     * @param o
     * @return
     */
    public boolean equals (Object o) {
        if (this == o) return true;
        
        if ((o == null) || (this.getClass() != o.getClass())) return false;
        
        Taxi aux = (Taxi) o;
        
        return (this.matricula.equals(aux.getMatricula()) && this.velMedia == aux.getVelMedia() && this.precoKm == aux.getPrecoKm() && 
                this.factorFiabilidade == aux.getFactor() && this.totalFaturado == aux.getTotalFaturado() && 
                this.coords.equals(aux.getCoords()) && this.viagensEfetuadas.equals(aux.getViagensEfetuadas()));
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return
     */
    public String toString() {
        String aux = new String();
        aux = ("Taxi: \n" + "Matrícula: " + this.matricula + "\n" + "Velocidade Média: " + this.velMedia + "\n" + "Preco por Km: " + this.precoKm + "\n" + 
               "Factor de Fiabilidade: " + this.factorFiabilidade + "\n" + "Total Faturado: " + this.totalFaturado + "\n" + 
               "Coordenadas: " + this.coords.toString() + "\n" + "Viagens Efetuadas com o Taxi: " + this.viagensEfetuadas.toString() + "\n");
        return aux;
    }

    /**
     * Retorna uma cópia da instância.
     * @return
     */
    public Taxi clone() {
        return new Taxi(this);
    }
}
