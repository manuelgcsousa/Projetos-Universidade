/**
 * Classe Viagem.
 * 
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */

import java.util.Calendar;
import java.io.*;
import java.util.GregorianCalendar;

public class Viagem implements Serializable {
   /** Variáveis de Instância */
   private Motorista condutor;
   private Ponto2D coordPartida;
   private Ponto2D coordChegada;
   private boolean taxiProximo;
   private Taxi taxiUsado;
   private double distanciaInicial;
   private double distanciaViagem;
   private double precoEstimado;
   private double precoReal;
   private int nota;
   private GregorianCalendar data;

   /** 
    * Construtor vazio.
    */
   public Viagem() {
       this.condutor = new Motorista();
       this.coordPartida = new Ponto2D();
       this.coordChegada = new Ponto2D();
       this.taxiProximo = false;
       this.taxiUsado = new Taxi();
       this.distanciaInicial = 0.0;
       this.distanciaViagem = 0.0;
       this.precoEstimado = 0.0;
       this.precoReal = 0.0;
       this.nota = 0;
       this.data = new GregorianCalendar();
   }
   
   /** 
    * Construtor por cópia.
    * @param v
    */ 
   public Viagem(Viagem v) {
       this.condutor = v.getCondutor();
       this.coordPartida = v.getCoordPartida();
       this.coordChegada = v.getCoordChegada();
       this.taxiProximo = v.getTaxiProximo();
       this.taxiUsado = v.getTaxiUsado();
       this.distanciaInicial = v.getDistanciaIncial();
       this.distanciaViagem = v.getDistanciaViagem();
       this.precoEstimado = v.getPrecoEstimado();
       this.precoReal = v.getPrecoReal();
       this.nota = v.getNota();
       this.data = v.getCalendar();
   }
   
   /** 
    * Construtor por parâmetro.
    * @param condutor
    * @param coordPartida
    * @param coordChegada
    * @param taxiProximo
    * @param taxiusado
    * @param distanciaInicial
    * @param distanciaViagem
    * @param precoEstimado
    * @param precoReal
    * @param nota
    * @param data
    */ 
   public Viagem(Motorista condutor, Ponto2D coordPartida, Ponto2D coordChegada, boolean taxiProximo, Taxi taxiUsado, 
                 double distanciaInicial, double distanciaViagem, double precoEstimado, double precoReal, int nota, GregorianCalendar data) {
       this.condutor = condutor;
       this.coordPartida = coordPartida;
       this.coordChegada = coordChegada;
       this.taxiProximo = taxiProximo;
       this.taxiUsado = taxiUsado;
       this.distanciaInicial = distanciaInicial;
       this.distanciaViagem = distanciaViagem;
       this.precoEstimado = precoEstimado;
       this.precoReal = precoReal;
       this.nota = nota;
       this.data = data;
   }
   
   /**
    * Getters e Setters da classe Viagem.
    */
   public Motorista getCondutor() { 
       return this.condutor.clone();
   }
    
   public Ponto2D getCoordPartida() {
       return this.coordPartida.clone();
   }
    
   public Ponto2D getCoordChegada() {
       return this.coordChegada.clone();
   }
   
   public boolean getTaxiProximo() {
       return this.taxiProximo;
   }
    
   public Taxi getTaxiUsado() {
       return this.taxiUsado.clone();
   }
    
   public double getDistanciaIncial() {
       return this.distanciaInicial;
   }
    
   public double getDistanciaViagem() {
       return this.distanciaViagem;
   }
    
   public double getPrecoEstimado() {
       return this.precoEstimado;
   }
    
   public double getPrecoReal() {
       return this.precoReal;
   }
    
   public int getNota() {
       return this.nota;
   }
   
   public GregorianCalendar getCalendar() {
       return this.data;
   }
   
   public void setCondutor(Motorista condutor) {
       this.condutor = condutor;
   }
   
   public void setCoordPartida(Ponto2D coordPartida) {
       this.coordPartida = coordPartida;
   }
    
   public void setCoordChegada(Ponto2D coordChegada) {
       this.coordChegada = coordChegada;
   }
    
   public void setTaxiProximo(boolean taxiProximo) {
       this.taxiProximo = taxiProximo ;
   }  
    
   public void setTaxiUsado(Taxi taxiUsado) {
       this.taxiUsado = taxiUsado;
   }
    
   public void setDistanciaIncial(double distanciaIncial) {
       this.distanciaInicial = distanciaIncial;
   }
    
   public void setDistanciaViagem(double distanciaViagem) {
       this.distanciaViagem = distanciaViagem;
   }
    
   public void setprecoEstimado(double precoEstimado) {
       this.precoEstimado = precoEstimado;
   }
    
   public void setprecoReal(double precoReal) {
       this.precoReal = precoReal;
   }
    
   public void setNota(int nota) {
       this.nota = nota;
   }
   
   public void setCalendar (GregorianCalendar c) {
       this.data = c;
   }
   
   /**
    * Compara igualdade com outro objeto.
    * @param o
    * @return
    */ 
   public boolean equals(Object o) {
       if (this == o) return true;
       
       if((o == null) || (this.getClass() != o.getClass())) return false;
        
       Viagem aux = (Viagem) o;
        
       return (this.coordPartida.equals(aux.getCoordPartida()) && this.coordChegada.equals(aux.getCoordChegada())
               && this.taxiProximo == aux.getTaxiProximo() && this.taxiUsado.equals(aux.getTaxiUsado()) &&
               this.distanciaInicial == aux.getDistanciaIncial() &&  this.distanciaViagem == aux.getDistanciaViagem() && this.precoEstimado == aux.getPrecoEstimado() && 
               this.precoReal == aux.getPrecoReal() && this.nota == aux.getNota() && this.condutor.equals(aux.getCondutor()) && this.data.equals(aux.getCalendar()));
   }
   
   /** 
    * Devolve uma representação do objeto em formato textual.
    * @return
    */ 
   public String toString() {
       String aux = new String();
       aux = ("\n-> Viagem: \n" + "Coordenadas de Partida: " + this.coordPartida + "\n" + "Coordenada de Chegada: " +
              this.coordChegada + "\n" + "O taxi está proximo? " + this.taxiProximo + "\n" + "Distância Inicial percorrida: " + this.distanciaInicial + "\n" + 
              "Distância da Viagem: " + this.distanciaInicial + "\n" + "Preço da Viagem Estimado: " + this.precoEstimado + "\n" + 
              "Preço da Viagem Real: " + this.precoReal + "\n" + "Nota: " + this.nota + "\n" + "Motorista: " + this.condutor.getEmail() + "\n" + 
              "Data da Viagem: " + this.data.get(Calendar.DAY_OF_MONTH) + "/" + (this.data.get(Calendar.MONTH) + 1) + "/" + this.data.get(Calendar.YEAR));
       return aux;
   }
   
   /**
    * Retorna uma cópia da instância.
    * @return
    */ 
   public Viagem clone() {
       return new Viagem(this);
   }
}
