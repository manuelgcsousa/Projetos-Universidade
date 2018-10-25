/**
 * Classe Motos_comReservas (subclasse de Motos).
 * 
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */

import java.util.List;
import java.util.ArrayList;

public class Motos_comReservas extends Motos {
    /** Variáveis de Instância */
    private ArrayList<Cliente> reservas;
    
    /** 
     * Construtor vazio.
     */
    public Motos_comReservas() {
        super();
        this.reservas = new ArrayList<Cliente>();
    }
    
    /** 
     * Construtor por cópia.
     * @param cl
     */ 
    public Motos_comReservas(Motos_comReservas cl) {
        super(cl);
        this.reservas = cl.getReservas();
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
     * @param reservas
     */
    public Motos_comReservas(String matricula, Motorista condutor, double velMedia, double precoKm, double factorFiabilidade, 
                             double totalFaturado, Ponto2D coords, ArrayList<Viagem> viagensEfetuadas, ArrayList<Cliente> reservas) {
        super(matricula, condutor, velMedia, precoKm, factorFiabilidade, totalFaturado, coords, viagensEfetuadas);
        if (reservas == null) this.reservas = reservas;
        else {
            for (Cliente c: reservas) {
                this.reservas.add(c.clone());
            }
        }
    }
    
    /**
     * Getters e Setters da classe Motos_comReservas.
     */    
    public ArrayList<Cliente> getReservas() {
        ArrayList<Cliente> novo = new ArrayList<Cliente>();
        if (this.reservas == null) return novo;
        for(Cliente c: this.reservas) {
            novo.add(c.clone());
        }
        
        return novo;
    }
     
    public void setReservas(List<Cliente> reservas) {
        if (reservas == null) this.reservas = null;
        else {
            this.reservas.clear();
            for (Cliente c: reservas) {
                this.reservas.add(c.clone());
             }
        }
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return
     */  
    public String toString() {
        return new String("-> Motos: \n" + super.toString() + "Lista de Reservas: " + this.reservas.toString());
    } 
    
    /**
     * Compara igualdade com outro objeto.
     * @param umCarro
     * @return
     */   
    public boolean equals(Object umCarro) {
        if (this == umCarro) 
            return true;
        
        if ((umCarro == null || (this.getClass() != umCarro.getClass())))  
            return false;
                 
        Motos_comReservas cl = (Motos_comReservas) umCarro;
        return(super.equals(cl) && this.reservas.equals(cl.getReservas()));      
    } 
    
    /**
     * Retorna uma cópia da instância.
     * @return
     */ 
    public Motos_comReservas clone() {
        return new Motos_comReservas(this);
    }  
}
